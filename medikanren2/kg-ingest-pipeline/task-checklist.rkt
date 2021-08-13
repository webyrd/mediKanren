#lang racket
(provide
    fetch-task-events
    tasks-resolved
    commit-task
    )
(require aws/keys)
(require aws/s3)
(require xml/path)
(require json)
(require chk)
(require "metadata.rkt")
(require "main-params.rkt")
(require "simple-time.rkt")

#|
    Module task-checklist.rkt is a sort of "half" task queue: it persists
    task completion events, but it relies on an upstream system to provide
    task request events.  In our case, KGE can implicitly provide the
    upstream events for the time being.

    In order to support a way to inject .tsv data into the pipeline without
    involving KGE, this code would have to be generalized to support
    recording task request events.
|#

(define mimetype "application/json")

(define (s3path-tasks)
    (format "~a/tasks/v1" (s3path-base)))

(define (s3split-impl s3uri)
    (string-split s3uri "/" #:trim? #f))

(define (s3split-from-uri s3uri)
    (list-tail (s3split-impl s3uri) (+ 1 (length (s3split-impl (s3path-base))))))

(define (check-from-uri s3uri)
    (match (s3split-from-uri s3uri)
        (`("kgid" ,kgid "ver" ,ver ,relf)
            #:when (string-contains? relf ".json")
            (printf "matched ~a\n" s3uri)
            (let (
                    (tyysec (substring relf 0 14))
                    (state (substring relf 15 (- (string-length relf) 5))))
                `((check ,kgid ,ver ,tyysec ,state))))
        (foo
            (printf "could not match ~s\n" foo)
            `())))

(define (uri-from-check check)
    (match check
        (`(check ,kgid ,ver ,tyysec ,state)
            `(,(format "~a/kgid/~a/ver/~a/~a-~a.json" (s3path-base) kgid ver tyysec state)))
        (_ `())))

(define (fetch-task-events)
    (define num-max-each 1000)
    (ls/proc
        (s3path-base)
        (lambda (checks xmls)
            (define checks-new (append-map 
                (lambda (xml)
                    (define key (se-path* '(Key) xml))
                    (define uri (format "~a/~a" (s3path-base) key))
                    (check-from-uri uri))
                xmls))
            (printf "parsed tasks from ~a of ~a keys" (length checks-new) (length xmls))
            ;; TODO: add time filter
            (append checks-new checks))
        '() ; initial value of checks
        num-max-each))

(define ((tasks-resolved get-id get-ver) idvers checks states-completed)
    (define h (make-hash))
    (for ((check checks))
        (match check
            (`(check ,kgid ,ver ,tyysec ,state)
                (when (member state states-completed)
                    (hash-set! h `(,kgid ,ver) #t)))))
    (filter (lambda (idver)
        (not (hash-has-key? h `(,(get-id idver) ,(get-ver idver)))))
        idvers))


(define (commit-task idver state jsexpr)
    (define tsec-completed (floor (/ (current-milliseconds) 1000)))
    (define payload (jsexpr->bytes jsexpr))
    (match idver
        (`(idver ,kgid ,ver)
            (define tyysec (format-yyyy-from-tsec (floor (/ (current-milliseconds) 1000))))
            (match (uri-from-check `(check ,kgid ,ver ,tyysec ,state))
                (`(,uri)
                    (put/bytes
                        uri
                        payload
                        mimetype))))))

(module+ test

    (define check-1 `(check "foo-kg" "1.7" "20010203070809" "completed"))

    (chk
        (#:= (parameterize ((s3path-base "bucket"))
                (length (append-map uri-from-check `(,check-1))))
            1))

    (chk
        (#:= (parameterize ((s3path-base "bucket"))
                (append-map check-from-uri (append-map uri-from-check `(,check-1))))
            `(,check-1)))
)

