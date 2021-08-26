#lang racket
(provide
    fetch-task-events
    tasks-unresolved
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
(require "s3path.rkt")
(require "pipesig.rkt")

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
    (format "~a/tasks" (s3path-base)))

(define (check-from-uri s3uri)
    (match (s3split-from-uri (s3path-tasks) s3uri)
        (`("kgid" ,kgid "s" ,sig "t" ,tyysec ,relf)
            #:when (string-contains? relf ".json")
            (printf "matched ~a\n" s3uri)
            (let (
                    (state (substring relf 0 (- (string-length relf) 5))))
                `((check ,kgid ,sig ,tyysec ,state))))
        (foo
            (printf "could not match ~s from s3path-tasks=~a and s3uri=~a\n" foo (s3path-tasks) s3uri)
            `())))

(define (uri-from-check check)
    (match check
        (`(check ,kgid ,sig ,tyysec ,state)
            `(,(format "~a/kgid/~a/s/~a/t/~a/~a.json" (s3path-tasks) kgid sig tyysec state)))
        (_ `())))

(define (fetch-task-events)
    (define num-max-each 1000)
    (define bucket (bucket-from-s3path (s3path-tasks)))
    (ls/proc
        (s3path-tasks)
        (lambda (checks xmls)
            (define checks-new (append-map 
                (lambda (xml)
                    (define key (se-path* '(Key) xml))
                    (define uri (format "~a/~a" bucket key))
                    (check-from-uri uri))
                xmls))
            (printf "parsed tasks from ~a of ~a keys" (length checks-new) (length xmls))
            ;; TODO: add time filter
            (append checks-new checks))
        '() ; initial value of checks
        num-max-each))

(define ((tasks-unresolved get-id get-ver) psigs checks states-completed)
    (define h (make-hash))
    (for ((check checks))
        (match check
            (`(check ,kgid ,sig ,tyysec ,state)
                (when (member state states-completed)
                    (hash-set! h `(,kgid ,sig) #t)))))
    (filter (lambda (psig)
        (define kgid (psig-main-ref psig "kgid"))
        (not (hash-has-key? h `(,kgid ,(psig-hash psig)))))
        psigs))


(define (commit-task psig state jsexpr)
    (define tsec-completed (floor (/ (current-milliseconds) 1000)))
    (define payload (jsexpr->bytes jsexpr))
    (define kgid (psig-main-ref psig "kgid"))
    (define hsig (psig-hash psig))
            (define tyysec (format-yyyy-from-tsec (floor (/ (current-milliseconds) 1000))))
            (match (uri-from-check `(check ,kgid ,hsig ,tyysec ,state))
                (`(,uri)
                    (put/bytes
                        uri
                        payload
                        mimetype))))

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

