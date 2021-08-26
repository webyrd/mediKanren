#lang racket
(provide
    fetch-task-events
    tasks-unresolved
    commit-task
    s3adir-for-psig
    state-from-check
    hsig-from-check
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
        (`("kgid" ,kgid "s" ,sig "t" ,tyysec . ,relfs)
            (printf "matched ~a\n" s3uri)
            `((check ,kgid ,sig ,tyysec . ,relfs)))
        (foo
            (printf "could not match ~s from s3path-tasks=~a and s3uri=~a\n" foo (s3path-tasks) s3uri)
            `())))

(define (uri-from-check check)
    (match check
        (`(check ,kgid ,sig ,tyysec . ,relfs)
            (format "~a/kgid/~a/s/~a/t/~a" (s3path-tasks) kgid sig tyysec))
        (_ (error "uri-from-check: malformed check"))))

(define (s3adir-for-psig psig tsec)
    ;tsec: (floor (/ (current-milliseconds) 1000))
    (define kgid (psig-main-ref psig "kgid"))
    (define hsig (psig-hash psig))
    (define tyysec (format-yyyy-from-tsec tsec))
    (uri-from-check `(check ,kgid ,hsig ,tyysec)))

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

(define (hsig-from-check check)
    (match check
        (`(check ,kgid ,sig ,tyysec . ,relfs)
            sig)
        (_ (error "uri-from-check: malformed check"))))

(define (state-from-relf relf)
    (define i (- (string-length relf) 5))
    (define s-first (substring relf 0 i))
    (define s-end (substring relf i (string-length relf)))
    (if (equal? s-end ".json")
        s-first
        #f))
(define (state-from-relfs relfs)
    (if (empty? relfs) #f (state-from-relf (car relfs))))
(define (state-from-check check)
    (match check
        (`(check ,kgid ,sig ,tyysec . ,relfs)
            (state-from-relfs relfs))
        (_ #f)))

(define (tasks-unresolved psigs-in checks-out state-get #;(from out) hsig-get #;(from out) states-completed)
    (define h (make-hash))
    (for ((check-out checks-out))
        (define state (state-get check-out))
        (when (member state states-completed)
            (hash-set! h (hsig-get check-out) #t)))
    (filter (lambda (psig-in)
        (not (hash-has-key? h (psig-hash psig-in))))
        psigs-in))

(define (commit-task psig state)
    (define payload (psig-payload (psig-extra-set psig "state" state)))
    (define tsec-completed (floor (/ (current-milliseconds) 1000)))
    (define uri (format "~a/~a.json" (s3adir-for-psig psig tsec-completed) state))
                    (put/bytes
                        uri
                        payload
                        mimetype))

(module+ test

    (define check-1 `(check "foo-kg" "1.7" "20010203070809"))

    (chk
        (#:do (parameterize ((s3path-base "bucket"))
                (uri-from-check check-1)))
        (#:t #t))

    (chk
        (#:= (parameterize ((s3path-base "bucket"))
                (check-from-uri (uri-from-check check-1)))
            `(,check-1)))

    (chk
        (#:=
            (state-from-relf "hello.json")
            "hello"))
)

