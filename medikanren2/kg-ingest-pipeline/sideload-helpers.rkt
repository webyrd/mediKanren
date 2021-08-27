#lang racket
(provide
  (all-defined-out))
(require aws/s3)
(require xml/path)
(require chk)
(require "metadata.rkt")
(require "main-params.rkt")
(require "s3path.rkt")
(require "pipesig.rkt")
(require "cmd-helpers.rkt")
(require "dispatch.rkt")

(define rfile-default "payload.tar.gz")
(define duration-max-default 3600)

(define (s3path-sideload)
    (format "~a/sideload" (s3path-base)))

(define (sideload-new kgid ver sha1 tyysec rfile)
  `(sideload ,kgid ,ver ,sha1 ,tyysec ,rfile))

(define (sideload-from-uri s3uri)
    (match (s3split-from-uri (s3path-sideload) s3uri)
        (`("kgid" ,kgid "v" ,ver "s" ,sha1 "t" ,tyysec . ,relfs)
            (printf "matched ~a\n" s3uri)
            `((sideload ,kgid ,ver ,sha1 ,tyysec . ,relfs)))
        (foo
            (printf "could not match ~s from s3path-tasks=~a and s3uri=~a\n" foo (s3path-sideload) s3uri)
            `())))

(define (uri-from-sideload sideload)
    (match sideload
        (`(sideload ,kgid ,ver ,sha1 ,tyysec . ,relfs)
            (format "~a/kgid/~a/v/~a/s/~a/t/~a" (s3path-sideload) kgid ver sha1 tyysec))
        (_ (error "uri-from-sideload: malformed sideload"))))

(define (fetch-sideload-events)
    (define num-max-each 1000)
    (define bucket (bucket-from-s3path (s3path-sideload)))
    (ls/proc
        (s3path-sideload)
        (lambda (sideloads xmls)
            (define sideloads-new (append-map 
                (lambda (xml)
                    (define key (se-path* '(Key) xml))
                    (define uri (format "~a/~a" bucket key))
                    (sideload-from-uri uri))
                xmls))
            (printf "parsed sideload, found ~a of ~a keys" (length sideloads-new) (length xmls))
            ;; TODO: add time filter
            (append sideloads-new sideloads))
        '() ; initial value of checks
        num-max-each))

(define (psig-from-sideload sideload)
    (match sideload
        (`(sideload ,kgid ,ver ,sha1 ,tyysec . ,relfs)
            (define version-of-dbwrapper
                (version-of-dbwrapper/validation kgid ver))
            (psig
                `#hash(("source" . "sideload") 
                        ("kgid" . ,kgid)
                        ("ver" . ,ver)
                        ("sha1" . ,sha1)
                        ("version-of-dbwrapper" . ,version-of-dbwrapper))
                `#hash(("sideload" . ,sideload))))
        (_ (error (format "psig-from-sideload: could not parse sideload: ~s" sideload)))))

(define (sideload-fetch-to-disk psig)
    (define sideload (psig-extra-ref psig "sideload"))
    (match sideload
        (`(sideload ,kgid ,ver ,sha1 ,tyysec . ,relfs)
            (define uri-remote (uri-from-sideload sideload))
            (define sig (psig-hash psig))
            (define rfile (format "~a_~a.tar.gz" kgid ver))
            (define afile-archive (build-path (adir-temp) #;(TODO insert sig) rfile))
            (make-parent-directory* afile-archive)
            (get/file uri-remote afile-archive)
            (printf "wrote file to ~a\n" afile-archive))
        (_ (error (format "sideload-fetch-to-disk: could not parse sideload: ~s" sideload)))))

(define (tbi-from-sideload-psig psig)
  (define sideload (psig-extra-ref psig "sideload"))
  (printf "sideload psig=~s\n" psig)
  (printf "sideload=~s\n" sideload)
  (match sideload
    (`(sideload ,kgid ,ver ,sha1 ,tyysec . ,relfs)
        (task-build-index
            (kge-coord kgid ver (substring tyysec 0 8))
            (floor (/ (current-milliseconds) 1000))
            duration-max-default))
    (_ (error (format "could not create tbi from sideload psig: ~s" psig)))))


(module+ test

    (define check-1 `(sideload "foo-kg" "1.7" "123abc" "20010203070809"))

    (chk
        (#:do (parameterize ((s3path-base "bucket"))
                (uri-from-sideload check-1)))
        (#:t #t))

    (chk
        (#:= (parameterize ((s3path-base "bucket"))
                (sideload-from-uri (uri-from-sideload check-1)))
            `(,check-1)))
)