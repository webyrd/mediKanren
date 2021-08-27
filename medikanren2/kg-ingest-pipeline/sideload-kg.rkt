#lang racket
(require json)
(require aws)
(require aws/keys)
(require aws/s3)
(require "current-source.rkt")
(require "main-params.rkt")
(require "cmd-helpers.rkt")
(require "sideload-helpers.rkt")
(require "simple-time.rkt")

(define rfile-to-ingest (make-parameter #f))
(define kgid (make-parameter #f))
(define ver (make-parameter #f))

(define (s3-exists s3path)
  (not (empty? (ls s3path))))

(define (run-main)
  (aws-cli-profile)
  ;(printf "dry-run is ~a\n" (dry-run))
  (define sha1 (sha1sum (rfile-to-ingest)))
  ;(printf "sha1 of ~a is ~a\n" (rfile-to-ingest) sha1)
  (define tyysec (format-yyyy-from-tsec (floor (/ (current-milliseconds) 1000))))
  (define sideload (sideload-new (kgid) (ver) sha1 tyysec rfile-default))
  (define s3path (uri-from-sideload sideload))
  (printf "s3 object path: ~a\n" s3path)
  ;(printf "s3 object exists? ~a\n" (s3-exists s3path))
  (if (s3-exists s3path)
    (printf "object already exists, skipping upload: ~a\n" s3path)
    (begin
      (multipart-put/file
        s3path
        (string->path (rfile-to-ingest)))
      (void))))

(define (with-fileconfig thunk-run)
  (define afile-config 
    (simplify-path (build-path (adir-current-source) "config.json")))
  (define st-token (file->string afile-config))
  (define jsexpr (string->jsexpr st-token))
  (printf "using s3path-prefix=~a\n" (dict-ref jsexpr 's3path-prefix))
  (parameterize ((s3path-base (dict-ref jsexpr 's3path-prefix)))
    (thunk-run)))

(define (parse-cliconfig)
  (command-line
   #:program "sideload-kg.rkt"
   #:usage-help "sideload-kg.rkt <options>"
   #:once-each
    [("--file-to-ingest") adir
                  "An upstream file to ingest"
                  (rfile-to-ingest (string-trim #:left? #f adir))]
    [("--kg-id") x
                    "Indentity of the kg"
                    (kgid x)]
    [("--kg-version") v
                    "Version of the kg"
                    (ver v)]
    [("--dry-run")
                    "Print commands to run.  Do not run."
                    (dry-run #t)]
   #:args ()
   '()))

(define params-required `(
  ("--file-to-ingest" ,rfile-to-ingest) 
  ("--kg-id" ,kgid)
  ("--kg-version" ,ver)))

(define (validate-cliconfig)
  (define param-missing (findf (lambda (a) (not ((cadr a)))) params-required))
  (cond
    (param-missing
      (error (format "Missing required argument ~a.  Pass --help for usage." (car param-missing))))
    ((not (file-exists? (rfile-to-ingest))) (error (format "File not found: ~a" (rfile-to-ingest))))))

(module+ main
  (with-fileconfig
    (lambda ()
      (parse-cliconfig)
      (validate-cliconfig)
      (parameterize ((dry-run #f))
        (run-main))
    )))
