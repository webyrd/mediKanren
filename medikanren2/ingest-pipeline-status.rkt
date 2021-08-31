#lang racket
(provide
    /ingest-pipeline-status)
(require
  web-server/servlet
  web-server/servlet-env
  racket/dict
  (prefix-in aws: aws)
  ;(except-in aws module) ;module: identifier already required ;except-in: identifier `module' not included in nested require spec
  aws/keys
  aws/s3
)
(define mime:text (string->bytes/utf-8 "text/plain;charset=utf-8"))

(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list body)))

(define (OK req mime-type body)
  (define headers '())
  (define bytes.body (if (string? body) (string->bytes/utf-8 body) body))
  (respond 200 "OK" headers mime-type bytes.body))

(define (respond-404)
  (respond 404 "Not found" '() mime:text (string->bytes/utf-8 "")))

(define (sanitize-input-path p)
    (define p2 (regexp-match #rx"[a-zA-Z0-9_/\\.-]+" p))
    (define p3 (if (list? p2) (car p2) #f))
    (if (and
            (string? p3)
            (or
                (string-suffix? p3 "completed.json")
                (string-suffix? p3 "install.yaml")))
        p3
        #f))

;;; endpoint /ingest-pipeline-status
;;; required parameter:
;;;   path: an S3 URL that conforms that is a status file from kg-ingest-pipeline.
(define (/ingest-pipeline-status req . args)
  (define bindings (request-bindings req))
  (aws-cli-profile)
  (define path (sanitize-input-path (dict-ref bindings 'path "")))
  (with-handlers
    ((aws:exn:fail:aws?
        (lambda (ex)
            (respond-404)
        )))
    (if path
        (let ((st (bytes->string/utf-8 (get/bytes path))))
        (OK req mime:text st))
        (respond-404)
        )))

