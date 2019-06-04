#lang racket/base
(require
  "common.rkt"
  racket/match
  racket/port
  racket/pretty
  web-server/servlet
  web-server/servlet-env
  web-server/managers/none
  net/url
  xml)

(print-as-expression #f)
(pretty-print-abbreviate-read-macros #f)

(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))

(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error "optional arguments ~s; given ~s" argv-optional argv))

;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
(load-databases #t)

;; TODO:
;;; Query save file settings
(define WRITE_QUERY_RESULTS_TO_FILE            (config-ref 'query-results.write-to-file?))
(define QUERY_RESULTS_FILE_NAME                (config-ref 'query-results.file-name))
(define HUMAN_FRIENDLY_QUERY_RESULTS_FILE_NAME (config-ref 'query-results.file-name-human))
(define SPREADSHEET_FRIENDLY_QUERY_RESULTS_FILE_NAME (config-ref 'query-results.file-name-spreadsheet))
(define QUERY_RESULTS_FILE_MODE                (config-ref 'query-results.file-mode))
;;; Decreases/increases predicate names
(define DECREASES_PREDICATE_NAMES (config-ref 'decreases-predicate-names))
(define INCREASES_PREDICATE_NAMES (config-ref 'increases-predicate-names))

(define (xexpr->html-string xe)
  (string-append "<!doctype html>" (xexpr->string xe)))
(define mime:text/plain  (string->bytes/utf-8 "text/plain;charset=utf-8"))
(define mime:text/s-expr (string->bytes/utf-8 "text/plain;charset=utf-8"))
(define mime:text/html   (string->bytes/utf-8 "text/html;charset=utf-8"))
(define mime:text/css    (string->bytes/utf-8 "text/css;charset=utf-8"))
(define mime:text/js     (string->bytes/utf-8 "text/javascript;charset=utf-8"))
(define mime:json        (string->bytes/utf-8 "application/json;charset=utf-8"))
(define mime:binary      (string->bytes/utf-8 "application/octet-stream"))
(define (respond code message headers mime-type body)
  (response/full code (string->bytes/utf-8 message)
                 (current-seconds) mime-type headers
                 (list (string->bytes/utf-8 body))))

(define (read*/string s)
  (define (read*)
    (define datum (read))
    (if (eof-object? datum) '() (cons datum (read*))))
  (with-handlers (((lambda _ #t)
                   (lambda _ (printf "unreadable input string: ~s\n" s)
                     #f)))
                 (with-input-from-string s read*)))

(define (/query req)
  (define post-data (request-post-data/raw req))
  (define (e400/body body)
    (respond 400 "Bad Request" '() mime:text/plain body))
  (define (e400 reason)
    (e400/body (with-output-to-string
                 (lambda () (printf "~a:\n~s\n" reason
                                    (bytes->string/utf-8 post-data))))))
  (define (e400/failure failure)
    (e400/body (with-output-to-string
                 (lambda ()
                   (printf "Query failed:\n~s\n"
                           (bytes->string/utf-8 post-data))
                   (pretty-print failure)))))
  (define (ok200 data)
    (respond 200 "OK" '() mime:text/s-expr
             (with-output-to-string (lambda () (pretty-print data)))))
  (cond ((not post-data) (e400/body "Invalid POST data."))
        ((read*/string (bytes->string/utf-8 post-data))
         => (lambda (data)
              (match data
                ('()           (e400 "Empty POST"))
                ((list* x y z) (e400 "Too many POST s-expressions"))
                ((list datum)
                 (match datum
                   (`(concept ,subject? ,object? ,isa-count ,via-cui? ,strings)
                     (with-handlers
                       (((lambda _ #t) e400/failure))
                       (ok200 (find-predicates/concepts
                                subject? object?
                                (find-concepts subject? object?
                                               isa-count via-cui? strings)))))
                   (`(X ,subject ,object)
                     (with-handlers
                       (((lambda _ #t) e400/failure))
                       (ok200 (find-Xs subject object))))
                   (_ (e400 "Invalid query")))))))
        (else (e400 "Bad POST s-expression format"))))

;; TODO: show config options, starting with config.defaults.

(define index.js
  "")
(define index.html
  `(html (head (title "mediKanren Web UI"))
         ;; TODO: Support web form POST for manual testing.
         (body (p "POST to /query"))))
(define (index req)
  (respond 200 "ok" '() mime:text/html (xexpr->html-string index.html)))

(define (method-not-allowed req)
  (respond 405 "Method Not Allowed" '() mime:text/html
           (xexpr->html-string
             `(html (head (title "Method Not Allowed"))
                    (body (h1 "Method Not Allowed")
                          (pre ,(url->string (request-uri req)))
                          (p "does not support")
                          (pre ,(bytes->string/utf-8 (request-method req))))))))

(define (not-found req)
  (respond 404 "Not Found" '() mime:text/html
           (xexpr->html-string
             `(html (head (title "Not Found"))
                    (body (h1 "What are you looking for?")
                          (p "There was nothing found at")
                          (pre ,(url->string (request-uri req))))))))

(define-values (dispatcher _)
  (dispatch-rules
    (("")      #:method "get"         index)
    (("query") #:method "post"        /query)
    (("query") #:method (regexp ".*") method-not-allowed)
    (else                             not-found)))

(serve/servlet dispatcher
               ;; The none-manager offers better performance when not using
               ;; web continuations.
               #:manager (create-none-manager #f)
               #:servlet-regexp #rx""
               #:launch-browser? #f)
