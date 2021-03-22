#lang racket/base
(provide current-config config.default file->config current-config-ref
         current-config-set current-config-set/alist
         current-config-set! current-config-set!/alist
         current-config-relation-path config-ref config-set config-set/alist
         policy-allow? logf logf/date)
(require racket/date racket/string)

(define config.default
  (make-immutable-hash
    '((relation-root-path         . #f) ;; root path for materialized relations
      (temporary-root-path        . #f) ;; root path for temporary caches
      (buffer-size                . 100000)
      (progress-logging-threshold . 100000) ;; number of rows per log message
      (update-policy              . interactive) ;; rebuild stale tables
      (cleanup-policy             . interactive) ;; remove unspecified indexes
      (migrate-policy             . interactive) ;; migrate to new data format
      (search-strategy            . #f))))

(define (valid-config?! cfg)
  (define (valid-policy? policy) (member policy '(always never interactive)))
  (define (valid-path?   p)      (or (not p) (string? p) (path? p)))
  (define-syntax validate!
    (syntax-rules ()
      ((_ (test ... key) ...)
       (begin (unless (test ... (hash-ref cfg 'key))
                (error "invalid config:" 'key (hash-ref cfg 'key))) ...))))
  (validate! (valid-path?   relation-root-path)
             (valid-path?   temporary-root-path)
             (< 0           buffer-size)
             (< 0           progress-logging-threshold)
             (valid-policy? update-policy)
             (valid-policy? cleanup-policy)
             (valid-policy? migrate-policy))
  cfg)

(define (config-ref cfg key)       (hash-ref cfg key))
(define (config-set cfg key value) (valid-config?! (hash-set cfg key value)))
(define (config-set/alist cfg kvs)
  (valid-config?! (foldl (lambda (kv cfg) (hash-set cfg (car kv) (cdr kv)))
                         cfg kvs)))
(define (file->config path)
  (config-set/alist config.default (with-input-from-file path read)))

(define current-config (make-parameter config.default))
(define (current-config-ref key)       (config-ref (current-config) key))
(define (current-config-set key value) (config-set (current-config) key value))
(define (current-config-set/alist kvs) (config-set/alist (current-config) kvs))
(define (current-config-set! key value)
  (current-config (current-config-set key value)))
(define (current-config-set!/alist kvs)
  (current-config (current-config-set/alist kvs)))
(define (current-config-relation-path path)
  (cond ((and (string? path) (string-prefix? path "/")) path)
        (else (define relation-root-path
                (current-config-ref 'relation-root-path))
              (if relation-root-path
                (path->string (build-path relation-root-path path))
                path))))

(define (policy-allow? policy describe prompt-message prompt-args)
  (case policy
    ((interactive)
     (describe)
     (apply printf (string-append prompt-message " [y/n]: ") prompt-args)
     (case (read)
       ((y Y yes Yes YES) #t)
       (else              #f)))
    ((always) #t)
    (else     #f)))

(define (pad2 n) (let ((s (number->string n)))
                   (if (<= 2 (string-length s)) s
                     (string-append "0" s))))

(define (logf/date message . args)
  (define msg (string-append "[~a/~a/~a - ~a:~a:~a] " message))
  (define d (current-date))
  (define stamp (list (date-month d) (date-day d)
                      (date-hour d) (date-minute d) (date-second d)))
  (apply eprintf msg (date-year d) (append (map pad2 stamp) args)))

(define (logf message . args)
  (define msg (string-append "[~a:~a:~a] " message))
  (define d (current-date))
  (define stamp (list (date-hour d) (date-minute d) (date-second d)))
  (apply eprintf msg (append (map pad2 stamp) args)))

;; TODO: job system
;; single worker thread
;; jobs w/ independent loggers (port, file, in-memory, or null)

;(define (thread/wait proc)
;  (define t (thread proc))
;  (plumber-add-flush! (current-plumber)
;                      (lambda (h) (thread-wait t)))
;  t)
