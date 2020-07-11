#lang racket/base
(provide
  (all-from-out "dbk/dbk.rkt")
  path-simple path/data path/root path.data path.root

  config config-ref load-config

  read/file read/string

  validate-header
  materialize-relation

  appendo membero
  )
(require
  "dbk/dbk.rkt"
  racket/list racket/port racket/runtime-path racket/string)

(define buffer-size 100000)  ;; main memory used for external sorting

(define-runtime-path path.root ".")
(define (path-simple path) (path->string (simplify-path path)))
(define (path/root relative-path)
  (path-simple (build-path path.root relative-path)))
(define path.data (path/root "data"))
(define (path/data relative-path)
  (path-simple (build-path path.data relative-path)))

(define (read/file path)  (with-input-from-file  path read))
(define (read/string str) (with-input-from-string str read))

(define box.config (box #f))
(define (config)
  (define cfg (unbox box.config))
  (cond (cfg cfg)
        (else (load-config #t #f)
              (unbox box.config))))
(define (config-ref key)
  (define kv (assoc key (config)))
  (unless kv (error "missing configuration key:" key))
  (cdr kv))
(define (load-config verbose? path.config)
  (define path.config.user     (or path.config (path/root "config.scm")))
  (define path.config.defaults (path/root "config.defaults.scm"))
  (when verbose? (printf "loading configuration defaults: ~a\n"
                         (path-simple path.config.defaults)))
  (define config.defaults (read/file path.config.defaults))
  (when verbose? (if (file-exists? path.config.user)
                   (printf "loading configuration overrides: ~a\n"
                           (path-simple path.config.user))
                   (printf "configuration overrides not present: ~a\n"
                           (path-simple path.config.user))))
  (define config.user (if (file-exists? path.config.user)
                        (read/file path.config.user)
                        '()))
  (unless (and (list? config.user) (andmap pair? config.user))
    (error "invalid configuration overrides:" config.user))
  (define user-keys (map car config.user))
  (define (user-defined? kv) (member (car kv) user-keys))
  (set-box! box.config
            (append config.user (filter-not user-defined? config.defaults))))

(define (validate-header in header-expected delimiter)
  (define header-found (read-line in 'any))
  (when (not (equal? header-found (string-join header-expected delimiter)))
    (error "unexpected header:" header-found header-expected)))

(define (materialize-stream stream mat-args)
  (let ((mat (materializer mat-args)))
    (define count 0)
    (time (s-each (lambda (x)
                    (when (= 0 (remainder count 100000))
                      (printf "Ingested ~s rows\n" count))
                    (mat 'put x)
                    (set! count (+ count 1)))
                  stream))
    (printf "Processing ~s rows\n" count)
    (time (mat 'close))
    (printf "Finished processing ~s rows\n" count)))

(define (materialize-relation path fnin header delimiter port->stream transform
                              fields types indexes)
  (unless (directory-exists? (path/data path))
    (printf "materializing relation ~s from ~s\n"
            (path/data path) (path/data fnin))
    (let/files ((in (path/data fnin))) ()
      (when header (validate-header in header delimiter))
      (define stream (port->stream in))
      (materialize-stream
        (if transform (s-map transform stream) stream)
        `((buffer-size     . ,buffer-size)
          (path            . ,(path/data path))
          (attribute-names . ,fields)
          (attribute-types . ,types)
          (tables  ((columns . ,fields)))
          (indexes . ,(map (lambda (i) (list (cons 'columns i))) indexes)))))))

(define-relation (appendo xs ys xsys)
  (conde ((== '() xs) (== xsys ys))
         ((fresh (a d dys)
            (== `(,a . ,d)  xs)
            (== `(,a . ,dys) xsys)
            (appendo d ys dys)))))

(define-relation (membero x xs)
  (fresh (y rest)
    (== `(,y . ,rest) xs)
    (conde
      ((== x y))
      (;(=/= x y)  ;; TODO: uncomment once we have =/=
       (membero x rest)))))
