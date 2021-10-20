#lang racket/base
(provide path->format file-stats s-pop-header produce/pop-header
         port-produce port-consume
         out:port out:file
         in:transform in:procedure in:port in:file
         in:stream in:pop-header
         json->scm scm->json jsexpr->scm scm->jsexpr
         jsonl:read jsonl:write json:read json:write
         tsv:read tsv:write csv:read csv:write csv:escape)
(require "codec.rkt" "enumerator.rkt" "misc.rkt" "stream.rkt"
         json racket/list racket/match racket/port racket/string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path->format path.0)
  (define path (if (path? path.0) (path->string path.0) path.0))
  (case (last (string-split path "." #:trim? #f))
    (("bscm")  'bscm)
    (("scm")   'scm)
    (("tsv")   'tsv)
    (("csv")   'csv)
    (("json")  'json)
    (("jsonl") 'jsonl)
    (else      #f)))

(define (file-stats path)
  (and (file-exists? path)
       (hash 'size          (file-size path)
             'time.modified (file-or-directory-modify-seconds path))))

(define (s-pop-header ? expected s.0)
  (define s (s-force s.0))
  (unless (pair? s)   (error "missing header:" 'expected: expected))
  (unless (? (car s)) (error "invalid header:" 'found: (car s) 'expected: expected))
  (cdr s))

(define (produce/pop-header produce header)
  (define ((produce/pop ? h)) (s-pop-header ? h (produce)))
  (match header
    (#f produce)
    (#t (produce/pop (lambda (x) #t)           #t))
    (h  (produce/pop (lambda (x) (equal? x h)) h))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producers and consumers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in:procedure  proc)           proc)
(define (in:stream     s)              (lambda () s))
(define (in:transform  produce f)      (cond (f    (lambda () (f (produce))))
                                             (else produce)))
(define (in:pop-header produce header) (produce/pop-header produce header))

(define (in:port in . pargs)
  (define kwargs (make-immutable-hash (plist->alist pargs)))
  (define (produce) (port-produce in
                                  (hash-ref kwargs 'close? #f)
                                  (hash-ref kwargs 'format)
                                  (hash-ref kwargs 'type #f)))
  (in:transform (produce/pop-header produce (hash-ref kwargs 'header #f))
                (hash-ref kwargs 'transform #f)))

(define (in:file path.0 . pargs)
  (define path   (if (path? path.0) (path->string path.0) path.0))
  (define kwargs (make-immutable-hash (plist->alist pargs)))
  (define format (hash-ref kwargs 'format (path->format path)))
  (unless format            (error "unknown format:"     path))
  (unless (file-stats path) (error "missing input file:" path))
  (define (produce) (let ((in (open-input-file path)))
                      (port-produce in (lambda () (close-input-port in))
                                    format
                                    (hash-ref kwargs 'type #f))))
  (in:transform (produce/pop-header produce (hash-ref kwargs 'header #f))
                (hash-ref kwargs 'transform #f)))

(define (out:port out . pargs) (port-consume out
                                             (plist-ref pargs 'format)
                                             (plist-ref pargs 'type #f)))

(define (out:file path.0 . pargs)
  (define path   (if (path? path.0) (path->string path.0) path.0))
  (define kwargs (make-immutable-hash (plist->alist pargs)))
  (define exists (hash-ref kwargs 'exists 'error))
  (define format (hash-ref kwargs 'format (path->format path)))
  (unless format (error "unknown format:" path))
  (let ((pargs (alist->plist (hash->list (hash-set kwargs 'format format)))))
    (call-with-output-file path
                           (lambda (out) (apply out:port out pargs))
                           #:exists exists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Port management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (port-produce in close? format type)
  (case format
    ((json) (define data (vector->list (json:read in)))
            (when close? (close?))
            data)
    (else   (define get
              (case format
                ((bscm)  (lambda () (if (eof-object? (peek-byte in)) eof (decode in type))))
                ((scm)   (lambda () (read in)))
                ((tsv)   (lambda () (tsv:read in)))
                ((csv)   (lambda () (csv:read in)))
                ((jsonl) (lambda () (jsonl:read in)))
                (else    (error "unsupported input format:" format))))
            (let loop ()
              (lambda ()
                (define datum (get))
                (cond ((eof-object? datum) (when close? (close?))
                                           '())
                      (else                (cons datum (loop)))))))))

(define (port-consume out format type)
  (case format
    ((json) (lambda (en) (json:write out (enumerator->list en))))
    (else   (let ((yield (case format
                           ((scm)   (lambda (x) (write x out) (write-char #\newline out)))
                           ((bscm)  (lambda (x) (encode      out type x)))
                           ((tsv)   (lambda (x) (tsv:write   out      x)))
                           ((csv)   (lambda (x) (csv:write   out      x)))
                           ((jsonl) (lambda (x) (jsonl:write out      x)))
                           (else    (error "unsupported output format:" format)))))
              (lambda (en) (en yield))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON and JSONL formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (json->scm s) (jsexpr->scm    (string->jsexpr s)))
(define (scm->json x) (jsexpr->string (scm->jsexpr    x)))

(define (jsexpr->scm j)
  (cond ((hash?     j) (define kvs (hash->list j))
                       (make-immutable-hash
                         (map cons
                              (map symbol->string (map car kvs))
                              (map jsexpr->scm    (map cdr kvs)))))
        ((pair?     j) (list->vector (map jsexpr->scm j)))
        ((eq? 'null j) '())
        (else          j)))

(define (scm->jsexpr x)
  (cond ((hash?   x) (define kvs (hash->list x))
                     (make-immutable-hash
                       (map cons
                            (map string->symbol (map car kvs))
                            (map scm->jsexpr (map cdr kvs)))))
        ((vector? x) (map scm->jsexpr (vector->list x)))
        ((null?   x) 'null)
        (else        x)))

(define (jsonl:read in)
  (define s (read-line in 'any))
  (if (eof-object? s) s (json->scm s)))

(define (jsonl:write out x)
  (write-string (scm->json x) out)
  (write-char #\newline out))

(define (json:read in)
  (json->scm (port->string in)))

(define (json:write out x)
  (write-string (scm->json x) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TSV format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Informal grammar for tab-separated values (no escapes supported)
;FIELD-SEPARATOR  ::= \t
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream    ::= EOF | record [RECORD-SEPARATOR] EOF | record RECORD-SEPARATOR record-stream
;record           ::= field | field FIELD-SEPARATOR record
;field            ::= CONTENT*
;CONTENT includes anything other than \t, \n, \r

(define (tsv:read in)
  (define l (read-line in 'any))
  (if (eof-object? l)
    l
    (let ((fields (string-split l "\t" #:trim? #f)))
      (if (null? fields)
        '("")
        fields))))

(define (tsv:write out x)
  (write-string (string-join x "\t" #:after-last "\n") out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSV format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Informal grammar for delimiter-separated values (escapes via double quote)
;RECORD-SEPARATOR ::= \r\n | \n | \r
;record-stream    ::= EOF | record [RECORD-SEPARATOR] EOF | record RECORD-SEPARATOR record-stream
;record           ::= field | field FIELD-SEPARATOR record
;field            ::= \" inner-content* \" | CONTENT*
;inner-content    ::= CONTENT | \"\" | FIELD-SEPARATOR | WHITESPACE
;CONTENT includes anything other than double-quote, field separator, whitespace

(define (csv:read in)
  (define (field)
    (define ch (peek-char in))
    (cond ((eqv? ch #\,)           (read-char in)
                                   "")
          ((or (eqv? ch #\newline)
               (eqv? ch #\return)
               (eof-object? ch))   "")

          ((eqv? ch #\")           (read-char in)
                                   (let loop ((i 0))
                                     (define ch (peek-char in i))
                                     (cond ((eqv? ch #\") (if (eqv? (peek-char in (+ i 1)) #\")
                                                            (loop (+ i 2))
                                                            (let ((qs (bytes->string/utf-8 (read-bytes i in))))
                                                              (read-char in)
                                                              (when (eqv? (peek-char in) #\,)
                                                                (read-char in))
                                                              (string-replace qs "\"\"" "\""))))
                                           (else          (loop (+ i 1))))))

          (else                    (let loop ((i 1))
                                     (define ch (peek-char in i))
                                     (cond ((eqv? ch #\,)           (define s (bytes->string/utf-8 (read-bytes i in)))
                                                                    (read-char in)
                                                                    s)
                                           ((or (eqv? ch #\newline)
                                                (eqv? ch #\return)
                                                (eof-object? ch))   (bytes->string/utf-8 (read-bytes i in)))
                                           (else                    (loop (+ i 1))))))))
  (if (eof-object? (peek-char in))
    eof
    (let record ()
      (cons (field)
            (let ((ch (peek-char in)))
              (cond ((eqv? ch #\return)  (read-char in)
                                         (when (eqv? (peek-char in) #\newline)
                                           (read-char in))
                                         '())
                    ((eqv? ch #\newline) (read-char in)
                                         '())
                    ((eof-object? ch)    '())
                    (else                (record))))))))

(define (csv:write out x)
  (write-string (string-join (map csv:escape x) "," #:after-last "\n") out))

(define (csv:escape s)
  (if (ormap (lambda (x) (string-contains? s x)) '("," "\"" "\n" "\r"))
    (string-append "\"" (string-replace s "\"" "\"\"") "\"")
    s))
