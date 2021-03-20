#lang racket/base
(provide record)
(require (for-syntax racket/base) racket/match)

(begin-for-syntax
  (define (syntax/context context stx)
    (datum->syntax context (syntax->datum stx)))
  (define (id/suffix id str)
    (define str.id (symbol->string (syntax->datum id)))
    (datum->syntax id (string->symbol (string-append str.id str))))
  (define (syntax-permute keys stx.alist blame)
    (define kvs
      (syntax-case stx.alist ()
        (((key . value) ...) (map cons
                                  (syntax->datum #'(key ...))
                                  (syntax->list #'((key . value) ...))))))
    (map (lambda (k) (let ((kv (assoc k kvs)))
                       (if kv (cdr kv)
                         (error "missing record field:" k blame))))
         keys))
  (define-syntax with-syntax*
    (syntax-rules ()
      ((_ ()                  body ...) (with-syntax () body ...))
      ((_ ((p0 e0) (p e) ...) body ...) (with-syntax ((p0 e0))
                                          (with-syntax* ((p e) ...)
                                            body ...))))))

;; (record name (field ...) extra ...)
;; produces these definitions:
;; * struct:         (struct name:struct (field ...) extra ...)
;; * predicate:      name?
;; * constructor:    (name (field expr) ...)
;; * setter:         (name:set expr (field expr) ...)
;; * match-expander: (name:match (field pattern) ...)
(define-syntax (record stx)
  (syntax-case stx ()
    ((_ name (field-name ...) extras ...)
     (let* ((field-names     (syntax->datum #'(field-name ...)))
            (field-name-strs (map symbol->string field-names)))
       (with-syntax* ((id.struct  (id/suffix #'name ":struct"))
                      (id.ref     (id/suffix #'name ":ref"))
                      (id.set     (id/suffix #'name ":set"))
                      (id.?       (id/suffix #'name "?"))
                      (id.struct? (id/suffix #'name ":struct?"))
                      (id.match   (id/suffix #'name ":match"))
                      (ids.field-names #'(field-name ...))
                      ((id.accessor ...)
                       (map (lambda (f) (id/suffix #'name
                                                   (string-append "-" f)))
                            field-name-strs))
                      ((id.struct-accessor ...)
                       (map (lambda (f) (id/suffix #'id.struct
                                                   (string-append "-" f)))
                            field-name-strs)))
         #`(begin
             (struct id.struct (field-name ...) extras ...)
             (define id.?        id.struct?)
             (define id.accessor id.struct-accessor) ...
             (...
               (begin
                 (define-syntax (name stx)
                   (syntax-case stx ()
                     ((_ (f e) ...)
                      (with-syntax ((((f e) ...) (syntax-permute
                                                   'ids.field-names
                                                   #'((f e) ...)
                                                   stx)))
                        #'(let ((f e) ...) (id.struct f ...))))))
                 (define-syntax (id.set stx)
                   (syntax-case stx ()
                     ((_ r (f e) ...)
                      (let ((field-names '#,field-names))
                        #`(let ()
                            (match-define (id.struct #,@field-names) r)
                            (let #,(map list
                                        (syntax->datum #'(f ...))
                                        (syntax->list  #'(e ...)))
                              (id.struct #,@field-names)))))))
                 (define-match-expander id.match
                   (lambda (stx)
                     (syntax-case stx ()
                       ((_ (f p) ...)
                        #'(struct* id.struct ((f p) ...))))))))))))))
