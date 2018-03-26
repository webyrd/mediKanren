#lang racket
(provide
  db-info
  table-info->metadata
  table-info->detail-stream
  )

(require
  "read.rkt"
  "repr.rkt"
  )

(define (db-info dir)
  (define tables-dir (build-path dir "tables"))
  (define table-names
    (map path->string (directory-list tables-dir #:build? #f)))
  (define table-dirs (directory-list tables-dir #:build? #t))
  (make-immutable-hash
    (map (lambda (name dir) (cons name (table-info dir)))
         table-names table-dirs)))

(define (read-one-from-file path)
  (define data (read-all-from-file path))
  (if (= 1 (length data))
    (car data)
    (error "found more data than expected:" path (length data))))

(define (table-info->metadata tinfo) (vector-ref tinfo 0))
(define (table-info->detail-stream tinfo) (vector-ref tinfo 1))
(define (table-info dir)
  ;; columns: [(name, type)]
  ;; indexed keys: [col-names]
  ;; foreign keys: [(local-col-names, table-name, foreign-col-names)]
  (define cols (read-one-from-file (build-path dir "columns.scm")))
  (define ixs (read-one-from-file (build-path dir "indexes.scm")))
  (define fks (read-one-from-file (build-path dir "foreign-keys.scm")))
  (define not-indexed? (equal? ixs '(#f)))
  (define masks (map (lambda (ix) (index->mask cols (or ix '()))) ixs))
  (define t->ks (map (lambda (m) (mask->tuple->key m)) masks))
  (define c2s (map (lambda (m) (mask->compare2 cols m)) masks))

  (define (detail-stream tmask tuple)
    (define i (choose-index masks tmask 0))
    (define key (and i ((list-ref t->ks i) tuple)))
    (define compare (if i ((list-ref c2s i) key) (lambda (_) 0)))
    (define ipath (build-path dir (number->string (or i 0))))
    (define detail-path (build-path ipath "detail.scm"))
    (define offset-path (build-path ipath "offset.bin"))
    (define (stream-from pos)
      (call-with-input-file
        detail-path
        (lambda (detail-in)
          (file-position detail-in pos)
          (define detail (read detail-in))
          (define next-pos (file-position detail-in))
          (if (or (eof-object? detail)
                  (not (or not-indexed? (= 0 (compare detail)))))
            '() (stream-cons detail (stream-from next-pos))))))

    (call-with-input-file
      detail-path
      (lambda (detail-in)
        (call-with-input-file
          offset-path
          (lambda (offset-in)
            (define detail (if (or not-indexed? (not i))
                             (read detail-in)
                             (detail-find detail-in offset-in compare)))
            (define pos (file-position detail-in))
            (if (not (eof-object? detail))
              (stream-cons detail (stream-from pos))
              '()))))))

  `#(((columns . ,cols) (indexes . ,ixs) (foreign-keys ,fks))
     ,detail-stream
     ;; TODO: resolve foreign keys.
     ;; detail-stream->elaborated-detail-stream
     ))

(define (compare2-number key candidate)
  (cond ((< candidate key) -1)
        ((> candidate key) 1)
        (else 0)))
(define (compare2-text key candidate)
  (cond ((string<? candidate key) -1)
        ((string>? candidate key) 1)
        (else 0)))

(define (index->mask cols icols)
  (define ncols (map (lambda (n col) (cons (car col) n))
                     (range (length cols)) cols))
  (sort (map (lambda (icol) (cdr (assoc icol ncols))) icols) <))

(define (mask->tuple->key ns)
  (lambda (tuple) (list->vector (map (lambda (n) (vector-ref tuple n)) ns))))

(define (mask->compare2 cols ns)
  (define compare2s
    (map (lambda (n) (case (cadr (list-ref cols n))
                       (("integer" "decimal") compare2-number)
                       (else compare2-text))) ns))
  (lambda (key)
    (lambda (candidate)
      (let loop ((cs compare2s) (ns ns) (ki 0) (k1 key) (k2 candidate))
        (cond
          ((null? cs) 0)
          (else (define n (car ns))
                (define cmp ((car cs) (vector-ref k1 ki) (vector-ref k2 n)))
                (if (= 0 cmp) (loop (cdr cs) (cdr ns) (+ 1 ki) k1 k2)
                  cmp)))))))

(define (masked? mask tmask)
  (cond ((null? mask) #t)
        ((null? tmask) #f)
        (else (define m0 (car mask))
              (define mrest (cdr mask))
              (define t0 (car tmask))
              (define trest (cdr tmask))
              (cond ((< m0 t0) #f)
                    ((> m0 t0) (masked? mask trest))
                    (else (masked? mrest trest))))))

(define (choose-index masks tmask i)
  (cond ((null? masks) #f)
        ((masked? (car masks) tmask) i)
        (else (choose-index (cdr masks) tmask (+ i 1)))))
