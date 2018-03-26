#lang racket
(provide
  pharos-metao
  pharoso
  )

(require
  "db.rkt"
  "mk.rkt"
  )

(define db (db-info "data/db"))

(define (pharos-metao table-name metadata)
  (project (table-name)
    (if (var? table-name)
      (let loop ((kv (hash->list db)))
        (if (null? kv) (== #f #t)
          (conde
            ((== (caar kv) table-name) (pharos-metao (caar kv) metadata))
            ((loop (cdr kv))))))
      (== (table-info->metadata (hash-ref db table-name)) metadata))))

(define (pharoso table-name tuple)
  (project (table-name tuple)
    (cond ((var? table-name)
           (let loop ((kv (hash->list db)))
             (if (null? kv) (== #f #t)
               (conde
                 ((== (caar kv) table-name) (pharoso (caar kv) tuple))
                 ((loop (cdr kv)))))))
          (else (define ti (hash-ref db table-name))
                (define tt (list-truncate tuple))
                (define ds ((table-info->detail-stream ti)
                            (tuple->mask tt 0) (list->vector tt)))
                (let loop ((ds ds))
                  (cond ((stream-empty? ds) (== #f #t))
                        (else (conde
                                ((== (vector->list (stream-first ds)) tuple))
                                ((loop (stream-rest ds)))))))))))

(define (list-truncate xs)
  (cond ((var? xs) '())
        (else (cons (car xs) (list-truncate (cdr xs))))))

(define (tuple->mask tuple i)
  (cond ((or (var? tuple) (null? tuple)) '())
        ((var? (car tuple)) (tuple->mask (cdr tuple) (+ i 1)))
        (else (cons i (tuple->mask (cdr tuple) (+ i 1))))))
