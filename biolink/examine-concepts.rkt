#lang racket

(provide

 concept-info
 alist-of-concept-lists
 alist-of-concept-sets
 concept-tag-intersections
 concept-tag-set-symmetric-difference
 )

(define get-concepts-in-db
  (lambda (db)
    (printf "reading ~s concept tags...\n" db)
    (let ((p (open-input-file (string-append "data/" db "/concepts.scm"))))
      (let loop ((concept (read p))
                 (concept-sources '())
                 (concept-count-ht (hash)))
        (cond
          [(eof-object? concept)
           (close-input-port p)
           (printf "read ~s ~s concept tags\n" (length concept-sources) db)
           (list (sort concept-sources string<?)
                 concept-count-ht)]
          [else (let* ((concept-tag (vector-ref concept 0))
                       (tag-parts (regexp-split #rx":" concept-tag))
                       (tag-source (car tag-parts))
                       (concept-sources (if (member tag-source concept-sources)
                                            concept-sources
                                            (cons tag-source concept-sources)))
                       (concept-count-ht (hash-set concept-count-ht
                                                   tag-source
                                                   (add1 (hash-ref concept-count-ht tag-source 0)))))
                  (loop (read p)                 
                        concept-sources
                        concept-count-ht))])))))


(define concept-info
  (map (lambda (db)
         (let ((c (get-concepts-in-db db)))
           (cons db c)))
       '("monarch-lite" "rtx" "scigraph" "semmed")))

(define alist-of-concept-lists
  (map (lambda (info)
         (cons (car info) (cadr info)))
       concept-info))

(define alist-of-concept-sets
  (map (lambda (e)
         (cons (car e) (apply set (cdr e))))
       alist-of-concept-lists))

;; calculate intersections (overlaps) between data sources
(define concept-tag-intersections
  (let loop ((alist-of-concept-sets alist-of-concept-sets)
             (intersections '()))
    (match alist-of-concept-sets
      ['() intersections]
      [`((,db1 . ,concept-set1) . ,rest)
       (loop (cdr alist-of-concept-sets)
             (append (map (lambda (e)
                            (match e
                              [`(,db2 . ,concept-set2)
                               (list (list db1 db2)
                                     (sort (set->list (set-intersect concept-set1 concept-set2)) string<?))]))
                          rest)
                     intersections))])))

;; calculate differences between data sources
(define concept-tag-set-symmetric-difference 
  (let loop ((alist-of-concept-sets alist-of-concept-sets)
             (symmetric-differences '()))
    (match alist-of-concept-sets
      ['() symmetric-differences]
      [`((,db1 . ,concept-set1) . ,rest)
       (loop (cdr alist-of-concept-sets)
             (append (map (lambda (e)
                            (match e
                              [`(,db2 . ,concept-set2)
                               (list (list db1 db2)
                                     (set-symmetric-difference concept-set1 concept-set2))]))
                          rest)
                     symmetric-differences))])))
