#lang racket/base

(provide semantic-exclude* domain-exclude* range-exclude*)

(require racket/runtime-path
         yaml
         racket/match
         racket/list
         "neo-biolink-reasoning-low-level.rkt")

(define SEMMED_EXCLUDE_LIST_FILE "../neo-biolink/biolink_3_5_2/semmed-exclude-list.yaml")

(define-runtime-path path.here ".")
(define semmed-exclude-path (build-path path.here SEMMED_EXCLUDE_LIST_FILE))

(printf "loading semmed exclude list YAML file from '~s'...\n" semmed-exclude-path)

(define ip (open-input-file semmed-exclude-path))
(define exclude-orig (time (read-yaml ip)))
(define exclude-recd* (hash-ref exclude-orig "excluded_semmedb_records"))

(match-define-values (semantic-exclude* domain-exclude* range-exclude*)
  (let loop ((h exclude-recd*) (s '()) (d '()) (r '()))
    (if (null? h)
        (values (remove-duplicates s)
                (remove-duplicates d)
                (remove-duplicates r))
        (let* ((hs (car h))
               (type (hash-ref hs "exclusion_type"))
               (sub-t (hash-ref hs "semmed_subject_t_code"))
               (pred (hash-ref hs "semmed_predicate"))
               (obj-t (hash-ref hs "semmed_object_t_code")))
          (cond
            ((equal? type "semantic type exclusion")
             (let ((sub-b (UMLS-biolink-class-mapper sub-t))
                   (obj-b (UMLS-biolink-class-mapper obj-t)))
               (if (or sub-b obj-b)
                   (loop (cdr h) (cons (list sub-b #f obj-b) s) d r)
                   (loop (cdr h) s d r))))
            ((equal? type "Domain exclusion")
             (let ((sub-b (UMLS-biolink-class-mapper sub-t))
                   (pred-b (UMLS-biolink-pred-mapper pred)))
               (if (and sub-b pred-b)
                   (loop (cdr h) s (cons (list sub-b pred-b #f) d) r)
                   (loop (cdr h) s d r))))
            ((equal? type "Range exclusion")
             (let ((obj-b (UMLS-biolink-class-mapper obj-t))
                   (pred-b (UMLS-biolink-pred-mapper pred)))
               (if (and pred-b obj-b)
                   (loop (cdr h) s d (cons (list #f pred-b obj-b) r))
                   (loop (cdr h) s d r)))))))))
           
 


