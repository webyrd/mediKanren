#lang racket
(provide (all-defined-out))
(require "query.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;USEFUL FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; useful-drug-curie filter:
(define (useful-drug-curie? curie)
  (or (string-prefix? curie "CHEMBL:")
      (string-prefix? curie "CHEBI:")
      (string-prefix? curie "TTD:")
      (string-prefix? curie "GTPI:")))

;; sort-hash function to sort hash tables by the values and return an association list
(define (sort-hash h)
  (sort
   (hash-map h (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (length (cdr x)))))


;; This is a helper function for the find-drugs-for-genes function below:
(define gene-to-disease-preds (find-predicates '("associated_with_disease"
                                                 "contributes_to"
                                                 "biomarker_for"
                                                 "gene_associated_with_condition"
                                                 "affects"
                                                 "causes"
                                                 "prevents"
                                                 "treats"
                                                 "related_to"
                                                 "predisposes"
                                                 "gene_associated_with_condition"
                                                 "associated_with"
                                                 "complicates"
                                                 "predisposes"
                                                 "prevents"
                                                 "treats"
                                                 "disrupts"
                                                 "augments")))


;; The find-drugs-for-genes function takes a gene-list and a list of drug-gene predicates (direction that we
;; want the drugs to influence on the genes, one gene at a time) and return a list of two hash-tables: a sorted drug=>gene table and
;; a sorted gene=>drug table, which is sorted by the number of genes that have drugs available to modify them or
;; the number of drugs that can are available to target multiple genes:

(define (find-drugs-for-genes gene-list drug-gene-preds)
  (let ((gene=>drug (make-hash))
        (drug=>gene (make-hash)))
    (for-each
     (lambda (g)
       (define q (time (query/graph
                        ((D useful-drug-curie?)
                         (G g))
                        ((D->G drug-gene-preds))
                       (D D->G G))))
       (hash-set! gene=>drug (assoc g (curie-synonyms/names g))
                  (map (lambda (d) (assoc d (curie-synonyms/names d))) (curies/query q 'D)))
       (for-each
        (lambda (d)
          (hash-update! drug=>gene (assoc d (curie-synonyms/names d))
                        (lambda(v) (set-add v (assoc g (curie-synonyms/names g)))) '()))
        (filter (lambda(x) (not (string-prefix? x "CUI:"))) (curies/query q 'D))))
     gene-list)
  (list (sort-hash drug=>gene) (sort-hash gene=>drug))))

;;; examples of how to extract these two hash-tables result from find-drugs-for-genes functions:

;(define results-CHAMP1-phenotype-RNA-down (find-drugs-for-genes CHAMP1-phenotype-RNA-down positively-regulates))
;;extract drug=>gene hash-table:
;(define drug=>gene-CHAMP1-phenotype-RNA-down (car results-CHAMP1-phenotype-RNA-down))
;;extract gene=>drug hash-table:
;(define gene=>drug-CHAMP1-phenotype-RNA-down (cadr results-CHAMP1-phenotype-RNA-down))

;;; get-num-values takes a hash-table and count how many values are for each key:
(define (get-num-values hash-table)
  (map (lambda (x) (cons (car x) (length (cdr x)))) hash-table))


(define drug-treat-disease-preds
  '(;; common
     "prevents"
     "negatively_regulates"
    ;; semmed
    ;; robokop
     "negatively_regulates__entity_to_entity"
     "disrupts"
     ;; orange
     ;; rtx2
     "inhibits"
     "inhibitor"
     "channel_blocker"
     "treats"
     "disrupts"
     "may_inhibit_effect_of"
     "may_prevent"
     "may_treat"))


;; find-drug-for-condition takes a disease condition, a gene list (which are derived from RNA-seq DEG results),
;; a list of drug-to-gene predicates that we want the drug to reverse.
;; This function will generate a query/graph that does a 2-edge query which find drugs that modify a disease condition
;; while also modulate the gene in the gene list
;; this function will return a list which contains 2 hash-tables which are sorted based on the number
;; of genes or drugs available:

(define (find-drug-for-condition disease-condition gene-list drug-gene-preds)
  (let ((gene=>drug (make-hash))
        (drug=>gene (make-hash)))
    (for-each
     (lambda (g)
       (define q (time (query/graph
                        ((D useful-drug-curie?)
                         (C disease-condition)
                         (G g))
                        ((D->C drug-treat-disease-preds)
                         (D->G drug-gene-preds))
                        (D D->C C)
                        (D D->G G))))
       (hash-set! gene=>drug (assoc g (curie-synonyms/names g))
                  (map (lambda (d) (assoc d (curie-synonyms/names d))) (curies/query q 'D)))
       (for-each
        (lambda (d)
          (hash-update! drug=>gene (assoc d (curie-synonyms/names d))
                        (lambda (v) (set-add v (assoc g (curie-synonyms/names g)))) '()))
        (filter (lambda (x) (not (string-prefix? x "CUI:"))) (curies/query q 'D))))
     gene-list)
    (list (sort-hash drug=>gene) (sort-hash gene=>drug))))


;; combine-keys returns a list of keys from the input as a list of association list 

(define combine-keys
  (lambda (ls-of-alist)
    (cond
      [(null? ls-of-alist) '()]
      [else
       (let ((keys (map car (car ls-of-alist))))
         (set-union keys (combine-keys (cdr ls-of-alist))))])))


;; merge-2-assoc-list takes 2 association lists and return
;; a merged and sorted association list with unique keys and values
(define (merge-2-assoc-list alist1 alist2)
  (let ((merged (make-hash))
        (keys (set-union (map car alist1) (map car alist2))))
    (for-each
     (lambda (k)
       (let ((genes-pr-1 (assoc k alist1))
             (genes-pr-2 (assoc k alist2)))
         (let ((genes-1
                (if genes-pr-1
                    (cdr genes-pr-1)
                    '()))
               (genes-2
                (if genes-pr-2
                    (cdr genes-pr-2)
                    '())))
           (let ((genes (set-union genes-1 genes-2)))
             (hash-set! merged k genes)))))
     keys)
    (list (sort-hash merged))))

;; expanding form the merge-2-assoc-list function above, the merge-assoc-lists function takes a list of association
;; lists and return a merged and sorted association list with unique keys and values
(define (merge-assoc-lists ls-of-alist)
  (cond
    [(null? ls-of-alist) '()]
    [(> (length ls-of-alist) 1)
     (let ((merged (make-hash))
           (keys (set-union (map car (car ls-of-alist)) (map car (cadr ls-of-alist)))))
       (for-each
        (lambda (k)
          (let ((genes-pr-1 (assoc k (car ls-of-alist)))
                (genes-pr-2 (assoc k (cadr ls-of-alist))))
            (let ((genes-1
                   (if genes-pr-1
                       (cdr genes-pr-1)
                       '()))
                  (genes-2
                   (if genes-pr-2
                       (cdr genes-pr-2)
                       '())))
              (let ((genes (set-union genes-1 genes-2)))
                (hash-set! merged k genes)))))
        keys)
       (merge-assoc-lists (cons (sort-hash merged) (cddr ls-of-alist))))]
    [else
     (car ls-of-alist)]))


;;recursively merge values of a ls-of-alist with the given key 
(define merge-values
  (lambda (ls-of-alist key)
    (cond
      [(null? ls-of-alist) '()]
      [else
       (let ((first (car ls-of-alist))
             (rest (cdr ls-of-alist)))
         (let ((genes-pr-first (assoc key first)))
           (let ((genes-first
                  (if genes-pr-first
                    (list (cdr genes-pr-first))
                    '())))
             (let ((genes (set-union genes-first (merge-values rest key))))
               genes))))])))

#|
write-list-to-tsv takes a (header-list), a list of list and  a path-to-file name and
convert the list to tsv format. This has been defined in query.rkt

*example usage, where 'results' is bound to a list of lists:

(write-list-to-tsv
  (list "db" "subject" "predicate" "object")
  results
  "my-tsv.tsv")
|#
#|
(define write-list-to-tsv
  (lambda (header-ls lol path)
    (with-output-to-file path
      ;; thunk -- procedure that takes no arguments
      (lambda ()
        (for-each
          (lambda (l)
            (let loop ([l l])
              (cond
                ((null? l)
                 (error 'output-to-tsv "where's the data!?"))
                ((null? (cdr l)) ;; l contains exactly 1 element
                 (display (car l))
                 (display #\newline))
                (else
                 (display (car l))
                 (display #\tab)
                 (loop (cdr l))))))
          (cons header-ls lol)))
      #:mode 'text
      #:exists 'replace)))
|#

;; unwrap function flattens a list one level
(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))


