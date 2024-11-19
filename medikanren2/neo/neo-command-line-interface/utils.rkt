#lang racket/base

(require
  "../neo-low-level/query-low-level-multi-db.rkt"
  "../neo-utils/neo-helpers-without-db.rkt"
  racket/string
  racket/list
  racket/match
  racket/set
  )

(provide get-publications
         write-out-prepared-answers
         activate-edge-filter
         inhibite-edge-filter
         write-list-to-tsv
         curie->gene/protein-conflation
         synonym-format
         )

(define (get-assoc k m)
  (let ((r (assoc k m)))
    (if r
        (cadr r)
        #f)))

(define get-publications
  (lambda props
    (define (helper props pubs)
      (cond
        [(null? props) pubs]
        [else
         (let ((publication (or (get-assoc "publications" (car props))
                                (get-assoc "supporting_publications" (car props))
                                (get-assoc "publications:string[]" (car props)))))
           (if publication
               (helper (cdr props)
                       (append 
                        (cond
                          [(string-prefix? publication "(")
                           (string-split (string-trim (string-trim publication "(") ")"))]
                          [(string-contains? publication "|") (string-split publication "|")]
                          [(string-contains? publication ";") (string-split publication "; ")]
                          [else (string-split publication)])
                        pubs))
               (helper (cdr props) pubs)))]))
    (define pubs (filter
                  (lambda (p) (not (equal? "PMID:" p)))
                  (remove-duplicates (helper props '()))))
    pubs))

(define (get-mediKanren-score props)
  (string->number (get-assoc "mediKanren-score" props)))

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

(define (concept->name curie)
  (let ((id-name-val
         (remove-duplicates (filter (lambda (cl)
                                      (and (equal? (car cl) curie)
                                           (equal? (cadr cl) "name")))
                                    (query:Concept (list curie))))))
        (if (null? id-name-val)
            curie
            (caddar id-name-val))))

(define curie-names (make-hash))

(define (curie->name-remember curie)
  (let ((name (hash-ref curie-names curie #f)))
    (if name
        name
        (let ((name^ (concept->name curie)))
          (if (> (hash-count curie-names) 10000)
              (begin (set! curie-names (make-hash)) name^)
              (begin (hash-set! curie-names curie name^) name^))))))   

(define (write-out-prepared-answers edge* path)
  (write-list-to-tsv
   '("subject"
     "subject properties"
     "predicate"
     "object"
     "object properties"
     "publications"
     "mediKanren score"
     "edge properties")
   (map (lambda (e)
          (match e
            [`(,sub ,pred ,obj . ,prop)
             (list* (curie->name-remember sub)
                    (curie->properties sub)
                    pred
                    (curie->name-remember obj)
                    (curie->properties obj)
                    (get-publications prop)
                    (get-mediKanren-score prop)
                    prop)]
            [else '()]))
        (remove-duplicates edge*))
   path))

(define activate-edge-filter
  (lambda (e)
    (match e
      [`(,sub ,pred ,obj . ,prop)
       (let ((qualified-pred (get-assoc "qualified_predicate" prop))
             (object-aspect-qualifier
              (or (get-assoc "object_aspect_qualifier" prop)
                  (get-assoc "qualified_object_aspect" prop)))
             (object-direction-qualifier
              (or (get-assoc "object_direction_qualifier" prop)
                  (get-assoc "qualified_object_direction" prop))))
         (if (member pred (list "biolink:affects" "biolink:regulates"))
             (member (list pred qualified-pred object-aspect-qualifier object-direction-qualifier)
                     (list (list "biolink:regulates" #f #f "upregulated")
                           (list "biolink:regulates" "biolink:causes" "expression" "increased")
                           (list "biolink:regulates" "biolink:causes" #f "increased")
                           (list "biolink:regulates" "biolink:causes" #f "upregulated")
                           (list "biolink:affects" "biolink:causes" "expression" #f)
                           (list "biolink:affects" "biolink:causes" "expression" "increased")
                           (list "biolink:affects" "biolink:causes" "transport" "increased")
                           (list "biolink:affects" "biolink:causes" "activity_or_abundance" "increased")
                           (list "biolink:affects" "biolink:causes" "activity" "increased")
                           (list "biolink:affects" "biolink:causes" "secretion" "increased")
                           (list "biolink:affects" "biolink:causes" "synthesis" "increased")
                           (list "biolink:affects" "biolink:causes" "molecular_interaction" "increased")
                           (list "biolink:affects" "biolink:causes" "degradation" "decreased")
                           (list "biolink:affects" "biolink:causes" "localization" "increased")
                           (list "biolink:affects" "biolink:causes" "uptake" "increased")))
             #t))]
      [else (error "the edge format seems not correct")])))

(define inhibite-edge-filter
  (lambda (e)
    (match e
      [`(,sub ,pred ,obj . ,prop)
       (let ((qualified-pred (get-assoc "qualified_predicate" prop))
             (object-aspect-qualifier
              (or (get-assoc "object_aspect_qualifier" prop)
                  (get-assoc "qualified_object_aspect" prop)))
             (object-direction-qualifier
              (or (get-assoc "object_direction_qualifier" prop)
                  (get-assoc "qualified_object_direction" prop))))
         (if (member pred (list "biolink:affects" "biolink:regulates"))
             (member (list pred qualified-pred object-aspect-qualifier object-direction-qualifier)
                     (list (list "biolink:regulates" #f	#f "decreased")
                           (list "biolink:regulates" "biolink:causes" #f "decreased")
                           (list "biolink:regulates" "biolink:causes" #f "downregulated")
                           (list "biolink:regulates" #f	#f "downregulated")
                           (list "biolink:regulates" "biolink:causes" "expression" "decreased")
                           (list "biolink:affects" "biolink:causes" "degradation" #f)
                           (list "biolink:affects" "biolink:causes" "expression" "decreased")
                           (list "biolink:affects" "biolink:causes" "activity" "decreased")
                           (list "biolink:affects" "biolink:causes" "activity_or_abundance" "decreased")
                           (list "biolink:affects" "biolink:causes" "secretion" "decreased")
                           (list "biolink:affects" "biolink:causes" "molecular_interaction" "decreased")
                           (list "biolink:affects" "biolink:causes" "degradation" "increased")
                           (list "biolink:affects" "biolink:causes" "transport" "decreased")
                           (list "biolink:affects" "biolink:causes" "localization" "decreased")
                           (list "biolink:affects" "biolink:causes" "synthesis" "decreased")
                           (list "biolink:affects" "biolink:causes" "uptake" "decreased")))       
             #t))]
      [else (error "the edge format seems not correct")])))

(define (curie->gene/protein-conflation curies)
  (set->list 
   (set-union (list->set curies)
              (set-fixed-point
               (list->set
                (curies->synonyms 
                 (append 
                  (map car
                       (query:X->Known-scored
                        #f
                        '("biolink:gene_product_of")
                        curies
                        (list (list 1112) #f (list 0))))
                  (map caddr
                       (query:Known->X-scored
                        curies
                        '("biolink:gene_product_of")
                        #f
                        (list (list 1112) #f (list 0)))))))
               (lambda (new-curies)
                 (list->set
                  (curies->synonyms 
                   (append 
                    (map car
                         (query:X->Known-scored
                          #f
                          '("biolink:gene_product_of")
                          (set->list curies)
                          (list (list 1112) #f (list 0))))
                    (map caddr
                         (query:Known->X-scored
                          (set->list curies)
                          '("biolink:gene_product_of")
                          #f
                          (list (list 1112) #f (list 0))))))))))))

(define (synonym-format curie*)
  (map (lambda (c)
          (list* c (curie->name-remember c) (curie->properties c)))
        curie*))
  
           
      
  