#lang racket/base

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; Problem encountered on 5 Dec 2022:
;;
;; * HGNC CURIES are not normalized to UMLS or NCBI or OMIM CURIES

;; Hand-curated mappings of HGNC to UMLS mappings, since there doesn't
;; seem to currently be a way to map HGNC CURIES to UMLS CURIES
;; through RTX-KG2 or through the node normalization knowledge graph.
(define symbol/HGNC/UMLS*-list
  '((MAPK8IP3  HGNC:6884   C1417026 C1436798)
    (VMA21     HGNC:22082  C2681112 C2715590)
    (NTRK2     HGNC:8032   C1334909 C1704842 C3853697)
    (SGO1      HGNC:25088  C1428445 C1565925 C3890006)
    (TPI1      HGNC:12009  C1420871 C3887669 C3541335 C0041078)
    (FKRP      HGNC:17997  C1425226 C1448221)
    (PHETA1    HGNC:26509  C1825239 C2932936)
    (SPOP      HGNC:11254  C1420368 C3540803 C0669183)
    (YWHAZ     HGNC:12855  C1421564 C3492243 C2981740)
    (JAK2      HGNC:6192   C1334291 C1527617 C0169661)
    (GDAP1     HGNC:15968  C1423872 C1453327)
    (ESCO2     HGNC:27230  C1539367 C1569535)
    (ZMIZ1     HGNC:16493  C1823956 C1433857 C3889263)
    (SBF1      HGNC:10542  C1419823 C1259214)
    (PEX10     HGNC:8851   C1418470 C1449205)
    (KIF5A     HGNC:6323   C1416638 C1569075)
    (LAMA2     HGNC:6482   C1416776 C1569076)
    (FOXG1     HGNC:3811   C0812297 C1705148)
    (BANP      HGNC:13450  C1538317 C0961071)
    (NEXMIF    HGNC:29433  C1845260 C4041927)
    (TBCD      HGNC:11581  C1420596 C1447856)
    (DHTKD1    HGNC:23537  C1428130 C3657993)
    (PNPT1     HGNC:23166  C1427960 C1310854 C2984134)
    (ALG2      HGNC:23159  C1427955)
    (SETBP1    HGNC:15573  C1423585 C1309881 C3815268)))

(define get-generic-synonym/descendent-curies
  (lambda (curie)
    (get-descendent-curies*-in-db
     (curies->synonyms-in-db (list curie)))))

(define get-gene-synonym/descendent-curies
  (lambda (curie)
    (let ((related-curies (get-generic-synonym/descendent-curies curie))
          (gene-synonym-predicates
           (set->list
            (apply set-union
                   (map get-predicate-descendents-in-db
                        '("biolink:same_as"
                          "biolink:expresses"
                          "biolink:has_gene_product"
                          "biolink:has_gene_or_gene_product"
                          "biolink:has_output"
                          "biolink:exact_match"
                          "biolink:mesh_terms"
                          ;; Beware of using 'xref', since it is a very loose predicate
                          ;; "biolink:xref"
                          ))))))
      (set-union
       related-curies
       (list->set
        (map (lambda (e) (list-ref e 3))
             (query:Known->X
              (set->list related-curies)
              gene-synonym-predicates
              #f)))
       (list->set
        (map car
             (query:X->Known
              #f
              gene-synonym-predicates
              (set->list related-curies))))))))

(define process-gene
  (lambda (curie)
    (let ((gene-curies (get-gene-synonym/descendent-curies curie)))
      (query:X->Known
       ;; Looking for any type of chemical substance
       #f
       ;; list of predicates
       (set->list
        (apply set-union
               (map get-predicate-descendents-in-db
                    '("biolink:positively_regulates"
                      "biolink:negatively_regulates"
                      ;;
                      "biolink:entity_positively_regulates_entity"
                      "biolink:entity_negatively_regulates_entity"
                      ;;
                      "biolink:increased_amount_of"
                      "biolink:increases_activity_of"
                      "biolink:increases_amount_or_activity_of"
                      "biolink:increases_abundance_of"
                      "biolink:increases_expression_of"
                      "biolink:increases_synthesis_of"
                      ;;
                      "biolink:decreased_amount_in"
                      "biolink:decreases_abundance_of"
                      "biolink:decreases_synthesis_of"
                      "biolink:decreases_activity_of"
                      "biolink:decreases_amount_or_activity_of"
                      "biolink:decreases_expression_of"))))
       ;; list of known CURIES (for genes)
       (set->list gene-curies)))))

(define process-symbol/HGNC/UMLS*-list
  (lambda (symbol/HGNC/UMLS*-list)
    (let loop ((ls symbol/HGNC/UMLS*-list))
      (match ls
        ['() '()]
        [`((,symbol ,hgnc-curie-symbol . ,cid-curie-symbols) . ,rest)
         (let ((hgnc-curie (symbol->string hgnc-curie-symbol))
               (cid-curies (map symbol->string cid-curie-symbols)))
           (let ((umls-curies (map (lambda (c) (string-append "UMLS:" c)) cid-curies)))
             (let ((answers (apply append (map process-gene (cons hgnc-curie umls-curies)))))
               (cons `(,symbol (,hgnc-curie-symbol . ,umls-curies) ,answers)
                     (loop rest)))))]))))

(define answers (process-symbol/HGNC/UMLS*-list symbol/HGNC/UMLS*-list))





#| Extract publication information from answers |#

(define (python->json py)
  (define len (string-length py))
  (let loop ((i 0) (start 0))
    (cond ((= i len) (if (= start 0) py (substring py start)))
          ((eqv? (string-ref py i) #\')
           (string-append
             (substring py start i) "\""
             (let requote ((i (+ i 1)) (start (+ i 1)))
               (cond ((eqv? (string-ref py i) #\')
                      (string-append (substring py start i) "\""
                                     (loop (+ i 1) (+ i 1))))
                     ((eqv? (string-ref py i) #\\)
                      (if (eqv? (string-ref py (+ i 1)) #\")
                        (requote (+ i 2) start)
                        (string-append (substring py start i)
                                       (requote (+ i 2) (+ i 1)))))
                     ((eqv? (string-ref py i) #\")
                      (string-append (substring py start i) "\\\""
                                     (requote (+ i 1) (+ i 1))))
                     (else (requote (+ i 1) start))))))
          ((eqv? (string-ref py i) #\")
           (let skip ((i (+ i 1)) (start start))
             (cond ((eqv? (string-ref py i) #\") (loop (+ i 1) start))
                   ((eqv? (string-ref py i) #\\)
                    (if (eqv? (string-ref py (+ i 1)) #\")
                      (skip (+ i 2) start)
                      (string-append (substring py start i)
                                     (skip (+ i 2) (+ i 1)))))
                   (else                         (skip (+ i 1) start)))))
          (else (loop (+ i 1) start)))))

(define (pubmed-ids-from-edge-props eprops)
  (cond
    [(assoc "publications" eprops)
     => (lambda (pr)
          (define pubs (cadr pr))
          (if (not (string? pubs))
            '()
            (regexp-match* #rx"PMID:([0-9]+)" pubs #:match-select cadr)))]
    [else '()]))
(define (pubmed-ids-from-edge edge)
  (remove-duplicates
    (match edge
      [`(,s-curie ,s-name ,pred ,o-curie ,o-name . ,eprops)
        (pubmed-ids-from-edge-props eprops)])))
(define PUBMED_URL_PREFIX "https://www.ncbi.nlm.nih.gov/pubmed/")
(define (pubmed-URLs-from-edge edge)
  (map (lambda (pubmed-id) (string-append PUBMED_URL_PREFIX (~a pubmed-id)))
       (pubmed-ids-from-edge edge)))

(define (publications-info-alist-from-edge-props eprops)
  (cond
    [(assoc "publications_info" eprops) ;; RTX2
     => (lambda (pr)
          (with-handlers ([exn:fail?
                           (lambda (v)
                             ((error-display-handler) (exn-message v) v)
                             '())])
            (define pubs (cadr pr))
            ;;(printf "pubs:\n~s\n" pubs)
            (define jason-ht (string->jsexpr (python->json pubs)))
            ;;(printf "jason-ht:\n~s\n" jason-ht)
            (let ((pub-info-ls
                   (hash-map jason-ht (lambda (k v)
                                        (let ((pubmed-num* (regexp-match* #rx"([0-9]+)"
                                                                          (symbol->string k)
                                                                          #:match-select cadr)))
                                          (if (pair? pubmed-num*)
                                              (let ((pubmed-url
                                                     (string-append PUBMED_URL_PREFIX (car pubmed-num*))))
                                                (cons pubmed-url
                                                      (list (hash-ref v '|publication date| #f)
                                                            (hash-ref v '|subject score| #f)
                                                            (hash-ref v '|object score| #f)
                                                            (regexp-replace*
                                                             #rx"([ ]+)"
                                                             (hash-ref v 'sentence #f)
                                                             " "))))
                                              ;; else, the JSON is malformed
                                              #f))))))
              ;; filter out any #f results from malformed JSON
              (filter (lambda (v) v) pub-info-ls))))]
    [else '()]))
(define (publications-info-alist-from-edge edge)
  ;; ((pubmed-URL . (publication-date subject-score object-score sentence)) ...)
  (remove-duplicates
    (match edge
      [`(,s-curie ,s-name ,pred ,o-curie ,o-name . ,eprops)
        (publications-info-alist-from-edge-props eprops)])))



(define write-answer-to-TSV-file
  (lambda (answer)
    (match answer
      [`(,gene-symbol (,hgnc-curie-symbol . ,umls-curies) ,answers)
       (let ((op (open-output-file
                  (string-append
                    "X regulates "
                    (symbol->string gene-symbol)
                    " "
                    (~a `(,hgnc-curie-symbol . ,umls-curies))
                    ".tsv")
                  #:mode 'text
                  #:exists 'error)))
         (fprintf op
                  "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t\n"
                  "Subject CURIE"
                  "Subject Name"
                  "Predicate"
                  "Object CURIE"
                  "Object Name"
                  "Pubmed URL"
                  "Publication Date"
                  "Sentence")
         (let loop ((answers answers))
           (match answers
             [`() (close-output-port op)]
             [`(,answer . ,rest)
              (match answer
                [`(,s-curie ,s-name ,pred ,o-curie ,o-name . ,eprops)
                 ;;(printf "calling publications-info-alist-from-edge on:\n")
                 ;;(printf "~s\n" `(,s-curie ,s-name ,pred ,o-curie ,o-name . ,eprops))
                 (let ((pubs-alist (publications-info-alist-from-edge answer)))
                   ;; ((pubmed-URL . (publication-date subject-score object-score sentence)) ...)
                   ;;(printf "pubs-alist:\n~s\n" pubs-alist)
                   (cond
                     [(null? pubs-alist)
                      (let ((pubmed-URLs (pubmed-URLs-from-edge answer)))
                        (cond
                          [(null? pubmed-URLs)
                           (fprintf op
                                    "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t\n"
                                    s-curie
                                    s-name
                                    pred
                                    o-curie
                                    o-name
                                    ""
                                    ""
                                    "")                           
                           (loop rest)]
                          [else
                           (for-each
                             (lambda (pubmed-URL)
                               (fprintf op
                                        "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t\n"
                                        s-curie
                                        s-name
                                        pred
                                        o-curie
                                        o-name
                                        pubmed-URL
                                        ""
                                        ""))
                             pubmed-URLs)
                           (loop rest)]))]
                     [else
                      (for-each
                        (lambda (pub-info)
                          (match pub-info
                            [`(,pubmed-URL . (,publication-date ,subject-score ,object-score ,sentence))
                             (fprintf op
                                      "~a\t~a\t~a\t~a\t~a\t~a\t~a\t~a\t\n"
                                      s-curie
                                      s-name
                                      pred
                                      o-curie
                                      o-name
                                      pubmed-URL
                                      publication-date
                                      sentence)]))
                        pubs-alist)
                      (loop rest)]))])])))])))

(for-each write-answer-to-TSV-file answers)
