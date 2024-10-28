#lang racket

(require
  "../neo-low-level/query-low-level-multi-db.rkt"
  "utils.rkt"
  racket/string
  readline
  )

;; TODO:
;; sentence column in output

(define (mediKanren-activates curie* path)
  (write-out-prepared-answers
   (filter
    activate-edge-filter
   (query:X->Known
    #f
    '("biolink:has_increased_amount"
      "biolink:affects"
      "biolink:produces"
      "biolink:regulates"
      "biolink:regulates"
      "biolink:enables"
      "biolink:treats"
      "biolink:positively_correlated_with"
      "biolink:produces"
      "biolink:ameliorates")
    curie*))
   path))

(define (mediKanren-inhibits curie* path)
  (write-out-prepared-answers
   (filter
    inhibite-edge-filter
    (query:X->Known
     #f
     '("biolink:prevents"
       "biolink:affects"
       "biolink:regulates"
       "biolink:has_decreased_amount"
       "biolink:disrupts"
       "biolink:negatively_correlated_with")
     curie*))
   path))

(define (mediKanren-all-relations curie* path)
  (write-out-prepared-answers
   (append
    (query:X->Known
     #f
     #f
     curie*)
    (query:Known->X
     curie*
     #f
     #f))
   path))

(define (mediKanren-with-predicates curie* predicate* path)
  (write-out-prepared-answers
   (append
    (query:X->Known
     #f
     predicate*
     curie*)
    (query:Known->X
     curie*
     predicate*
     #f))
   path))   

(define (mediKanren-synonyms curie*)
  (write-list-to-tsv
   '("curie" "curie-name" "curie-properties")
   (synonym-format curie*)
   "synonyms-details.tsv"))

; Main loop for CLI
(define (main-loop)
  (display "Enter your command: ")
  (let ([input (read-line)])
    (cond
      [(string-prefix? input "activates")
       (let ([arug* (string-split (substring input (string-length "activates")))])
         (match arug*
           [`(,c ... ">" ,file-name)
            (let ((curie* c)
                  (path file-name))
              (printf "The input curies are ~a\n" c)
              (mediKanren-activates curie* path)
              (printf "The results are written in file ~a\n" path)
              )]
           [else
            (displayln "Incomplete command. Please use 'activates CURIE ... > name-of-the-file'")]))]
      [(string-prefix? input "inhibits")
       (let ([arug* (string-split (substring input (string-length "inhibits")))])
         (match arug*
           [`(,c ... ">" ,file-name)
            (let ((curie* c)
                  (path file-name))
              (printf "The input curies are ~a\n" c)
              (mediKanren-inhibits curie* path)
              (printf "The results are written in file ~a\n" path)
              )]
           [else
            (displayln "Incomplete command. Please use 'inhibits CURIE ... > name-of-the-file'")]))]
      [(string-prefix? input "synonyms")
       (let* ([arug* (string-split (substring input (string-length "synonyms")))])
         (match arug*
           [`(,c ,c* ...) 
            (let* ((synonym* (curies->synonyms (list* c c*)))
                   (synonym* (curie->gene/protein-conflation synonym*)))
              (printf "The synonyms of ~a are ~a\n" arug* synonym*)
              (mediKanren-synonyms synonym*)
              (printf "You may find the details in the file synonyms-details.tsv.\n"))]
           [else (displayln "Incomplete command. Please use 'synonyms CURIE ...'")]))]
      [(string-prefix? input "all relations")
       (let* ([arug* (string-split (substring input (string-length "all relations")))])
         (match arug*
           [`(,c ... ">" ,file-name)
            (let ((curie* c)
                  (path file-name))
              (printf "The input curies are ~a\n" c)
              (mediKanren-all-relations curie* path)
              (printf "The results are written in file ~a\n" path)
              )]
           [else
            (displayln "Incomplete command. Please use 'all relations CURIE ... > name-of-the-file'")]))]
      [(string-prefix? input "with predicates")
       (let* ([arug* (string-split (substring input (string-length "with predicates")))])
         (match arug*
           [`("-predicate",p ... "-curie" ,c ... ">" ,file-name)
            (let ((curie* c)
                  (predicate* p)
                  (path file-name))
              (printf "The input predicates are ~a and the input curies are ~a\n" p c)
              (mediKanren-with-predicates curie* predicate* path)
              (printf "The results are written in file ~a\n" path)
              )]
           [else
            (displayln "Incomplete command. Please use 'with predicates -predicate PREDICATE ... -curie CURIE ... > name-of-the-file'")]))]
      [else (displayln "Unknown command. Please use
'activates CURIE ... > name-of-the-file' or
'inhibits CURIE ... > name-of-the-file' or
'synonyms CURIE ...' or
'all relations CURIE ... > name-of-the-file'
'with predicates -predicate PREDICATE ... -curie CURIE ... > name-of-the-file'")])
    (main-loop)))

; Start the program
(main-loop)