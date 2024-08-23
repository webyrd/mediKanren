#lang racket/base

(provide concept->name
         concept->category

         curie-synonyms-and-descendents

         write-answers-to-tsv

         get-pubs
         get-primary-knowledge-source
         
         get-assoc
         list-assoc
         merge-list
         merge-hash   
         )

(require racket/list
         racket/math
         racket/string
         json
         racket/match
         "../../../medikanren2/neo/neo-server/neo-server-utils.rkt"
         "../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
         "../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"         
         "../../../medikanren2/neo/neo-utils/neo-helpers-without-db.rkt"
         )

(define (get-pubs props)
  (if (zero? (num-pubs props))
      '()
      (hash-ref (get-publications props) 'value)))

(define (get-primary-knowledge-source props)
  (if (edge-has-source? props)
      (hash-ref (get-source props) 'resource_id)
      (error 'get-primary-knowledge-source (format "no primary knowledge source in properties: ~s" props))))





(define (curie-synonyms-and-descendents curie-list)
  (get-descendent-curies*-in-db
   (curies->synonyms-in-db curie-list)))

#;(define (concept->name curie)
  (let ((id-name-val
         (remove-duplicates (filter (lambda (cl)
                                      (and (equal? (car cl) curie)
                                           (equal? (cadr cl) "name")))
                                    (query:Concept (list curie))))))
        (if (null? id-name-val)
            curie
            (caddar id-name-val))))

(define (concept->name curie)
  (let ((name (assoc "name" (curie->properties curie))))
    (if name (cadr name) curie)))

(define (concept->category curie)
  (let ((category (assoc "category" (curie->properties curie))))
    (if category (cdr category) '())))

(define (write-answers-to-tsv file-name lines)
  (let ((op (open-output-file file-name #:mode 'text #:exists 'replace)))
    (for-each
      (lambda (line)
        (fprintf op (string-join line "\t"))
        (fprintf op "\n"))
      lines)
    (close-output-port op)))
