#lang racket

(provide
  (all-defined-out))


#|

(define semmed-ht (make-hash))
;; (define rtx-ht (make-hash))  ;; no HGNC entries
(define robokop-ht (make-hash))
(define orange-ht (make-hash))


;;; Hmm--these concepts both have the same HGNC xref (6342)  [this is the only such issue in semmed]
#|
#("UMLS:C1416655" 4 "KIT gene" (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['HGNC:HGNC:6342', 'MTH:NOCODE', 'PDQ:CDR0000258381', 'CHV:0000049115', 'NCI:C18348', 'OMIM:164920']") ("id" . "UMLS:C1416655") ("umls_type" . "['T028']") ("labels" . "['gene']")))

#("UMLS:C0920288" 4 "C-KIT Gene" (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['NCI:TCGA', 'NCI_NCI-HGNC:HGNC:6342', 'LNC:LP94238-0', 'NCI:C18609', 'MTH:NOCODE']") ("id" . "UMLS:C0920288") ("umls_type" . "['T028']") ("labels" . "['gene']")))
|#

(printf "processing semmed data...\n")
;;; for semmed:
;;; cpu time: 6653 real time: 6737 gc time: 27
(time
 (let ((ip (open-input-file "data/semmed/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]
       [else
        (when (= (modulo cid 10000) 0)
          (printf "semmed cid: ~s\n" cid))
        (let ((props (vector-ref data 3)))
          (let ((pr (assoc "xrefs" props)))
            (when pr
              (let ((xrefs-string (cdr pr)))
                (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" xrefs-string)))
                  (when m*
                    (let ((key (car m*)))
                      (let ((existing-entry (hash-ref semmed-ht key #f)))
                        (when existing-entry
                          (printf "!!! failed when trying to add semmed entry for key/cid pair ~s/~s\n" key cid)
                          (printf "!!! existing entry for semmed key ~s: ~s\n\n" key existing-entry)
                          ;;(error "existing entry detected!")
                          ))
                     
                      ;; (printf "adding ~s for cid ~s\n" key cid)
                      (hash-set! semmed-ht key cid))))))))
        (loop (add1 cid) (read ip))]))))


(printf "processing robokop data...\n")
;;; for robokop:
;;; cpu time: 36974 real time: 37257 gc time: 167
(time
 (let ((ip (open-input-file "data/robokop/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]      
       [else
        (when (= (modulo cid 10000) 0)
          (printf "robokop cid: ~s\n" cid))
        (let ((CUI-string (vector-ref data 0)))
          (when (string-prefix? CUI-string "HGNC:")
            (let ((key CUI-string))
              (let ((existing-entry (hash-ref robokop-ht key #f)))
                (when existing-entry
                  (printf "!!! failed when trying to add entry for robokop key/cid pair ~s/~s\n" key cid)
                  (printf "!!! existing robokop entry for key ~s: ~s\n\n" key existing-entry)
                  ;;(error "existing entry detected!")
                  ))
              (hash-set! robokop-ht key cid))))
        (loop (add1 cid) (read ip))]))))

;; for each robokop entry in which the 0th element of the vector looks
;; like "HGNC:12530", add an association of the form '12530 to cid'.
#|
(hash-ref robokop-ht "HGNC:12530" #f) ;; 0
(hash-ref robokop-ht "HGNC:12541" #f) ;; 2
(hash-ref robokop-ht "HGNC:10082" #f) ;; 363651 
|#

;;; orange
#|
#("NCBIGene:1140" 6 "CHRNB1" (("iri" . "http://www.ncbi.nlm.nih.gov/gene/1140") ("synonym" . "(\"acetylcholine receptor, nicotinic, beta 1 (muscle)\" \"Acetylcholine Receptor, Muscle, Beta Subunit\" \"CHOLINERGIC RECEPTOR, NICOTINIC, BETA POLYPEPTIDE 1; CHRNB1\" \"CHRNB1\" \"Chrnb\")") ("in_taxon" . "NCBITaxon:9606") ("same_as" . "(\"ENSEMBL:ENSG00000170175\" \"HGNC:1961\" \"OMIM:100710\" \"Orphanet:119419\")") ("provided_by" . "(\"orphanet.ttl\" \"omim.ttl\")") ("description" . "cholinergic receptor nicotinic beta 1 subunit") ("id" . "NCBIGene:1140")))
|#

(printf "processing orange data...\n")
;;; for orange:
;;; cpu time: 24943 real time: 25162 gc time: 98
(time
 (let ((ip (open-input-file "data/orange/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]      
       [else
        (when (= (modulo cid 10000) 0)
          (printf "orange cid: ~s\n" cid))
        (let ((props (vector-ref data 3)))
          (let ((pr (assoc "same_as" props)))
            (when pr
              (let ((same-as-string (cdr pr)))
                (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" same-as-string)))
                  (when m*
                    (let ((key (car m*)))
                      (let ((existing-entry (hash-ref orange-ht key #f)))
                        (when existing-entry
                          (printf "!!! failed when trying to add entry for orange key/cid pair ~s/~s\n" key cid)
                          (printf "!!! existing orange entry for key ~s: ~s\n\n" key existing-entry)
                          ;;(error "existing entry detected!")

                          ;; (printf "adding ~s for cid ~s\n" (car m*) cid)
                          (hash-set! orange-ht key cid))))))))))
        (loop (add1 cid) (read ip))]))))

(printf "done processing data\n")

#|
>  (hash-count semmed-ht)
17848
>  (hash-count robokop-ht)
26683
> (hash-count orange-ht)
19511
|#

|#






;; (define hgnc-ht (make-hash))

#|
(printf "adding semmed entries...\n")
(time
 (let ((ip (open-input-file "data/semmed/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]
       [else
        (when (= (modulo cid 10000) 0)
          (printf "semmed cid: ~s\n" cid))
        (let ((props (vector-ref data 3)))
          (let ((pr (assoc "xrefs" props)))
            (when pr
              (let ((xrefs-string (cdr pr)))
                (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" xrefs-string)))
                  (when m*
                    (let ((key (car m*)))
                      (let ((entry (hash-ref hgnc-ht key '())))
                        (hash-set! hgnc-ht key (cons (cons 'semmed cid) entry))))))))))
        (loop (add1 cid) (read ip))]))))
|#

#|
(printf "adding robokop entries...\n")
(time
 (let ((ip (open-input-file "data/robokop/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]      
       [else
        (when (= (modulo cid 10000) 0)
          (printf "robokop cid: ~s\n" cid))
        (let ((CUI-string (vector-ref data 0)))
          (when (string-prefix? CUI-string "HGNC:")
            (let ((key CUI-string))
              (let ((entry (hash-ref hgnc-ht key '())))
                (hash-set! hgnc-ht key (cons (cons 'robokop cid) entry))))))
        (loop (add1 cid) (read ip))]))))
|#

#|
(printf "adding orange entries...\n")
(time
 (let ((ip (open-input-file "data/orange/concepts.scm")))
   (let loop ([cid 0]
              [data (read ip)])
     (cond
       [(eof-object? data)
        (close-input-port ip)
        (void)]      
       [else
        (when (= (modulo cid 10000) 0)
          (printf "orange cid: ~s\n" cid))
        (let ((props (vector-ref data 3)))
          (let ((pr (assoc "same_as" props)))
            (when pr
              (let ((same-as-string (cdr pr)))
                (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" same-as-string)))
                  (when m*
                    (let ((key (car m*)))
                      (let ((entry (hash-ref hgnc-ht key '())))
                        (hash-set! hgnc-ht key (cons (cons 'orange cid) entry))))))))))
        (loop (add1 cid) (read ip))]))))
|#











(define add-concept-key/cid-associations-to-hashtable
  (lambda (concepts-file-name db-name ht concept-vector->key)
    (let ((ip (open-input-file concepts-file-name)))
      (let loop ([cid 0]
                 [data (read ip)])
        (cond
          [(eof-object? data)
           (close-input-port ip)
           (void)]
          [else
           (when (= (modulo cid 10000) 0)
             (printf "~s cid: ~s\n" db-name cid))
           (let ((key (concept-vector->key data)))
             (when key
               (let ((entry (hash-ref ht key '())))
                 (hash-set! ht key (cons (cons db-name cid) entry))))) 
           (loop (add1 cid) (read ip))])))))


(define hgnc-ht (make-hash))


(add-concept-key/cid-associations-to-hashtable
 "data/semmed/concepts.scm"
 'semmed
 hgnc-ht
 (lambda (data)
   (let ((props (vector-ref data 3)))
     (let ((pr (assoc "xrefs" props)))
       (and pr
            (let ((xrefs-string (cdr pr)))
              (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" xrefs-string)))
                (and m*
                     (let ((key (car m*)))
                       key)))))))))

(add-concept-key/cid-associations-to-hashtable
 "data/robokop/concepts.scm"
 'robokop
 hgnc-ht
 (lambda (data)
   (let ((props (vector-ref data 3)))
     (let ((pr (assoc "equivalent_identifiers" props)))
       (and pr
            (let ((equivalent-identifiers-string (cdr pr)))
              (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" equivalent-identifiers-string)))
                (and m*
                     (let ((key (car m*)))
                       key)))))))))

(add-concept-key/cid-associations-to-hashtable
 "data/orange/concepts.scm"
 'orange
 hgnc-ht
 (lambda (data)
   (let ((props (vector-ref data 3)))
     (let ((pr (assoc "same_as" props)))
       (and pr
            (let ((same-as-string (cdr pr)))
              (let ((m* (regexp-match #rx"(HGNC:[0-9]+)" same-as-string)))
                (and m*
                     (let ((key (car m*)))
                       key)))))))))


(define op (open-output-file "hgnc-hash.rkt"))
(write hgnc-ht op)
(close-output-port op)

#|
(define ip (open-input-file "hgnc-hash.rkt"))
(define ht (read ip))
(close-input-port ip)
(hash-count ht)
|#
