;;; X-Y-C queries
;;; unknown-unknown-known concepts
;;;
;;; example:
;;;
;;; X - increases - G1 - increases - G2
;;;
;;; where G1 and G2 are genes.
;;;
;;; Also want to report what other things that G1 increases (as part
;;; of tox info)

#lang racket

(provide
  (all-from-out "mk.rkt")
  (all-from-out "mk-db.rkt")
  (all-from-out "common.rkt")
  (all-defined-out))

(require
  "mk.rkt"
  "db.rkt"
  "mk-db.rkt"
  "common.rkt"
  (except-in racket/match ==)
  (only-in srfi/1 iota))

(define X-Y-C_VERSION_STRING "X-Y-C Tool 0.1.0")

(define argv (current-command-line-arguments))
(define argv-optional '#(CONFIG_FILE))

(when (not (<= (vector-length argv) (vector-length argv-optional)))
  (error "optional arguments ~s; given ~s" argv-optional argv))

(displayln "Starting X-Y-C tool...")
(displayln X-Y-C_VERSION_STRING)
;; Loading will occur at first use if not explicitly forced like this.
(load-config #t (and (<= 1 (vector-length argv)) (vector-ref argv 0)))
(load-databases #t)

;;; Decreases/increases predicate names
(define DECREASES_PREDICATE_NAMES (config-ref 'decreases-predicate-names))
(define INCREASES_PREDICATE_NAMES (config-ref 'increases-predicate-names))


(define G-G-increases-predicate-names
  '("positively_regulates"
    ;; "causes"
    ;; "produces"  ;;; maybe??
    ;; "causes_condition"
    ;; "causally_related_to"
    ;; "contributes_to"
    ;; "causes_adverse_event"
    ;; "gene_associated_with_condition"
    ;; "gene_mutations_contribute_to"
    ;; "disease_to_gene_association"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "decreases_degradation_of"
    "posetively_regulates" ;;; robokop typo??
    ;; "positively_regulates__entity_to_entity"
    ;; "increases_molecular_modification_of"
    ;; "increases_localization_of"
    ;; "increases_splicing_of"
    ;; "decreases_mutation_rate_of"
    ;; "predisposes"
    ;; "decreases_metabolic_processing_of"
    ))

(define D-G-increases-predicate-names
  '("positively_regulates"
    ;; "causes"
    ;; "produces"  ;;; maybe??
    ;; "causes_condition"
    ;; "causally_related_to"
    ;; "contributes_to"
    ;; "causes_adverse_event"
    ;; "gene_associated_with_condition"
    ;; "gene_mutations_contribute_to"
    ;; "disease_to_gene_association"
    "increases_activity_of"
    "increases_expression_of"
    "increases_molecular_interaction"
    "increases_response_to"
    "increases_secretion_of"
    "increases_stability_of"
    "increases_synthesis_of"
    "increases_transport_of"
    "increases_uptake_of"
    "decreases_degradation_of"
    "posetively_regulates" ;;; robokop typo??
    ;; "positively_regulates__entity_to_entity"
    ;; "increases_molecular_modification_of"
    ;; "increases_localization_of"
    ;; "increases_splicing_of"
    ;; "decreases_mutation_rate_of"
    ;; "predisposes"
    ;; "decreases_metabolic_processing_of"
    ))

;;; X - increases - G1 - increases - G2


#|
;;; let's try NGLY1 as an example
(define G2-concepts
  '((semmed 141262 "UMLS:C1425023" "NGLY1 gene" (4 . "gene") (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['HGNC:HGNC:17646', 'OMIM:610661', 'MTH:NOCODE']") ("id" . "UMLS:C1425023") ("umls_type" . "['T028']") ("labels" . "['gene']")))
 (robokop 22660 "HGNC:17646" "NGLY1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "3") ("location" . "3p24.2") ("taxon" . "9606") ("id" . "HGNC:17646") ("equivalent_identifiers" . "(\"UniProtKB:H0Y2P2\" \"ENSEMBL:ENSG00000151092\" \"UniProtKB:Q96IV0\" \"UniProtKB:C9JU75\" \"HGNC:17646\" \"NCBIGENE:55768\" \"UniProtKB:A0A0C4DFP4\")")))
 (orange 16958 "NCBIGene:55768" "NGLY1" (6 . "(\"gene\")") (("iri" . "http://www.ncbi.nlm.nih.gov/gene/55768") ("synonym" . "(\"FLJ11005\" \"PNG1\" \"peptide-N(4)-(N-acetyl-beta-glucosaminyl)asparagine amidase\" \"Peptide-N-Glycanase 1, S. Cerevisiae, Homolog of\" \"N-GLYCANASE 1; NGLY1\" \"NGLY1\")") ("in_taxon" . "NCBITaxon:9606") ("description" . "N-glycanase 1") ("same_as" . "(\"ENSEMBL:ENSG00000151092\" \"HGNC:17646\" \"OMIM:610661\" \"Orphanet:406885\")") ("provided_by" . "(\"orphanet.ttl\" \"omim.ttl\")") ("id" . "NCBIGene:55768")))))
|#

;;; let's try BRCA1 as an example
(define G2-concepts
  '((semmed 74686 "UMLS:C0376571" "BRCA1 gene" (4 . "gene") (("umls_type_label" . "['Gene or Genome']") ("xrefs" . "['NCI_NCI-HGNC:HGNC:1100', 'CHV:0000031821', 'PDQ:CDR0000043111', 'MESH:D019398', 'CSP:4005-0006', 'MTH:NOCODE', 'LNC:LP36227-4', 'NCI:C17965', 'LNC:LP19666-4', 'OMIM:113705', 'HGNC:HGNC:1100']") ("id" . "UMLS:C0376571") ("umls_type" . "['T028']") ("labels" . "['gene']")))
    (robokop 26 "HGNC:1100" "BRCA1" (0 . "(\"named_thing\" \"gene\")") (("locus_group" . "protein-coding gene") ("chromosome" . "17") ("taxon" . "9606") ("gene_family" . "(\"Ring finger proteins\" \"FA complementation groups\" \"Protein phosphatase 1 regulatory subunits\" \"BRCA1 A complex\" \"BRCA1 B complex\" \"BRCA1 C complex\")") ("location" . "17q21.31") ("id" . "HGNC:1100") ("gene_family_id" . "(58 548 694 1328 1335 1336)") ("equivalent_identifiers" . "(\"UniProtKB:C9IZW4\" \"UniProtKB:E9PC22\" \"UniProtKB:A0A2R8Y7V5\" \"UniProtKB:H0Y8D8\" \"UniProtKB:E9PH68\" \"UniProtKB:K7EPC7\" \"UniProtKB:E7EQW4\" \"UniProtKB:H0Y881\" \"UniProtKB:E7EWN5\" \"UniProtKB:H0Y850\" \"UniProtKB:C6YB45\" \"UniProtKB:E7EUM2\" \"UniProtKB:A0A024R1V0\" \"HGNC:1100\" \"UniProtKB:A0A0U1RRA9\" \"UniProtKB:E7ENB7\" \"UniProtKB:K7EJW3\" \"UniProtKB:H0Y8B8\" \"UniProtKB:A0A2R8Y6Y9\" \"UniProtKB:Q5YLB2\" \"UniProtKB:P38398\" \"UniProtKB:B7ZA85\" \"UniProtKB:A0A0A0MSN1\" \"ENSEMBL:ENSG00000012048\" \"UniProtKB:Q3B891\" \"UniProtKB:G1UI37\" \"NCBIGENE:672\" \"UniProtKB:A0A2R8Y587\")")))
    (orange 32553 "NCBIGene:672" "BRCA1" (6 . "(\"gene\")") (("iri" . "http://www.ncbi.nlm.nih.gov/gene/672") ("synonym" . "(\"BRCA1/BRCA2-containing complex, subunit 1\" \"Fanconi anemia, complementation group S\" \"protein phosphatase 1, regulatory subunit 53\" \"BRCC1\" \"FANCS\" \"PPP1R53\" \"RNF53\" \"BREAST CANCER 1 GENE; BRCA1\" \"BRCA1\")") ("in_taxon" . "NCBITaxon:9606") ("same_as" . "(\"ENSEMBL:ENSG00000012048\" \"HGNC:1100\" \"OMIM:113705\" \"Orphanet:119068\")") ("provided_by" . "(\"orphanet.ttl\" \"omim.ttl\")") ("description" . "BRCA1, DNA repair associated") ("id" . "NCBIGene:672")))))

;;; Want to restrict to gene/protein/whatever
;;;
;;; This is pretty restrictive, and doesn't include 'rtx' (which seems
;;; to use 'protein' instead of gene).
(define (restrict-concept-to-gene dbname c)
  (fresh (cid cui name catid cat props)
    (== `(,cid ,cui ,name (,catid . ,cat) ,props) c)
    (membero `(,dbname (,catid . ,cat))
             `((semmed (4 . "gene"))
               (robokop (0 . "(\"named_thing\" \"gene\")"))    
               (orange (6 . "(\"gene\")"))))))

;;; pretty restrictive!
;;; what about orange?
(define (restrict-concept-to-drug dbname c)
  (fresh (cid cui name catid cat props)
    (== `(,cid ,cui ,name (,catid . ,cat) ,props) c)
    (membero `(,dbname (,catid . ,cat))
             `((semmed (5 . "chemical_substance"))
               (rtx (0 . "chemical_substance"))
               (robokop (1 . "(\"named_thing\" \"chemical_substance\")"))))))

(define G1-G2-edges
  (remove-duplicates
   (run* (e)
     (fresh (dbname eid x o pid pred eprops)
       (== `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops) e)
       (membero `(,dbname . ,o) G2-concepts)
       (edgeo e)
       (membero pred G-G-increases-predicate-names)
       (restrict-concept-to-gene dbname x)))))

#|
;;; slow query 1
;;; cpu time: 24794 real time: 25191 gc time: 3238
(time
 (length
  (remove-duplicates
   (run 1 (path)
     (fresh (dbname1 eid1 s1 o1 pid1 pred1 eprops1 e1
                     dbname2 eid2 s2 o2 pid2 pred2 eprops2 e2
                     cid1 cui1 name1 catid1 cat1 props1
                     cid2 cui2 name2 catid2 cat2 props2)
       (== `(,dbname1 ,eid1 ,s1 ,o1 (,pid1 . ,pred1) ,eprops1) e1)
       (== `(,dbname2 ,eid2 ,s2 ,o2 (,pid2 . ,pred2) ,eprops2) e2)
       (== `(,e1 ,e2) path)

       (== `(,cid1 ,cui1 ,name1 (,catid1 . ,cat1) ,props1) o1)
       (== `(,cid2 ,cui2 ,name2 (,catid2 . ,cat2) ,props2) s2)
       (== cui1 cui2)
       (membero e2 G1-G2-edges)
       (edgeo e1)
       (restrict-concept-to-drug dbname1 s1)
       (membero pred1 D-G-increases-predicate-names)
       )))))

;;; swap two goals in the query above to make it suuuupeeer slow
(time
  (length
    (remove-duplicates
      (run 1 (path)
        (fresh (dbname1 eid1 s1 o1 pid1 pred1 eprops1 e1
                        dbname2 eid2 s2 o2 pid2 pred2 eprops2 e2
                        cid1 cui1 name1 catid1 cat1 props1
                        cid2 cui2 name2 catid2 cat2 props2)
          (== `(,dbname1 ,eid1 ,s1 ,o1 (,pid1 . ,pred1) ,eprops1) e1)
          (== `(,dbname2 ,eid2 ,s2 ,o2 (,pid2 . ,pred2) ,eprops2) e2)
          (== `(,e1 ,e2) path)

          (== `(,cid1 ,cui1 ,name1 (,catid1 . ,cat1) ,props1) o1)
          (== `(,cid2 ,cui2 ,name2 (,catid2 . ,cat2) ,props2) s2)
          (== cui1 cui2)
          (membero e2 G1-G2-edges)
          (restrict-concept-to-drug dbname1 s1)
          (edgeo e1)
          (membero pred1 D-G-increases-predicate-names)
          )))))
#|






#|
(time
  (length
    (remove-duplicates
      (run 1 (path)
        (fresh (dbname1 eid1 s1 o1 pid1 pred1 eprops1 e1
                        dbname2 eid2 s2 o2 pid2 pred2 eprops2 e2
                        cid1 cui1 name1 catid1 cat1 props1
                        cid2 cui2 name2 catid2 cat2 props2)
          (== `(,dbname1 ,eid1 ,s1 ,o1 (,pid1 . ,pred1) ,eprops1) e1)
          (== `(,dbname2 ,eid2 ,s2 ,o2 (,pid2 . ,pred2) ,eprops2) e2)
          (== `(,e1 ,e2) path)

          (== `(,cid1 ,cui1 ,name1 (,catid1 . ,cat1) ,props1) o1)
          (== `(,cid2 ,cui2 ,name2 (,catid2 . ,cat2) ,props2) s2)
          (== cui1 cui2)
          (membero e2 G1-G2-edges)
          (restrict-concept-to-drug dbname1 s1)
          (edgeo e1)
          (membero pred1 D-G-increases-predicate-names)
          )))))
|#

#|
(define D-G1-G2-paths
  (remove-duplicates
   (run* (path)
     (fresh (dbname1 eid1 s1 o1 pid1 pred1 eprops1 e1
             dbname2 eid2 s2 o2 pid2 pred2 eprops2 e2
             cid1 cui1 name1 catid1 cat1 props1
             cid2 cui2 name2 catid2 cat2 props2)
       (== `(,dbname1 ,eid1 ,s1 ,o1 (,pid1 . ,pred1) ,eprops1) e1)
       (== `(,dbname2 ,eid2 ,s2 ,o2 (,pid2 . ,pred2) ,eprops2) e2)
       (== `(,e1 ,e2) path)

       (== `(,cid1 ,cui1 ,name1 (,catid1 . ,cat1) ,props1) o1)
       (== `(,cid2 ,cui2 ,name2 (,catid2 . ,cat2) ,props2) s2)
       (== cui1 cui2) ;; tie the KG's together
       
       (membero e2 G1-G2-edges)
       (edgeo e1)
       (membero pred1 D-G-increases-predicate-names)
       (restrict-concept-to-drug dbname1 s1)))))
|#



#|
Exclude list?

(160755
 "UMLS:C1334043"
 "Homologous Gene"
 (4 . "gene")
 (("umls_type_label" . "['Gene or Genome']")
  ("xrefs" . "['NCI:C28709', 'MTH:NOCODE']")
  ("id" . "UMLS:C1334043")
  ("umls_type" . "['T028']")
  ("labels" . "['gene']")))

|#

#|
(~name*-concepto ~name* concept)

(~cui-concepto ~cui concept)

(~categoryo ~category-name category)

(~predicateo ~predicate-name predicate)

(sort-paths paths)

(pmid-edgeo pmid edge)

(pubmed-URLs-from-edge edge)

(define predicates
  (sort (remove-duplicates
         (time (run* (predicate)
                 (fresh (dbname pid c)
                   (membero c selected-concepts)
                   (concept-predicateo c `(,dbname ,pid . ,predicate))))))
        string<?))

(run* (q) (fuzzy-concepto current-name q))

(remove-duplicates
 (run 50 (s-with-dbname) ;; 50 should probably be a parameter ;
   (fresh (o-with-dbname dbname o s eid pid eprops e)
     (membero o-with-dbname ans)
     (== `(,dbname . ,o) o-with-dbname)
     (== `(,dbname ,eid ,s ,o (,pid . "subclass_of") ,eprops) e)
     (== `(,dbname . ,s) s-with-dbname)
     (edgeo e))))

(set! paths
      (remove-duplicates
       (append paths
               (run* (q)
                 (fresh (e dbname eid x o pid pred eprops)
                   (== (list `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops)) q)
                   (== `(,dbname . ,x) selected-X)
                   (== `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops) e)
                   (membero `(,dbname . ,o) concept-2*)
                   (membero pred atomic-predicate-2*)
                   (edgeo e))))))

(set! paths
      (remove-duplicates
       (append paths
               (run* (q)
                 (fresh (e dbname eid s x pid pred eprops)
                   (== (list `(,dbname ,eid ,s ,x (,pid . ,pred) ,eprops)) q)
                   (== `(,dbname . ,x) selected-X)
                   (== `(,dbname ,eid ,s ,x (,pid . ,pred) ,eprops) e)
                   (membero `(,dbname . ,s) concept-1*)
                   (membero pred atomic-predicate-1*)
                   (edgeo e))))))

(run* (q)
  (fresh (e dbname eid x o pid pred eprops)
    (== (list `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops)) q)
    (== `(,dbname . ,x) selected-X)
    (== `(,dbname ,eid ,x ,o (,pid . ,pred) ,eprops) e)
    (membero `(,dbname . ,o) concept-2*)
    (membero pred atomic-predicate-2*)
    (edgeo e)))

(set! all-X-concepts-with-edges
      (remove-duplicates
       (append all-X-concepts-with-edges
               (run* (q)
                 (fresh (dbname eid s o pid pred eprops e)
                        ;; TODO FIXME -- epropos may contain pubmed ids--how to extract it, or other evidence? ;
                   (== (list dbname s (list eprops) (list e)) q)
                   (== `(,dbname ,eid ,s ,o (,pid . ,pred) ,eprops) e)
                   (membero `(,dbname . ,o) concept-2*)
                   (membero pred atomic-predicate-2*)
                   (edgeo e))))))

(filter ;; only include concepts with at least one predicate ;
 (lambda (x)
   (match x
     [`(,dbname . ,concept)
      (let ((preds
             (run 1 (pred)
               (fresh (o s eid eprops e)
                 (case edge-type
                   [(out-edge) (== concept s)]
                   [(in-edge) (== concept o)])
                 (== `(,dbname ,eid ,s ,o ,pred ,eprops) e)
                 (edgeo e)))))
        (not (null? preds)))]))
 ans)

|#


