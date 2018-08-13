#lang racket
(require
  "mk-db.rkt"
  )

(define monarch-lite (time (make-db "data/monarch-lite")))

(displayln "categories:")
(pretty-print (run* (c) (db:categoryo monarch-lite c)))

(newline)
(displayln "predicates:")
(pretty-print (run* (p) (db:predicateo monarch-lite p)))

(newline)
(displayln "some concepts:")
(pretty-print (run 10 (c) (db:concepto monarch-lite c)))

(newline)
(displayln "some edges:")
(pretty-print (run 10 (e) (db:edgeo monarch-lite e)))

(newline)
(displayln "fuzzy search DHX30:")
(time (pretty-print (run* (c) (db:~name-concepto monarch-lite "DHX30" c))))
;; =>
'((4728
   "HGNC:16716"
   "DHX30"
   (1 . "['gene', 'sequence feature']")
   (("iri" . "http://identifiers.org/hgnc/HGNC:16716")
    ("clique"
     .
     "['HGNC:16716', 'NCBIGene:22907', 'OMIM:616423', 'ENSEMBL:ENSG00000132153', 'ENSEMBL:ENSP00000343442', 'ENSEMBL:ENSP00000392601', 'ENSEMBL:ENSP00000394682', 'ENSEMBL:ENSP00000379094', 'ENSEMBL:ENSP00000405620', 'UniProtKB:Q7L2E3', 'ENSEMBL:ENSP00000483160', 'ENSEMBL:ENSP00000395166', 'ENSEMBL:ENSP00000410571']")
    ("http://purl.org/dc/elements/1.1/description" . "DExH-box helicase 30")
    ("http://www.w3.org/2000/01/rdf-schema#label" . "DHX30")
    ("id" . "HGNC:16716")
    ("label" . "DHX30")
    ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
    ("labels"
     .
     "['gene', 'sequence_feature', 'Class', 'cliqueLeader', 'Node']")))
  (28624
   "MGI:1920081"
   "Dhx30"
   (1 . "['gene', 'sequence feature']")
   (("iri" . "http://www.informatics.jax.org/accession/MGI:1920081")
    ("clique"
     .
     "['MGI:1920081', 'NCBIGene:72831', 'ENSEMBL:ENSMUSG00000032480', 'ENSEMBL:ENSMUSP00000143371', 'ENSEMBL:ENSMUSP00000062622', 'ENSEMBL:ENSMUSP00000143529', 'ENSEMBL:ENSMUSP00000142659', 'UniProtKB:Q99PU8', 'ENSEMBL:ENSMUSP00000143616', 'ENSEMBL:ENSMUSP00000143607', 'ENSEMBL:ENSMUSP00000143700', 'ENSEMBL:ENSMUSP00000143751', 'ENSEMBL:ENSMUSP00000142549', 'ENSEMBL:ENSMUSP00000142489', 'ENSEMBL:ENSMUSP00000129174', 'ENSEMBL:ENSMUSP00000142952', 'ENSEMBL:ENSMUSP00000142636', 'ENSEMBL:ENSMUSP00000143272', 'ENSEMBL:ENSMUSP00000107622', 'ENSEMBL:ENSMUSP00000142836', 'ENSEMBL:ENSMUSP00000142846']")
    ("synonym"
     .
     "['PTN000433446', 'helG', 'Ddx30', 'C130058C04Rik', 'UniProtKB:Q99PU8', '2810477H02Rik', 'DEAH (Asp-Glu-Ala-His) box polypeptide 30']")
    ("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym"
     .
     "['PTN000433446', 'helG', 'Ddx30', 'C130058C04Rik', 'UniProtKB:Q99PU8', '2810477H02Rik', 'DEAH (Asp-Glu-Ala-His) box polypeptide 30']")
    ("http://purl.org/dc/elements/1.1/description"
     .
     "['Putative ATP-dependent RNA helicase DHX30', 'DEAH (Asp-Glu-Ala-His) box polypeptide 30']")
    ("http://www.w3.org/2000/01/rdf-schema#label" . "Dhx30")
    ("id" . "MGI:1920081")
    ("label" . "Dhx30")
    ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
    ("labels"
     .
     "['gene', 'sequence_feature', 'Class', 'cliqueLeader', 'Node']")))
  (75120
   "ZFIN:ZDB-GENE-130530-833"
   "dhx30"
   (1 . "['gene', 'sequence feature']")
   (("iri" . "http://zfin.org/ZDB-GENE-130530-833")
    ("synonym" . "zmp:0000000830")
    ("clique"
     .
     "['ZFIN:ZDB-GENE-130530-833', 'ENSEMBL:ENSDARG00000077839', 'NCBIGene:570735', 'ENSEMBL:ENSDARP00000101025']")
    ("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym"
     .
     "zmp:0000000830")
    ("http://purl.org/dc/elements/1.1/description"
     .
     "DEAH (Asp-Glu-Ala-His) box helicase 30")
    ("http://www.w3.org/2000/01/rdf-schema#label" . "dhx30")
    ("id" . "ZFIN:ZDB-GENE-130530-833")
    ("label" . "dhx30")
    ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
    ("labels"
     .
     "['gene', 'sequence_feature', 'Class', 'cliqueLeader', 'Node']")))
  (268417
   "RGD:1308888"
   "Dhx30"
   (1 . "['gene', 'sequence feature']")
   (("iri" . "http://rgd.mcw.edu/rgdweb/report/gene/main.html?id=1308888")
    ("clique" . "['RGD:1308888', 'NCBIGene:367172']")
    ("synonym" . "['UniProtKB:Q5BJS0', 'PTN000433447']")
    ("http://www.geneontology.org/formats/oboInOwl#hasExactSynonym"
     .
     "['UniProtKB:Q5BJS0', 'PTN000433447']")
    ("http://purl.org/dc/elements/1.1/description"
     .
     "['Putative ATP-dependent RNA helicase DHX30', 'DExH-box helicase 30']")
    ("http://www.w3.org/2000/01/rdf-schema#label" . "Dhx30")
    ("id" . "RGD:1308888")
    ("label" . "Dhx30")
    ("https://monarchinitiative.org/MONARCH_cliqueLeader" . "True")
    ("labels"
     .
     "['gene', 'sequence_feature', 'Class', 'cliqueLeader', 'Node']"))))

(newline)
(displayln "X ANY-PRED DHX30:")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto monarch-lite "DHX30" `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))
                        (db:edgeo monarch-lite edge)))))

(newline)
(displayln "DHX30 ANY-PRED X")
(time (pretty-print (run* (edge)
                      (fresh (scid scui sname scatid scat sprops
                                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
                        (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
                                   (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
                                   (,pid . ,pred) ,eprops) edge)
                        (db:~name-concepto monarch-lite "DHX30" `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
                        (db:edgeo monarch-lite edge)))))
