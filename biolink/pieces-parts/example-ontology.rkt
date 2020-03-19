;; Expected outputs are block-commented after each expression.

;; find ancestors of "substance with phytoestrogen role" in OBO:GOCHE ontology:
(get-all-OBO:GOCHE-ancestors (keep 1 (find-concepts #t (list "OBO:GOCHE_76989"))))

#|
=>

(set
 '(rtx2
   5606341
   "OBO:GOCHE_52210"
   "substance with pharmacological role role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_52210")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_52210")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5088598
   "OBO:GOCHE_24432"
   "substance with biological role role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_24432")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_24432")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   885146
   "OBO:GOCHE_24621"
   "substance with hormone role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_24621")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_24621")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5610174
   "OBO:GOCHE_33280"
   "substance with molecular messenger role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_33280")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_33280")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5611882
   "OBO:GOCHE_50112"
   "substance with sex hormone role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_50112")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_50112")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5611724
   "OBO:GOCHE_50906"
   "substance with role role"
   (2 . "http://w3id.org/biolink/vocab/NamedThing")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_50906")
    ("synonym" . "[]")
    ("category_label" . "named_thing")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_50906")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5606343
   "OBO:GOCHE_48705"
   "substance with agonist role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_48705")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_48705")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   947710
   "OBO:GOCHE_50114"
   "substance with estrogen role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_50114")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_50114")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]"))))
|#


;; find descendants of "substance with estrogen role" in OBO:GOCHE ontology:
(get-all-OBO:GOCHE-descendants (keep 1 (find-concepts #t (list "OBO:GOCHE_50114"))))

#|
=>

(set
 '(rtx2
   5606174
   "OBO:GOCHE_76989"
   "substance with phytoestrogen role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_76989")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_76989")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]")))
 '(rtx2
   5606175
   "OBO:GOCHE_76988"
   "substance with xenoestrogen role"
   (4 . "http://w3id.org/biolink/vocab/ChemicalSubstance")
   (("iri" . "http://purl.obolibrary.org/obo/GOCHE_76988")
    ("synonym" . "[]")
    ("category_label" . "chemical_substance")
    ("deprecated" . "False")
    ("provided_by"
     .
     "http://purl.obolibrary.org/obo/go/extensions/go-plus.owl")
    ("id" . "OBO:GOCHE_76988")
    ("update_date" . "2019-08-02 18:27:55 GMT")
    ("publications" . "[]"))))
|#



;; Examples finding the fixed-point of ancestor concepts in GO
;; ontology.  (GO has multiple inheritance, which makes finding the
;; fixed-point important.)

;; Lymphangiogenesis
(define S1 (keep 1 (find-concepts #t (list "GO:0001946"))))
(define A1 (get-all-GO-ancestors S1))
(pretty-print (get-curies/names-from-concepts A1))

#|
=>

'(("GO:0009653" "anatomical structure morphogenesis")
  ("GO:0008150" "biological_process")
  ("GO:0048646" "anatomical structure formation involved in morphogenesis")
  ("GO:0032502" "developmental process"))
|#

;; negative regulation of cell migration involved in sprouting angiogenesis
(define S2 (keep 1 (find-concepts #t (list "GO:0090051"))))
(define A2 (get-all-GO-ancestors S2))
(pretty-print (get-curies/names-from-concepts A2))

#|
=>

'(("GO:0010633" "negative regulation of epithelial cell migration")
  ("GO:0065007" "biological regulation")
  ("GO:0030334" "regulation of cell migration")
  ("GO:0045765" "regulation of angiogenesis")
  ("GO:0048523" "negative regulation of cellular process")
  ("GO:0032879" "regulation of localization")
  ("GO:0016525" "negative regulation of angiogenesis")
  ("GO:0040013" "negative regulation of locomotion")
  ("GO:0050789" "regulation of biological process")
  ("GO:0043537" "negative regulation of blood vessel endothelial cell migration")
  ("GO:0008150" "biological_process")
  ("GO:0030336" "negative regulation of cell migration")
  ("GO:0010632" "regulation of epithelial cell migration")
  ("GO:0051239" "regulation of multicellular organismal process")
  ("GO:2000145" "regulation of cell motility")
  ("GO:1901342" "regulation of vasculature development")
  ("GO:0022603" "regulation of anatomical structure morphogenesis")
  ("GO:0050793" "regulation of developmental process")
  ("GO:0051271" "negative regulation of cellular component movement")
  ("GO:0050794" "regulation of cellular process")
  ("GO:0043535" "regulation of blood vessel endothelial cell migration")
  ("GO:2000026" "regulation of multicellular organismal development")
  ("GO:0010594" "regulation of endothelial cell migration")
  ("GO:0051270" "regulation of cellular component movement")
  ("GO:0048519" "negative regulation of biological process")
  ("GO:0051241" "negative regulation of multicellular organismal process")
  ("GO:1901343" "negative regulation of vasculature development")
  ("GO:2000181" "negative regulation of blood vessel morphogenesis")
  ("GO:1903671" "negative regulation of sprouting angiogenesis")
  ("GO:0051093" "negative regulation of developmental process")
  ("GO:0010596" "negative regulation of endothelial cell migration")
  ("GO:0090049" "regulation of cell migration involved in sprouting angiogenesis")
  ("GO:2000146" "negative regulation of cell motility")
  ("GO:1903670" "regulation of sprouting angiogenesis")
  ("GO:0040012" "regulation of locomotion"))
|#

;; positive regulation of execution phase of apoptosis
(define S3 (keep 1 (find-concepts #t (list "GO:1900119"))))
(define A3 (get-all-GO-ancestors S3))
(pretty-print (get-curies/names-from-concepts A3))

#|
=>

'(("GO:0065007" "biological regulation")
  ("GO:0042981" "regulation of apoptotic process")
  ("GO:1900117" "regulation of execution phase of apoptosis")
  ("GO:0050789" "regulation of biological process")
  ("GO:0043067" "regulation of programmed cell death")
  ("GO:0008150" "biological_process")
  ("GO:0050794" "regulation of cellular process")
  ("GO:0048522" "positive regulation of cellular process")
  ("GO:0043065" "positive regulation of apoptotic process")
  ("GO:0010942" "positive regulation of cell death")
  ("GO:0048518" "positive regulation of biological process")
  ("GO:0010941" "regulation of cell death")
  ("GO:0043068" "positive regulation of programmed cell death"))
|#

;; positive regulation of cysteine-type endopeptidase activity involved in execution phase of apoptosis
(define S4 (keep 1 (find-concepts #t (list "GO:2001272"))))
(define A4 (get-all-GO-ancestors S4))
(pretty-print (get-curies/names-from-concepts A4))

#|
=>

'(("GO:0065007" "biological regulation")
  ("GO:0042981" "regulation of apoptotic process")
  ("GO:0044093" "positive regulation of molecular function")
  ("GO:0009893" "positive regulation of metabolic process")
  ("GO:0019222" "regulation of metabolic process")
  ("GO:0052547" "regulation of peptidase activity")
  ("GO:0045862" "positive regulation of proteolysis")
  ("GO:0051336" "regulation of hydrolase activity")
  ("GO:0032270" "positive regulation of cellular protein metabolic process")
  ("GO:1900117" "regulation of execution phase of apoptosis")
  ("GO:2001056" "positive regulation of cysteine-type endopeptidase activity")
  ("GO:0051171" "regulation of nitrogen compound metabolic process")
  ("GO:0060255" "regulation of macromolecule metabolic process")
  ("GO:0010950" "positive regulation of endopeptidase activity")
  ("GO:0050789" "regulation of biological process")
  ("GO:0043067" "regulation of programmed cell death")
  ("GO:0008150" "biological_process")
  ("GO:0010604" "positive regulation of macromolecule metabolic process")
  ("GO:0052548" "regulation of endopeptidase activity")
  ("GO:0050794" "regulation of cellular process")
  ("GO:0048522" "positive regulation of cellular process")
  ("GO:0051247" "positive regulation of protein metabolic process")
  ("GO:0043065" "positive regulation of apoptotic process")
  ("GO:0010942" "positive regulation of cell death")
  ("GO:0010952" "positive regulation of peptidase activity")
  ("GO:0031323" "regulation of cellular metabolic process")
  ("GO:2000116" "regulation of cysteine-type endopeptidase activity")
  ("GO:0051173" "positive regulation of nitrogen compound metabolic process")
  ("GO:0048518" "positive regulation of biological process")
  ("GO:0043085" "positive regulation of catalytic activity")
  ("GO:0050790" "regulation of catalytic activity")
  ("GO:0051246" "regulation of protein metabolic process")
  ("GO:0051345" "positive regulation of hydrolase activity")
  ("GO:0032268" "regulation of cellular protein metabolic process")
  ("GO:0043280" "positive regulation of cysteine-type endopeptidase activity involved in apoptotic process")
  ("GO:0010941" "regulation of cell death")
  ("GO:2001270" "regulation of cysteine-type endopeptidase activity involved in execution phase of apoptosis")
  ("GO:0043281" "regulation of cysteine-type endopeptidase activity involved in apoptotic process")
  ("GO:0065009" "regulation of molecular function")
  ("GO:0031325" "positive regulation of cellular metabolic process")
  ("GO:1900119" "positive regulation of execution phase of apoptosis")
  ("GO:0080090" "regulation of primary metabolic process")
  ("GO:0030162" "regulation of proteolysis")
  ("GO:0043068" "positive regulation of programmed cell death"))
|#

;; cysteine-type endopeptidase activity involved in execution phase of apoptosis
(define S5 (keep 1 (find-concepts #t (list "GO:0097200"))))
(define A5 (get-all-GO-ancestors S5))
(pretty-print (get-curies/names-from-concepts A5))

#|
=>

'(("GO:0097153"
   "cysteine-type endopeptidase activity involved in apoptotic process"))
|#

;; activation of JUN kinase activity
(define S6 (keep 1 (find-concepts #t (list "GO:0007257"))))
(define A6 (get-all-GO-ancestors S6))
(pretty-print (get-curies/names-from-concepts A6))

#|
=>

'(("GO:0065007" "biological regulation")
  ("GO:0043405" "regulation of MAP kinase activity")
  ("GO:0044093" "positive regulation of molecular function")
  ("GO:0023056" "positive regulation of signaling")
  ("GO:0009893" "positive regulation of metabolic process")
  ("GO:0019222" "regulation of metabolic process")
  ("GO:0071900" "regulation of protein serine/threonine kinase activity")
  ("GO:0032872" "regulation of stress-activated MAPK cascade")
  ("GO:0080134" "regulation of response to stress")
  ("GO:0032270" "positive regulation of cellular protein metabolic process")
  ("GO:0045937" "positive regulation of phosphate metabolic process")
  ("GO:0070304" "positive regulation of stress-activated protein kinase signaling cascade")
  ("GO:0080135" "regulation of cellular response to stress")
  ("GO:0001934" "positive regulation of protein phosphorylation")
  ("GO:0051171" "regulation of nitrogen compound metabolic process")
  ("GO:0060255" "regulation of macromolecule metabolic process")
  ("GO:0050789" "regulation of biological process")
  ("GO:0008150" "biological_process")
  ("GO:0046328" "regulation of JNK cascade")
  ("GO:0071902" "positive regulation of protein serine/threonine kinase activity")
  ("GO:0031399" "regulation of protein modification process")
  ("GO:0048583" "regulation of response to stimulus")
  ("GO:0070302" "regulation of stress-activated protein kinase signaling cascade")
  ("GO:0010604" "positive regulation of macromolecule metabolic process")
  ("GO:0051338" "regulation of transferase activity")
  ("GO:0001932" "regulation of protein phosphorylation")
  ("GO:0010562" "positive regulation of phosphorus metabolic process")
  ("GO:0051174" "regulation of phosphorus metabolic process")
  ("GO:0050794" "regulation of cellular process")
  ("GO:0048522" "positive regulation of cellular process")
  ("GO:0009966" "regulation of signal transduction")
  ("GO:0023051" "regulation of signaling")
  ("GO:0051347" "positive regulation of transferase activity")
  ("GO:0043549" "regulation of kinase activity")
  ("GO:0043410" "positive regulation of MAPK cascade")
  ("GO:0043506" "regulation of JUN kinase activity")
  ("GO:0000187" "activation of MAPK activity")
  ("GO:0051247" "positive regulation of protein metabolic process")
  ("GO:0045859" "regulation of protein kinase activity")
  ("GO:0042325" "regulation of phosphorylation")
  ("GO:1902531" "regulation of intracellular signal transduction")
  ("GO:0033674" "positive regulation of kinase activity")
  ("GO:0032147" "activation of protein kinase activity")
  ("GO:0031323" "regulation of cellular metabolic process")
  ("GO:0043507" "positive regulation of JUN kinase activity")
  ("GO:0051173" "positive regulation of nitrogen compound metabolic process")
  ("GO:1902533" "positive regulation of intracellular signal transduction")
  ("GO:0048518" "positive regulation of biological process")
  ("GO:0043085" "positive regulation of catalytic activity")
  ("GO:0048584" "positive regulation of response to stimulus")
  ("GO:0050790" "regulation of catalytic activity")
  ("GO:0051246" "regulation of protein metabolic process")
  ("GO:0046330" "positive regulation of JNK cascade")
  ("GO:0032268" "regulation of cellular protein metabolic process")
  ("GO:0043408" "regulation of MAPK cascade")
  ("GO:0010647" "positive regulation of cell communication")
  ("GO:0031401" "positive regulation of protein modification process")
  ("GO:0043406" "positive regulation of MAP kinase activity")
  ("GO:0032874" "positive regulation of stress-activated MAPK cascade")
  ("GO:0065009" "regulation of molecular function")
  ("GO:0031325" "positive regulation of cellular metabolic process")
  ("GO:0042327" "positive regulation of phosphorylation")
  ("GO:0009967" "positive regulation of signal transduction")
  ("GO:0010646" "regulation of cell communication")
  ("GO:0080090" "regulation of primary metabolic process")
  ("GO:0045860" "positive regulation of protein kinase activity")
  ("GO:0019220" "regulation of phosphate metabolic process"))
|#
