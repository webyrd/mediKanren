#lang racket
;; We are using Racket as our base language.

;; Load the mediKanren logic engine, and the Racket-formatted version
;; of a lightly curated and lightly normalized version of the SemMedDB
;; knowledge base.
(require
  "db.rkt"
  "mk-db.rkt"
  "concept.rkt"
  "edge.rkt"
;;  "helpers.rkt"
  )

(displayln
  "Finished loading mk-db.rkt.")

;; find connections between tacrine and alzheimer's disease

;; What concepts related to tacrine and alzheimer's disease does
;; SemMedDB know about?

;; tacrine
;;
;; (39245 "Tacrine" ("orch" "phsu"))
;; seems like the winner
(run* (q) (fuzzy-concepto "tacrine" q))
;; =>
'((39245 "Tacrine" ("orch" "phsu"))
  (99894 "7-methoxytacrine" ("orch" "phsu"))
  (295379 "4-hydroxytacrine" ("orch" "phsu"))
  (295380 "2-hydroxytacrine" ("phsu" "orch"))
  (386973 "6-chlorotacrine" ("orch"))
  (659809 "N-methyltacrine" ("orch"))
  (771182 "Tacrine Hydrochloride" ("phsu" "orch"))
  (1435294 "N-butyramide-tacrine" ("orch")))

;; Same query as above, but restrict to pharmacologic substances
;; ("phsu").
(run* (q)
  (fuzzy-concepto "tacrine" q)
  (fresh (cui name concept-type*)
    (== `(,cui ,name ,concept-type*) q)
    (membero "phsu" concept-type*)))
;; =>
'((39245 "Tacrine" ("orch" "phsu"))
  (99894 "7-methoxytacrine" ("orch" "phsu"))
  (295379 "4-hydroxytacrine" ("orch" "phsu"))
  (295380 "2-hydroxytacrine" ("phsu" "orch"))
  (771182 "Tacrine Hydrochloride" ("phsu" "orch")))


;; alzheimer
;;
;; (2395 "Alzheimer's Disease" ("dsyn"))
;; seems like the winner
(run* (q) (fuzzy-concepto "alzheimer" q))
;; =>
'((2395 "Alzheimer's Disease" ("dsyn"))
  (51532 "Alzheimer's disease antigen" ("aapp"))
  (276496 "Familial Alzheimer's disease" ("dsyn"))
  (333736 "Alzheimer type II glial cell" ("cell"))
  (338450 "Focal Alzheimer's disease" ("dsyn"))
  (494463 "Alzheimer Disease, Late Onset" ("mobd"))
  (528480 "PS2 protein (alzheimer-associated)" ("aapp" "gngm" "bacs"))
  (750901 "Alzheimer Disease, Early Onset" ("dsyn"))
  (949574 "Alzheimer Vaccines" ("phsu" "imft"))
  (1456623 "Alzheimer's Caregivers" ("humn" "popg")))


;; What are the direct connections between Tacrine (a Pharmacologic Substance, or "phsu")
;; and Alzheimer's Disease (a Disease or Syndrome, or "dsyn")?
;;
;; We are using the semantic types for the edges to improve
;; performance, and the CUI's from the "Tacrine" and "Alzheimer's
;; Disease" concepts we discovered above.
;;
;; There are two results, both with TREATS amnd NEG_TREATES predicates.
;; There are many more PubMed entries for TREATS (278) than for NEG_TREATS (2).
(run* (e)
  (fresh (s o predicate pubref)
    (== e `(,s ,o ,predicate "phsu" "dsyn" ,pubref))
    (cuio s 39245) (cuio o 2395) (edgeo e)))
;;
;; which is equivalent to the slightly more verbose
;;
(run* (e)
  (fresh (s o predicate pubref)
    (== e `(,s ,o ,predicate "phsu" "dsyn" ,pubref))
    (== '(39245 "Tacrine" ("orch" "phsu")) s)
    (== '(2395 "Alzheimer's Disease" ("dsyn")) o)
    (edgeo e)))
;; =>
'(((39245 "Tacrine" ("orch" "phsu"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "NEG_TREATS"
   "phsu"
   "dsyn"
   (20371996 1984813))
  ;;
  ((39245 "Tacrine" ("orch" "phsu"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "TREATS"
   "phsu"
   "dsyn"
   (27694908
    27128182
    27128182
    27100474
    27055393
    26985691
    26797191
    26760993
    26043757
    25747977
    25694076
    24706520
    24560791
    24343873
    24283645
    24283645
    24283645
    23931443
    23523257
    23481643
    23256654
    23256654
    23176115
    22302942
    22216416
    22032870
    22000936
    21728972
    21699076
    21472092
    21199749
    21148081
    21143111
    20925094
    20650219
    19714494
    19595868
    19393253
    19041670
    18004213
    17883890
    17636619
    16914883
    16914883
    16880719
    16762377
    16296676
    16296676
    15969312
    15767760
    15203897
    15071608
    15009666
    14519085
    14513664
    12939598
    12860473
    12725862
    12605726
    12568834
    12070846
    11924005
    11924005
    11914957
    11819153
    11819153
    11475012
    11400868
    11125238
    11044776
    11022244
    10956426
    10908463
    10886308
    10886308
    10886308
    10871308
    10840182
    10801254
    10796507
    10755847
    10733605
    10613616
    10559571
    10559571
    10559571
    10557569
    10386551
    10364647
    10364647
    10332935
    10325444
    10325444
    10232064
    10210906
    10172459
    10172130
    10088136
    9923574
    9923574
    9923574
    9923574
    9880090
    9850772
    9824956
    9813462
    9813462
    9813462
    9813282
    9808364
    9803773
    9789247
    9772028
    9771828
    9764962
    9764962
    9764962
    9676739
    9652334
    9579280
    9579280
    9547467
    9521254
    9521254
    9472847
    9472847
    9472847
    9460755
    9438743
    9437436
    9344406
    9344406
    9342788
    9329726
    9329702
    9329702
    9329702
    9280671
    9236571
    9223063
    9170813
    9152571
    9152571
    9125881
    9072868
    9065747
    9062655
    9010644
    9010644
    9010644
    8993489
    8993489
    8973053
    8973053
    8969979
    8962456
    8928167
    8923803
    8909368
    8899701
    8849507
    8845704
    8845704
    8845704
    8838779
    8838779
    8835781
    8804554
    8804553
    8649552
    8649552
    8649552
    8600428
    8548601
    8534422
    8497088
    8437606
    8437605
    8437604
    8437603
    8404544
    8361451
    8358504
    8341950
    8341950
    8341134
    8319751
    8312036
    8139083
    8139061
    8137602
    8128836
    8128835
    8128835
    8128831
    8084981
    8046778
    8046778
    7990234
    7990233
    7990232
    7919566
    7914636
    7907454
    7904023
    7893376
    7893376
    7870908
    7870908
    7868848
    7848488
    7825478
    7727536
    7727536
    7690228
    7690228
    7669991
    7664190
    7653420
    7640148
    7550603
    7517383
    7225483
    3587295
    3185960
    2893967
    2779475
    2754707
    2721578
    2693104
    2693104
    2693104
    2684292
    2594683
    2569126
    2567933
    2557101
    2503129
    2502236
    2497817
    2444444
    2395447
    2348794
    2236461
    2078308
    2053502
    2030370
    1986300
    1984813
    1984813
    1967197
    1953964
    1893185
    1881606
    1818685
    1813659
    1781972
    1763782
    1684233
    1676097
    1675337
    1673209
    1611059
    1572123
    1544011
    1491741
    1491741
    1454836
    1454836
    1454835
    1448787
    1414271
    1406817
    1406817
    1404825
    1404819
    1404819
    1384265)))


;; This variant of the query, using "orch" instead of "phsu" as the
;; semantic type, has AFFECTS and TREATS as predicates on direct
;; edges.
(run* (e)
  (fresh (s o predicate pubref)
    (== e `(,s ,o ,predicate "orch" "dsyn" ,pubref))
    (cuio s 39245) (cuio o 2395) (edgeo e)))
;; =>
'(((39245 "Tacrine" ("orch" "phsu"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "AFFECTS"
   "orch"
   "dsyn"
   (20533758
    17908041
    17908041
    11900820
    9733331
    8090107
    7888087
    7884402
    7710524
    1544011))
  ;;
  ((39245 "Tacrine" ("orch" "phsu"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "TREATS"
   "orch"
   "dsyn"
   (24515838
    22750583
    22192081
    19374459
    17636619
    16544849
    15258105
    11400868
    10886308
    10796507
    9813462
    9305568
    9249126
    8989658
    8496822
    8128831
    7656503
    2789791
    1981736
    1438050)))

;; Indirect connections between Tacrine and Alzheimer's, of the form
;;
;; Tacrine -> INHIBITS -> X -> CAUSES -> Alzheimer's
;;
(run* (e1 e2)
  (fresh (s
          m ;; unknown gene or other entity being inhibited
          o p1 p2 ts t1 t2 r1 r2)
    (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
    (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
    (cuio s 39245) ;; Tacrine
    (cuio o 2395)  ;; Alzheimer's
    (edgeo e1)
    (edgeo e2)
    ))
;; => 47 answers, 215 ms
;;
;; Not all of these answers involve inhibiting genes.
;; For example, Acetylcholine (a neurotransmitter) is inhibited by
;; Tacrine, and causes Alzheimer's (according to SemMedDB)
;;
;; We have duplicate answers, in that we have multiple answers for the same X for which
;;
;; Tacrine -> INHIBITS -> X -> CAUSES -> Alzheimer's
;;
;; This is because the INHIBITS edge Tacrine -> INHIBITS -> X can have multiple
;; semantic types for the same X, provided X is associated with more than one semantic
;; type.  Similarly for the X -> CAUSES -> Alzheimer's edge.
;;
;; For example, consider these two answers:
;;
'(((39245 "Tacrine" ("orch" "phsu"))
   (1041 "Acetylcholine" ("phsu" "nsba" "orch"))
   "INHIBITS"
   "orch"
   "nsba"
   (27876467 9881590 9374189 8842691 8336816))
  ((1041 "Acetylcholine" ("phsu" "nsba" "orch"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "CAUSES"
   "nsba"
   "dsyn"
   (12770689)))
;;
;; and
;;
'(((39245 "Tacrine" ("orch" "phsu"))
   (1041 "Acetylcholine" ("phsu" "nsba" "orch"))
   "INHIBITS"
   "orch"
   "nsba"
   (27876467 9881590 9374189 8842691 8336816))
  ((1041 "Acetylcholine" ("phsu" "nsba" "orch"))
   (2395 "Alzheimer's Disease" ("dsyn"))
   "CAUSES"
   "phsu"
   "dsyn"
   (16863459)))
;;
;; In both cases X is (1041 "Acetylcholine" ("phsu" "nsba" "orch")), but
;; the semantic types for the X -> CAUSES -> Alzheimer's edges differ:
;;
;; nsba|T124|Neuroreactive Substance or Biogenic Amine
;;
;; vs.
;;
;; phsu|T121|Pharmacologic Substance

;; Here is a variant of the query that just returns the entities X
;; being inhibited in the X -> CAUSES -> Alzheimer's path:
;;
(remove-duplicates
  (run* (m)
    (fresh (e1 e2
               s
               o p1 p2 ts t1 t2 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
      (cuio s 39245) ;; Tacrine
      (cuio o 2395)  ;; Alzheimer's
      (edgeo e1)
      (edgeo e2)
      )))
;; => 30 answers, 205 ms
'((1041 "Acetylcholine" ("phsu" "nsba" "orch"))
  (1044 "Acetylcholinesterase" ("gngm" "enzy" "aapp"))
  (2716 "Amyloid" ("bacs" "aapp" "gngm"))
  (6675 "Calcium" ("bacs" "elii"))
  (6685 "Calcium Channel" ("bacs" "gngm" "aapp"))
  (8377 "Cholesterol" ("strd" "bacs"))
  (8425 "Cholinesterase Inhibitors" ("phsu" "orch"))
  (8429 "Cholinesterases" ("enzy" "gngm" "aapp"))
  (17817 "Glutathione" ("bacs" "aapp" "gngm"))
  (21753 "Interleukin-1 beta" ("gngm" "imft" "aapp"))
  (22023 "Ions" ("elii"))
  (26455 "Monoamine Oxidase A" ("aapp" "gngm" "enzy"))
  (28040 "Nicotine" ("phsu" "hops" "orch"))
  (33634 "Protein Kinase C" ("aapp" "enzy" "gngm"))
  (34826 "Muscarinic Acetylcholine Receptor" ("aapp" "gngm" "rcpt"))
  (34830 "Nicotinic Receptors" ("rcpt" "gngm" "aapp"))
  (36442 "Scopolamine" ("orch" "phsu"))
  (36751 "Serotonin" ("nsba" "orch"))
  (72132 "prolyl oligopeptidase" ("enzy" "aapp" "gngm"))
  (79883 "N-Methylaspartate" ("aapp" "phsu" "gngm"))
  (220839 "Glutamate" ("gngm" "bacs" "aapp"))
  (243192 "agonists" ("phsu"))
  (728810 "Butyrylcholinesterase" ("enzy" "aapp" "gngm"))
  (812281 "ROS1 gene" ("bacs" "aapp" "gngm"))
  (965087 "beta-site APP cleaving enzyme 1" ("gngm" "enzy" "aapp"))
  (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
  (1412727 "BACE1 gene" ("aapp" "gngm" "enzy"))
  (1412756 "BCHE gene" ("enzy" "gngm" "aapp"))
  (1413043 "CA2 gene" ("gngm" "enzy" "bacs" "aapp" "rcpt" "phsu"))
  (1422569
   "SPANXC gene"
   ("gngm" "aapp" "enzy" "imft" "phsu" "horm" "bacs" "antb")))


;; Just return the entities X being inhibited in the
;; Tacrine -> INHIBITS -> X -> CAUSES -> Alzheimer's path,
;; where X has the concept type "gngm" (gngm|T028|Gene or Genome)
;; 
(remove-duplicates
  (run* (m)
    (fresh (e1 e2
            s
            o p1 p2 ts t1 t2 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
      (cuio s 39245)  ;; Tacrine
      (cuio o 2395)   ;; Alzheimer's
      (edgeo e1)
      (edgeo e2)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) m)
        (membero "gngm" concept-type*))
      )))
;; => 21 answers, 200 ms
'((1044 "Acetylcholinesterase" ("gngm" "enzy" "aapp"))
  (2716 "Amyloid" ("bacs" "aapp" "gngm"))
  (6685 "Calcium Channel" ("bacs" "gngm" "aapp"))
  (8429 "Cholinesterases" ("enzy" "gngm" "aapp"))
  (17817 "Glutathione" ("bacs" "aapp" "gngm"))
  (21753 "Interleukin-1 beta" ("gngm" "imft" "aapp"))
  (26455 "Monoamine Oxidase A" ("aapp" "gngm" "enzy"))
  (33634 "Protein Kinase C" ("aapp" "enzy" "gngm"))
  (34826 "Muscarinic Acetylcholine Receptor" ("aapp" "gngm" "rcpt"))
  (34830 "Nicotinic Receptors" ("rcpt" "gngm" "aapp"))
  (72132 "prolyl oligopeptidase" ("enzy" "aapp" "gngm"))
  (79883 "N-Methylaspartate" ("aapp" "phsu" "gngm"))
  (220839 "Glutamate" ("gngm" "bacs" "aapp"))
  (728810 "Butyrylcholinesterase" ("enzy" "aapp" "gngm"))
  (812281 "ROS1 gene" ("bacs" "aapp" "gngm"))
  (965087 "beta-site APP cleaving enzyme 1" ("gngm" "enzy" "aapp"))
  (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
  (1412727 "BACE1 gene" ("aapp" "gngm" "enzy"))
  (1412756 "BCHE gene" ("enzy" "gngm" "aapp"))
  (1413043 "CA2 gene" ("gngm" "enzy" "bacs" "aapp" "rcpt" "phsu"))
  (1422569
   "SPANXC gene"
   ("gngm" "aapp" "enzy" "imft" "phsu" "horm" "bacs" "antb")))

;; Just return the entities X being inhibited in the
;; <fuzzy 'tacrine'> -> INHIBITS -> X -> CAUSES -> <fuzzy 'alzheimer'>
;; path, where X has the concept type "gngm"
;; (gngm|T028|Gene or Genome)
;; and where <fuzzy 'tacrine'> concepts must have the "phsu" concept type
;; associated with them,
;; and where <fuzzy 'alzheimer'> concepts must have the "dsyn" concept type
;; associated with them.
;;
;; Interestingly, the same 21 answers are returned as when we use
;; the specific CUIs
;;
;;      (cuio s 39245)  ;; Tacrine
;;      (cuio o 2395)   ;; Alzheimer's
;;
;; Keep in mind that there are only 21 X's, but more than 21 paths from
;; <fuzzy 'tacrine'> -> INHIBITS -> X -> CAUSES -> <fuzzy 'alzheimer'>
;; since sometimes there are multiple direct edges between the same concepts,
;; with different semantic types (and PubMed Ids).
;;
;; Note that requiring a concept has a concept type associated with it is *less*
;; specific than requiring an edge of the path have that type.
(remove-duplicates
  (run* (m)
    (fresh (e1 e2
            s
            o p1 p2 ts t1 t2 t3 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 ,t3 ,r2))
      (fuzzy-concepto "tacrine" s)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) s)
        (membero "phsu" concept-type*))
      (fuzzy-concepto "alzheimer" o)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) o)
        (membero "dsyn" concept-type*))
      (edgeo e1)
      (edgeo e2)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) m)
        (membero "gngm" concept-type*))
      )))
;; => 21 answers, 935 ms
'((1044 "Acetylcholinesterase" ("gngm" "enzy" "aapp"))
  (2716 "Amyloid" ("bacs" "aapp" "gngm"))
  (6685 "Calcium Channel" ("bacs" "gngm" "aapp"))
  (8429 "Cholinesterases" ("enzy" "gngm" "aapp"))
  (17817 "Glutathione" ("bacs" "aapp" "gngm"))
  (21753 "Interleukin-1 beta" ("gngm" "imft" "aapp"))
  (26455 "Monoamine Oxidase A" ("aapp" "gngm" "enzy"))
  (33634 "Protein Kinase C" ("aapp" "enzy" "gngm"))
  (34826 "Muscarinic Acetylcholine Receptor" ("aapp" "gngm" "rcpt"))
  (34830 "Nicotinic Receptors" ("rcpt" "gngm" "aapp"))
  (72132 "prolyl oligopeptidase" ("enzy" "aapp" "gngm"))
  (79883 "N-Methylaspartate" ("aapp" "phsu" "gngm"))
  (220839 "Glutamate" ("gngm" "bacs" "aapp"))
  (728810 "Butyrylcholinesterase" ("enzy" "aapp" "gngm"))
  (812281 "ROS1 gene" ("bacs" "aapp" "gngm"))
  (965087 "beta-site APP cleaving enzyme 1" ("gngm" "enzy" "aapp"))
  (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
  (1412727 "BACE1 gene" ("aapp" "gngm" "enzy"))
  (1412756 "BCHE gene" ("enzy" "gngm" "aapp"))
  (1413043 "CA2 gene" ("gngm" "enzy" "bacs" "aapp" "rcpt" "phsu"))
  (1422569
   "SPANXC gene"
   ("gngm" "aapp" "enzy" "imft" "phsu" "horm" "bacs" "antb")))

;; "Canned" query schema.
;;
;; Return the entities X being inhibited in the
;; <fuzzy-drug-name> -> INHIBITS -> X -> CAUSES -> <fuzzy-disease-name>
;; path, where X has the concept type "gngm"
;; (gngm|T028|Gene or Genome)
;; and where <fuzzy-drug-name> concepts must have the "phsu" concept type
;; associated with them,
;; and where <fuzzy-disease-name> concepts must have the "dsyn" concept type
;; associated with them.
(define (find-inhibited-genes fuzzy-drug-name fuzzy-disease-name)
  (remove-duplicates
   (run* (m)
     (fresh (e1 e2
             s
             o p1 p2 ts t1 t2 t3 r1 r2)
       (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
       (== e2 `(,m ,o "CAUSES" ,t2 ,t3 ,r2))
       (fuzzy-concepto fuzzy-drug-name s)
       (fresh (cui name concept-type*)
         (== `(,cui ,name ,concept-type*) s)
         (membero "phsu" concept-type*))
       (fuzzy-concepto fuzzy-disease-name o)
       (fresh (cui name concept-type*)
         (== `(,cui ,name ,concept-type*) o)
         (membero "dsyn" concept-type*))
       (edgeo e1)
       (edgeo e2)
       (fresh (cui name concept-type*)
         (== `(,cui ,name ,concept-type*) m)
         (membero "gngm" concept-type*))
       ))))

(find-inhibited-genes "tacrine" "alzheimer")
;; => 21 answers, 900 ms


;; variants of the basic query above


;; Use full concept entries, rather than CUIs, for Tacrine and Alzheimer's Disease.
;; This is equivalent to using the CUIs, but is more verbose and easier for humans to read.
(run* (e1 e2)
  (fresh (s
          m ;; unknown gene or other entity being inhibited
          o p1 p2 ts t1 t2 r1 r2)
    (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
    (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
    (== '(39245 "Tacrine" ("orch" "phsu")) s)
    (== '(2395 "Alzheimer's Disease" ("dsyn")) o)
    (edgeo e1)
    (edgeo e2)
    ))
;; => 47 answers, 215 ms


;; Instead of using one specific concept each for tacrine and for
;; alzheimer's, do fuzzy concept searches.
;;
;; This query is a bit sloppy, since we are not removing duplicate
;; answers, and are also not using concept types to ensure tacrine is
;; a pharmacologic substance.
;;
;; As a result, the query may produce duplicate answers, and answers
;; in which tacrine is not restricted to pharmacologic substances.
(run* (e1 e2)
  (fresh (s
          m ;; unknown gene or other entity being inhibited
          o p1 p2 ts t1 t2 r1 r2)
    (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
    (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
    (fuzzy-concepto "tacrine" s)
    (fuzzy-concepto "alzheimer" o)
    (edgeo e1)
    (edgeo e2)
    ))
;; => 57 answers, 2 seconds


;; As above, but remove duplicate answers.
;;
;; Turns out there aren't duplicates for this query.
(remove-duplicates
  (run* (e1 e2)
    (fresh (s
            m ;; unknown gene or other entity being inhibited
            o p1 p2 ts t1 t2 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
      (fuzzy-concepto "tacrine" s)
      (fuzzy-concepto "alzheimer" o)
      (edgeo e1)
      (edgeo e2)
      )))
;; => 57 answers, 2 seconds


;; As above, but remove duplicate answers, and also restrict tacrine
;; to pharmacologic substances.
(remove-duplicates
  (run* (e1 e2)
    (fresh (s
            m ;; unknown gene or other entity being inhibited
            o p1 p2 ts t1 t2 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
      (fuzzy-concepto "tacrine" s)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) s)
        (membero "phsu" concept-type*))
      (fuzzy-concepto "alzheimer" o)      
      (edgeo e1)
      (edgeo e2)
      )))
;; => 55 answers, 2 seconds

;; Same query as above, but with the fuzzy alzheimer's goal moved last
;; to improve performance.
(remove-duplicates
  (run* (e1 e2)
    (fresh (s
            m ;; unknown gene or other entity being inhibited
            o p1 p2 ts t1 t2 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
      (fuzzy-concepto "tacrine" s)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) s)
        (membero "phsu" concept-type*))      
      (edgeo e1)      
      (edgeo e2)
      (fuzzy-concepto "alzheimer" o)
      )))
;; => 55 answers, 425 ms

;; Slightly different query: here we allow any fuzzy variant of
;; alzheimer's that has semantic type "dsyn" associated with it (as
;; opposed to requiring the CAUSES edge have type "dsyn").
(remove-duplicates
  (run* (e1 e2)
    (fresh (s
            m ;; unknown gene or other entity being inhibited
            o p1 p2 ts t1 t2 t3 r1 r2)
      (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
      (== e2 `(,m ,o "CAUSES" ,t2 ,t3 ,r2))
      (fuzzy-concepto "tacrine" s)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) s)
        (membero "phsu" concept-type*))
      (fuzzy-concepto "alzheimer" o)
      (fresh (cui name concept-type*)
        (== `(,cui ,name ,concept-type*) o)
        (membero "dsyn" concept-type*))
      (edgeo e1)
      (edgeo e2)
      )))
;; => 55 answers, 915 ms
