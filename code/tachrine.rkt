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


;; What are the connections between Tacrine (a Pharmacologic Substance, or "phsu")
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

;; Tacrine ->INHIBITS -> X -> CAUSES -> Alzheimer's 
(run* (e1 e2)
  (fresh (s
          m ;; unknown gene
          o p1 p2 ts t1 t2 r1 r2)
    (== e1 `(,s ,m "INHIBITS" ,ts ,t1 ,r1))
    (== e2 `(,m ,o "CAUSES" ,t2 "dsyn" ,r2))
    (cuio s 39245) ;; Tacrine
    (cuio o 2395)  ;; Alzheimer's
    (edgeo e1)
    (edgeo e2)
    ))
