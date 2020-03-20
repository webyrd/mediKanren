(define directly-upregulate-gene
  (lambda (gene-curie)
    (displayln "\nRunning 1-hop up query with concept categories and drug safety")
    (define q (time (query/graph
                     ((X       drug)
                      (my-gene gene-curie)
                      (T       #f))
                     ((X->my-gene positively-regulates)
                      (X->T       drug-safe))
                     (X X->my-gene my-gene)
                     (X X->T T))))

    (displayln "\nRanking paths")
    (define ranked (time (ranked-paths q)))

    ranked))

;; alms1
(define alms1-directly-up (directly-upregulate-gene "HGNC:428"))
(pretty-ranked (take alms1-directly-up 1))

;; rpgrip1l
(define rpgrip1l-directly-up (directly-upregulate-gene "HGNC:29168"))
(pretty-ranked (take rpgrip1l-directly-up 1))

;; inpp5e
(define inpp5e-directly-up (directly-upregulate-gene "HGNC:21474"))
(pretty-ranked (take inpp5e-directly-up 1))

;; nphp3
(define nphp3-directly-up (directly-upregulate-gene "HGNC:7907"))
(pretty-ranked (take nphp3-directly-up 1))

;; kdm1a
(define kdm1a-directly-up (directly-upregulate-gene "HGNC:29079"))
(pretty-ranked (take kdm1a-directly-up 1))





(define directly-downregulate-gene
  (lambda (gene-curie)
    (displayln "\nRunning 1-hop down query with concept categories and drug safety")
    (define q (time (query/graph
                     ((X       drug)
                      (my-gene gene-curie)
                      (T       #f))
                     ((X->my-gene negatively-regulates)
                      (X->T       drug-safe))
                     (X X->my-gene my-gene)
                     (X X->T T))))

    (displayln "\nRanking paths")
    (define ranked (time (ranked-paths q)))

    ranked))

(define alms1-directly-down (directly-downregulate-gene "HGNC:428"))
(pretty-ranked (take alms1-directly-down 1))

;; rpgrip1l
(define rpgrip1l-directly-down (directly-downregulate-gene "HGNC:29168"))
(pretty-ranked (take rpgrip1l-directly-down 1))

;; inpp5e
(define inpp5e-directly-down (directly-downregulate-gene "HGNC:21474"))
(pretty-ranked (take inpp5e-directly-down 1))

;; nphp3
(define nphp3-directly-down (directly-downregulate-gene "HGNC:7907"))
(pretty-ranked (take nphp3-directly-down 1))

;; kdm1a
(define kdm1a-directly-down (directly-downregulate-gene "HGNC:29079"))
(pretty-ranked (take kdm1a-directly-down 1))





(define report-on-gene
  (lambda (gene-curie)
    (displayln "\nRunning 2-hop down-up query with concept categories and drug safety:")
    (define q (time (query/graph
                  ((X       drug)
                   (Y       gene-or-protein)
                   (my-gene gene-curie)
                   (T       #f))
                  ((X->Y       negatively-regulates)
                   (Y->my-gene positively-regulates)
                   (X->T       drug-safe))
                  (X X->Y Y Y->my-gene my-gene)
                  (X X->T T))))

    (displayln "\nBuilding report:")
    (pretty-print (time (report/query q)))

    (displayln "\nRanking paths:")
    (define ranked (time (ranked-paths q)))

    (pretty-ranked (take ranked 1))))


;; alms1
(report-on-gene "HGNC:428")

;; rpgrip1l
(report-on-gene "HGNC:29168")

;; inpp5e
(report-on-gene "HGNC:21474")

;; nphp3
(report-on-gene "HGNC:7907")

;; kdm1a
(report-on-gene "HGNC:29079")
