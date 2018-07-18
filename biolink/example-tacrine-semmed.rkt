#lang racket
(require
  "mk-db.rkt"
  )

(define semmed (make-db "data/semmed"))

(displayln "------- tacrine examples, using semmed ---------")


(newline)
(displayln "fuzzy search for tacrine:")
(time (pretty-print (run* (i v) (db:~name-concepto semmed "tacrine" i v))))

(newline)
(displayln "fuzzy search for tacrine, displayed with old format:")
(time (pretty-print (map
                      (lambda (e)
                        (match e
                          [`(,ignore1
                             ((,id
                               ,ignore2
                               ,name
                               ,props)
                              ,ignore3))
                           (let ((type-p (assoc "umls_type_label" props)))
                             (let ((type (if type-p
                                             (cdr type-p)
                                             'no-type-found)))
                               (list id name type)))]))
                      (run* (i v) (db:~name-concepto semmed "tacrine" i v)))))
;; =>
'(("UMLS:C0386973" "6-chlorotacrine" "['Organic Chemical']")
  ("UMLS:C0295380" "2-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0039245" "Tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C1435294" "N-butyramide-tacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0771182" "Tacrine Hydrochloride" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0659809" "N-methyltacrine" "['Organic Chemical']")
  ("UMLS:C0295379" "4-hydroxytacrine" "['Pharmacologic Substance', 'Organic Chemical']")
  ("UMLS:C0099894" "7-methoxytacrine" "['Pharmacologic Substance', 'Organic Chemical']"))
