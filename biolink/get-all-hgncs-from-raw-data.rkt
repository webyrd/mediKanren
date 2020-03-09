;; rtx2 contains a bunch of weird entries of the form:
;;
;; "HGNC:24039/HGNC:<some number>"
;;
;; "HGNC:24039/HGNC:877"
;; "HGNC:24039/HGNC:2602"
;; "HGNC:24039/HGNC:470"
;;
;; These are presumably errors.
;;
;; TODO:  update the code to ensure these bogus CURIEs aren't added
;;
;; TODO:  update the code to ensure CURIEs are written in increasing numeric order

(define get-hgncs-for-concept-file-path
  (lambda (concept-file-path)
    (let ((ip (open-input-file concept-file-path)))
      (let loop ([cid 0]
                 [data (read ip)]
                 [s (set)])
        (cond
          [(eof-object? data)
           (close-input-port ip)
           (printf "read ~s entries\n" cid)
           s]
          [else
           (when (= (modulo cid 10000) 0)
             (printf "cid: ~s\n" cid))
           (let ((s
                  (let ((curie (vector-ref data 0)))
                    (if (string-prefix? curie "HGNC:")
                        (set-add s curie)
                        s))))
             (loop (add1 cid) (read ip) s))])))))

(define (save-hgncs-to-file! s hgnc-file-path)
  (define op (open-output-file hgnc-file-path #:exists 'replace))
  (for-each (lambda (str) (write str op) (newline op)) (set->list s))
  (close-output-port op))


(define robokop-hgncs
  (get-hgncs-for-concept-file-path "data/robokop/concepts.scm"))

(define rtx2-hgncs
  (get-hgncs-for-concept-file-path "data/rtx2/concepts.scm"))

(define all-hgns
  (set-union robokop-hgncs
             rtx2-hgncs))

(save-hgncs-to-file! all-hgns "./all-hgnc-curies.rkt")
