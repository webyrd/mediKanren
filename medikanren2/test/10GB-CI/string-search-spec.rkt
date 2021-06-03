#lang racket
;(require racket)
;(require racket/match)
(require chk)
(require "../../common.rkt")
(require "../../profiling.rkt")
(require "../../string-search-config.rkt")
(require "../../string-search-impl.rkt")
(require "../../string-search-impl2.rkt")
(require "../../string-search.rkt")
(require racket/pretty)


;; *** Utilities ***

(define (with-finally thunk-run thunk-cleanup)
  (with-handlers
      ((exn:fail? (lambda (ex)
        (thunk-cleanup)
        (raise ex))))
    (let ((x (thunk-run)))
      (thunk-cleanup)
      x)))

(chk
 (#:=
  (with-finally
    (lambda () 7)
    (lambda () 'no-opp))
  7))

(chk
 (#:do (define t 1))
 (#:do
  (with-finally
    (lambda () 8)
    (lambda () (set! t 2))))
 (#:= t 2))

(chk
 (#:do (define t 1))
 (#:do
  (with-handlers ([exn? (lambda (ex)
                   'no-op)]) ; "swallow" exception
    (with-finally
      (lambda () (error "foobaaar!!"))
      (lambda () (set! t 2)))))
 (#:= t 2))


(define (with-temporary-dir-cleanup template fn)
  (define adirTemp (make-temporary-file template 'directory #f))
  (with-finally
    (lambda ()
      (test:parameterize-defaults (lambda ()
        (fn adirTemp))))
    (lambda ()
      (with-handlers ([exn:fail?
                       'no-op]) ; "swallow" exception
        (delete-directory/files adirTemp)))))

;; No cleanup, in case manual inspection is required
(define (with-temporary-dir template fn)
  (define adirTemp (make-temporary-file template 'directory #f))
  (printf "populating ~a\n" adirTemp)
  (test:parameterize-defaults (lambda ()
    (fn adirTemp))))
  
;; We count both work ticks (w) and results (i).  Make a version
;; parametreized by numTicksMax?
(define (run-stream numMax s)
  (define (iter i w xs s)
    (cond
      ((and (>= numMax 0) (>= i numMax)) xs)
      ((null? s) xs)
      ((procedure? s)
       (iter i (+ w 1) xs (s)))
      ((pair? s)
       (let* ((x (car s))
              (t (current-milliseconds))
              )
         (iter (+ i 1) (+ w 1) (cons x xs) ((cdr s)))))
      (else (raise (format "unexpected type: ~a" s)))))
  (reverse (iter 0 0 '() s)))


(define (run-query numMax q)
  (run-stream numMax (query->stream q)))

(chk
  (#:do
    (for ((xp (range 0 46)))
      (let* ((foffs (arithmetic-shift 1 xp))
              (foffs2 (car (bytes->suffix-key (suffix-key->bytes (cons foffs 7)) 0))))
        (unless (== foffs foffs2)
          (error (format "could not round trip file offset ~a" foffs))))))
  (#:t #t))

(chk
  (#:do
    (for ((xp (range 0 46)))
      (let* ((foffs (arithmetic-shift 1 xp))
              (foffs2 (test:bytes->string-key (test:string-key->bytes foffs))))
        (unless (== foffs foffs2)
          (error (format "could not round trip file offset ~a" foffs))))))
  (#:t #t))

(chk
  (#:do
    (for ((xp (range 0 14)))
      (let* ((soffs (arithmetic-shift 1 xp))
              (soffs2 (car (bytes->suffix-key (suffix-key->bytes (cons 7 soffs)) 0))))
        (unless (== soffs soffs2)
          (error (format "could not round trip string offset ~a" soffs))))))
  (#:t #t))

;; Originally, we thought that a query could be sufficient for populating a 
;; string-search index.  We then realized that in order to record file offsets,
;; the name data had to be collected from a file encoded with codec.rkt.  
;; Ever since then, we have abandoned the idea of collecting string data
;; from a query.

(define (query-names-from-rel rel)
  (query (id name)
         (rel id "name" name)))

(define (query-names-from-id-rel id rel)
  (query (name)
         (rel id "name" name)))

(define uri-ion-transport "GO:0034765")

;; Can we find one particular name?
(chk
 (#:=
  (run-query 10 (query-names-from-id-rel uri-ion-transport cprop))
  '(("regulation of ion transmembrane transport"))))


;; Can we find 10 names without crashing?
(chk
 (#:=
  (length (run-query 10
            (query-names-from-rel cprop)))
  10))



;; generalize suffix:corpus->index

;; vector-sparse-find

#|
(list->vector
 (reverse
  (foldl
   (lambda (xn yn)
     (let ((n (+ (cdr xn) (cdar yn)))
           (y (car xn)))
       (cons (cons y n) yn)))
   '(("" . 0))
   (map (lambda (x) (cons x (+ 1 (random 10)))) (string-split "now is the time for all good men to come to the aid of their country")))))
|#

(define vsf-example-1
  '#(("" . 0) ("now" . 7) ("is" . 9) ("the" . 18) ("time" . 24) ("for" . 25) ("all" . 31) ("good" . 39) ("men" . 44) ("to" . 52) ("come" . 56) ("to" . 65) ("the" . 73) ("aid" . 80) ("of" . 81) ("their" . 88) ("country" . 95)))

(chk
 (#:do
  (for ((i (range (vector-length vsf-example-1))))
    (let* ((s-n (vector-ref vsf-example-1 i))
           (s-expected (car s-n))
           (n (cdr s-n))
           (s (vector-sparse-find vsf-example-1 n)))
      (unless (equal? s s-expected)
        (error (format "vector-sparse-find i=~a n=~a s=~a s-expected=~a" i n s s-expected))))))
  (#:t #t))



;; suffix:corpus->index: status quo
(chk
 (#:=
  (suffix:corpus->index
   (vector
    "the"))
  '#((0 . 2) (0 . 1) (0 . 0))))

(chk
 (#:=
  (suffix:corpus->index
   (vector
    "the"
    "the"))
  '#((1 . 2) (0 . 2) (1 . 1) (0 . 1) (1 . 0) (0 . 0))
  ))

(chk
 (#:=
  (suffix:corpus->index
   (vector
    "tzzhze"))
  '#((0 . 5) (0 . 3) (0 . 0) (0 . 4) (0 . 2) (0 . 1))
  ))

(chk
 (#:=
  (suffix:corpus->index
   (vector
    "the"
    "tzzhze"))
  '#((1 . 5) (0 . 2) (0 . 1) (1 . 3) (0 . 0) (1 . 0) (1 . 4) (1 . 2) (1 . 1))
  ))

(chk
 (#:=
  (suffix:corpus->index
   (vector
    "tzzhze"
    "the"
    ))
  '#((1 . 2) (0 . 5) (1 . 1) (0 . 3) (1 . 0) (0 . 0) (0 . 4) (0 . 2) (0 . 1))
  ))

;; suffix:corpus->index-suffixes
(chk
 (#:=
  (test:suffix:corpus->index-suffixes
   (vector
    "the"
    "the"))
  '((1 . 2) (1 . 1) (1 . 0) (0 . 2) (0 . 1) (0 . 0))
  ))

(chk
 (#:=
  (list->set (map (lambda (b) (bytes->suffix-key b 0)) (vector->list (test:suffix:corpus2->index-suffixes
   (vector
    (cons "the" 7)
    (cons "the" 9)
    )))))
  (list->set '((7 . 0) (7 . 1) (7 . 2) (9 . 0) (9 . 1) (9 . 2)))))

(chk
 (#:=
  (list->set (map (lambda (b) (bytes->suffix-key b 0)) (vector->list (test:suffix:corpus2->index-suffixes
   (vector
    (cons "the" 7))))))
  (list->set '((7 . 2) (7 . 1) (7 . 0)))))


(chk
 (#:=
  (list->set (map (lambda (b) (bytes->suffix-key b 0)) (vector->list (test:suffix:corpus2->index-suffixes
   (vector
    (cons "the" 7)
    (cons "the" 9)
    )))))
  (list->set '((7 . 2) (9 . 2) (7 . 1) (9 . 1) (7 . 0) (9 . 0)))
  ))


;; Can we pass names to the index?

(define concept*-example-1
  (list->vector
   '((("OMIM:MTHU026069" "\"'Swiss-cheese\\\" appearance of cartilage\"")
   "cartilage")
 (("CHEMBL.COMPOUND:CHEMBL2271031"
   "\"(+/-)-[1S*,2R*,5R*,6S*,(2\\\\\\\"S*,3\\\\\\\"R*)/(2\\\\\\\"R*,3\\\\\\\"S*)]-2-(2',6'-dimethoxyphenoxy)-1-hydroxy-6-[7\\\\\\\"-methoxy-3\\\\\\\"-methoxymethyl-2\\\\\\\"-(3,4-methylenedioxyphenyl)chroman-6\\\\\\\"-yl]-3,7-dioxabicyclo[3.3.0]octane\"")
  "methoxy-3")
 (("CHEBI:139029"
   "\"(1S,3R)-dihydroxy-(20S)-[(5'R)-(2\\\"-hydroxy-2\\\"-propyl)-(2'S)-tetrahydrofuryl]-22,23,24,25,26,27-hexanor-1alpha-hydroxyvitamin D3\"")
  "hexanor")
 (("CHEMBL.COMPOUND:CHEMBL2426108"
   "\"(3R',5S')-3,5-dihydroxy-1-(4'-hydroxy-3',5'-dimethoxyphenyl)-7-(4\\\\\\\"-hydroxy-3'-methoxyphenyl)heptane\"")
  "hydroxy-3'-m")
 (("CHEMBL.COMPOUND:CHEMBL2426107"
   "\"(3R',5S')-3,5-dihydroxy-1-(4'-hydroxy-3',5'-dimethoxyphenyl)-7-(4\\\\\\\"-hydroxyphenyl)heptane\"")
  "hydroxyphenyl)heptane")
 (("CHEMBL.COMPOUND:CHEMBL2426106"
   "\"(3S)-1-(3'-methoxy-4'-hydroxyphenyl)-7-(4\\\\\\\"-hydroxyphenyl)-(6E)-6-hepten-3-ol\"")
  "hepten")
 (("CHEMBL.COMPOUND:CHEMBL3735273"
   "\"(Clarithromycin-(4\\\\\\\"-(methylamino)-N(methyl)(4-benzyltriazolyl))-N-hydroxyacetamide\"")
  "hydroxya"))))



(define (build-test-corpus concept* absdOut)
  (let ((fd (open-output-file (expand-user-path (build-path absdOut fn-cprop-primary)))))
    (for/vector ((c concept*))
      (let ((id (car c))
            (name (cadr c)))
        (encode fd schema-pri `(,id ,test:field-indexed ,name))))
    (close-output-port fd)))

(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (build-test-corpus (vector-map car concept*-example-1) adirTemp))))
 (#:t #t))


(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (test:build-name-string-search-v1 (vector-map car concept*-example-1) adirTemp))))
 (#:t #t))
 
;; can we build an index in a format that returns file offsets?
(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let ((fn-primary (expand-user-path (build-path adirTemp fn-cprop-primary))))
        (build-test-corpus (vector-map car concept*-example-1) adirTemp)
        (build-name-string-search-via-codec fn-cprop-primary fn-concept-name-index adirTemp)))))
 (#:t #t))


;; Test vector-sparse-find using data decoded from a temporary file.
(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let ((fn-primary (expand-user-path (build-path adirTemp fn-cprop-primary)))
            (without-expected (vector-map car concept*-example-1)))
        (build-test-corpus without-expected adirTemp)
        (let ((name-corpus (test:read-name-corpus fn-cprop-primary adirTemp)))
          (for ((i (range (vector-length name-corpus))))
            (let* ((s-n (vector-ref name-corpus i))
                  (s-expected (car s-n))
                  (n (cdr s-n))
                  (s (vector-sparse-find name-corpus n)))
              (unless (equal? s s-expected)
                (error (format "vector-sparse-find i=~a n=~a s=~a s-expected=~a" i n s s-expected))))))))))
  (#:do (writeln "name-corpus OK"))
  (#:t #t))


(define (print-index-v1 corpus1 adirTemp)
  (define (string-left str end)
    (substring str 0 (min (string-length str) end)))
  (let* ((fn-index (expand-user-path (build-path adirTemp fn-concept-name-index)))
         (fd-index (open-input-file fn-index #:mode 'binary)))
    (for ((i (range (test:suffix-key-count/port fd-index))))
      (let* ((s-n (test:suffix-index->suffix-key fd-index i)))
        (let* ((foffs (car s-n))
               (soffs (cdr s-n))
               (concept (vector-ref corpus1 foffs))
               (name concept)
               (st-hit (substring (string/searchable name) soffs))
               (st-hit-short (string-left st-hit 30)))
          (printf "~a (~a,~a) ~a len=~a \n" i foffs soffs st-hit-short (string-length st-hit) ))))
    (close-input-port fd-index)))

;;; print-index-v2: print a human-readable form of the index for debugging purposes
(define (print-index-v2 index-v2 adirTemp)
  (define (string-left str end)
    (substring str 0 (min (string-length str) end)))
  (let* ((fn-primary (expand-user-path (build-path adirTemp fn-cprop-primary)))
         (fd-primary (open-input-file fn-primary #:mode 'binary)))
    (for ((i (range (vector-length index-v2))))
      (let* ((s-n (vector-ref index-v2 i)))
          (let* ((foffs (car s-n))
                 (soffs (cdr s-n))
                 (concept (test:foffs->concept fd-primary foffs))
                 (id (list-ref concept 0))
                 (name (list-ref concept 2))
                 (st-hit (substring (string/searchable name) soffs))
                 (st-hit-short (string-left st-hit 30)))
            (printf "~a (~a,~a) ~a len=~a id=~a\n" i foffs soffs st-hit-short (string-length st-hit) id))))
    (close-input-port fd-primary)))


(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (test:build-name-string-search-v1 (vector-map car concept*-example-1) adirTemp)
      (let* ((corpus1 (vector-map  cadar concept*-example-1))
             (index (suffix:corpus->index corpus1)))
;        (printf "about to print index from corpus v1:\n")
;        (print-index-v1 corpus1 adirTemp)
        '()
        ))))
 (#:t #t))

;; Can we the sort an index without crashing?  If we print the index does it look right?
(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let* ((corpus2 #(("ANIMALS:0123" "brown fox"))))
        (build-test-corpus corpus2 adirTemp)
        (let-values (((hashcorpus index) (test:build-name-index-via-codec fn-cprop-primary adirTemp)))
          (test:verify-corpus-index hashcorpus index)
;          (printf "about to print-index from corpus v2:\n")
;          (print-index-v2 index adirTemp)
          '()
        )))))
 (#:t #t))

(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let* ((corpus2 #(("JOBS:0123" "bookkeeper"))))
        (build-test-corpus corpus2 adirTemp)
        (let-values (((hashcorpus index) (test:build-name-index-via-codec fn-cprop-primary adirTemp)))
          (test:verify-corpus-index hashcorpus index)
;          (printf "about to print-index from corpus v2:\n")
;          (print-index-v2 index adirTemp)
          '()
        )))))
 (#:t #t))

(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let* ((corpus2 #(("JOBS:0123" "bookkeeper")
                        ("JOBS:0123" "bookbinder"))))
        (build-test-corpus corpus2 adirTemp)
        (let-values (((hashcorpus index) (test:build-name-index-via-codec fn-cprop-primary adirTemp)))
          (test:verify-corpus-index hashcorpus index)
;          (printf "about to print-index from corpus v2:\n")
;          (print-index-v2 index adirTemp)
          '()
        )))))
 (#:t #t))


(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let* ((corpus2 (vector-map car concept*-example-1)))
        (build-test-corpus corpus2 adirTemp)
        (let-values (((hashcorpus index) (test:build-name-index-via-codec fn-cprop-primary adirTemp)))
          (test:verify-corpus-index hashcorpus index)
;          (printf "about to print-index from corpus v2:\n")
;          (print-index-v2 index adirTemp)
          '()
        )))))
 (#:t #t))




;; can we perform lookups in the new index format?
(chk
 (#:do
  (with-temporary-dir-cleanup
    "mediKanren-test-~a"
    (lambda (adirTemp)
      (let ((fn-primary (expand-user-path (build-path adirTemp fn-cprop-primary))))
        (build-test-corpus (vector-map car concept*-example-1) adirTemp)
        (build-name-string-search-via-codec fn-cprop-primary fn-concept-name-index adirTemp)
        (let ((lookup (lambda (name*)
            (map uri-from-pri (db:~name*->concept*/options2 stsopt-default adirTemp fn-cprop-primary fn-concept-name-index name*)))))
          (for ((i (range (vector-length concept*-example-1))))
            (let* ((a-q (vector-ref concept*-example-1 i))
                   (q (cdr a-q))
                   (a (car a-q))
                   (ids-expected (list (car a)))
                   (ids (lookup q)))
              (if (equal? ids ids-expected)
                '() ;(displayln "PASS")
                (displayln (format "FAIL: name*->cid* i=~a ids-expected=~a ids=~a" i ids-expected ids))))))))))
 (#:t #t))

