#lang racket
(provide (all-defined-out))
(require csv-reading
         racket/hash
         racket/set
         racket/mpair
         racket/runtime-path)

;=================================================== example & demo =================================================

#|
1. load the data from rtx2_2020_09_16-subject_predicate_object_count.tsv
   (define rtx2-data (load-data "rtx2_2020_09_16-subject_predicate_object_count.tsv"))

2. load all required hash tables for finding the shortest path: node-hash edge-hash pred/prefix-pr-hash
   with a level of restriction or number toward the count of predicates conneting between databases
   (define all-hash (load-all-hash rtx2-data 0))

   2.1 You may want to assign names to each loaded hash-table
       (match-define (list node-hash edge-hash pred/prefix-pr-hash) all-hash)

3. find the shortest path(s) with or without showing the connecting predicate(s)
   - without showing predicate: (shortest-path* all-hash start end pred)
   - with showing predicate: (shortest-path*-with-pred all-hash start end pred)
   - arguments end or pred can be #f, indicating an unknown endding node or an unspecified predicate

3.1 shorest path from node-1 to node-2:  

    * find the shortest paths from "CHEBI" to "UMLS"
      (shortest-path* all-hash "CHEBI" "UMLS" #f)
      => (("CHEBI" "DOID" "UMLS"))
    * find the shortest paths from "CHEBI" to "UMLS" with the predicates connecting each pair of nodes(databases)
      (shortest-path*-with-pred all-hash "CHEBI" "UMLS" #f)
      => ((("CHEBI" "DOID" "UMLS"))
          (((("CHEBI" . "DOID") ("INVERTED:has_allergic_trigger"))
            (("DOID" . "UMLS") ("xref")))))

3.2 shortest paths containing predicate pred from node-1 to node-2:

    * find all the ("shortest") paths containing predicate "clinically_tested_terminated_phase_2" from "CHEBI" to "UMLS"
      (shortest-path* all-hash "CHEBI" "UMLS" "clinically_tested_terminated_phase_2")
      => (("CHEBI" "DRUGBANK" "UMLS"))

    * combine the result from the step above and show the predicates connecting each pair of nodes(databases) for the shortest path(s)
      (shortest-path*-with-pred all-hash "CHEBI" "UMLS" "clinically_tested_terminated_phase_2")
      => ((("CHEBI" "DRUGBANK" "UMLS"))
          (((("CHEBI" . "DRUGBANK") ("same_as"))
            (("DRUGBANK" . "CHEBI") ("external_identifier" "same_as"))
            (("DRUGBANK" . "UMLS")
            ("clinically_tested_suspended_phase_2"
             "clinically_tested_terminated_phase_1_or_phase_2"
             "clinically_tested_terminated_phase_1"
             "clinically_tested_terminated_phase_2"
             "clinically_tested_suspended_phase_2_or_phase_3"
             "clinically_tested_terminated_phase_0"
             "clinically_tested_withdrawn_phase_3"
             "clinically_tested_suspended_phase_1_or_phase_2"
             "clinically_tested_withdrawn_phase_2_or_phase_3"
             "clinically_tested_terminated_phase_3"
             "clinically_tested_suspended_phase_1"
             "clinically_tested_withdrawn_phase_2"
             "clinically_tested_suspended_phase_3"
             "xref"
             "clinically_tested_withdrawn_phase_1"
             "clinically_tested_approved_unknown_phase"
             "clinically_tested_suspended_phase_0"
             "clinically_tested_terminated_phase_2_or_phase_3"
             "clinically_tested_withdrawn_phase_0"
             "clinically_tested_withdrawn_phase_1_or_phase_2")))))

3.3 shortest paths containing predicate pred from node-1 to unknown node:

    * find all the ("shortest") paths containing predicate "clinically_tested_terminated_phase_2" from "CHEBI"
      (shortest-path* all-hash "CHEBI" #f "clinically_tested_terminated_phase_2")
      => (("CHEBI" "DRUGBANK" "UMLS") ("CHEBI" "DOID" "UMLS" "DRUGBANK"))

    * find the shortest path(s) and show the predicates connecting each pair of nodes(databases)
      (shortest-path*-with-pred all-hash "CHEBI" #f "clinically_tested_terminated_phase_2")
      => ((("CHEBI" "DRUGBANK" "UMLS") ("CHEBI" "DOID" "UMLS" "DRUGBANK"))
          (((("CHEBI" . "DRUGBANK") ("same_as"))
            (("DRUGBANK" . "CHEBI") ("external_identifier" "same_as"))
            (("DRUGBANK" . "UMLS")
             ("clinically_tested_suspended_phase_2"
              "clinically_tested_terminated_phase_1_or_phase_2"
              "clinically_tested_terminated_phase_1"
              "clinically_tested_terminated_phase_2"
              "clinically_tested_suspended_phase_2_or_phase_3"
              "clinically_tested_terminated_phase_0"
              "clinically_tested_withdrawn_phase_3"
              "clinically_tested_suspended_phase_1_or_phase_2"
              "clinically_tested_withdrawn_phase_2_or_phase_3"
              "clinically_tested_terminated_phase_3"
              "clinically_tested_suspended_phase_1"
              "clinically_tested_withdrawn_phase_2"
              "clinically_tested_suspended_phase_3"
              "xref"
              "clinically_tested_withdrawn_phase_1"
              "clinically_tested_approved_unknown_phase"
              "clinically_tested_suspended_phase_0"
              "clinically_tested_terminated_phase_2_or_phase_3"
              "clinically_tested_withdrawn_phase_0"
              "clinically_tested_withdrawn_phase_1_or_phase_2")))))

4. visualize the shortest path
   - The input of visualize-with-web must be the result returned from shortest-path*-with-pred (i.e. list contains all path(s)
     and shortest-path(s) with predicates)
   (define p (shortest-path*-with-pred all-hash "CHEBI" #f "clinically_tested_terminated_phase_2"))
   (visualize-with-web p)

   - Open shorest-path-graph-visual-hover.html under the same folder and the path(s) will be displayed on browser.

5. Tool: print out the connecting predicate(s) for a given/desired path [e.g. (node-1 node-2 ...)]
   - (single-path=>path-with-pred all-hash single-path)

   * (single-path=>path-with-pred all-hash '("CHEBI" "DOID" "UMLS" "DRUGBANK"))


|#

;===================================================loading data==============================================

; a tsv reader
(define make-tsv-reader
  (make-csv-reader-maker
   '((separator-chars            #\tab)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

#|
(define convert-to-int
  (lambda (ls)
    (list
     (car ls)
     (cadr ls)
     (caddr ls)
     (string->number (cadddr ls)))))
|#
; conver the third item on the list from string to integer
(define last-ele=>int
  (lambda (ls)
    (cond
      [(null? (cdr ls)) (list (string->number (car ls)))]
      [else (cons (car ls) (last-ele=>int (cdr ls)))])))

;; load the data from a tsv file
;; each row is represented as one list
;; data type (string string string int)
(define load-data
  (lambda (file-name)
    (cdr (load-data-with-header file-name))))

;; header ("subject CURIE prefix" "object CURIE prefix" "predicate" "edge count")
(define load-data-with-header
  (lambda (file-name)
    (csv-map last-ele=>int (make-tsv-reader (open-input-file file-name)))))


;========================================building graph and loading necessary hash tables======================================
#|
The graph is composed with two hash tables: node hash table and edge hash table.

node hash table: (key) curie prefix => (value) curie prefix list (two curies are connected by predicate(s))
^ is based on how it traverses from one database to another database (the path): either sub=>obj or obj=>sub

edge hash table: (key) subject-object-pair => (value) predicate list
^ edges information are from the KG
(hash-ref edge-hash-table (B . A)) returning '() means there is no predicate from B to A.
If A ="same_as"=> B, but there is no predicate from B to A, A => B and B => A are two both possible paths.
|#

; node-hash with undirected edges (subject <-> object)
; biuld node-hash with edge/predicate count no less than 100("low" level), 1000("medium" level),
; or 10000("high" level); or desired number(e.g. no more than 500(int))
; for filtering low edge count data
(define make-undirected-node-hash
  (lambda (ls level/num)
    (let ((input-graph (make-immutable-hash)))
      (if (string? level/num)
          (make-undirected-node-hash-helper ls input-graph (level=>min-count level/num))
          (make-undirected-node-hash-helper ls input-graph level/num)))))

(define make-undirected-node-hash-helper
  (lambda (ls graph min-count)
    (cond
      [(null? ls) graph]
      [else
       (let ((sub (caar ls))
             (obj (cadar ls))
             (count (car (cdddar ls))))
         (if (and (not (equal? sub obj)) (> count min-count))
             (let ((one-direct-graph
                    (hash-update graph sub (lambda (v) (set-add v obj)) '())))
               (let ((two-direct-graph
                      (hash-update one-direct-graph obj (lambda (v) (set-add v sub)) '())))
                 (make-undirected-node-hash-helper (cdr ls) two-direct-graph min-count)))
             (make-undirected-node-hash-helper (cdr ls) graph min-count)))])))

(define level=>min-count
  (lambda (str)
    (cond
      [(equal? str "high") 9999]
      [(equal? str "medium") 999]
      [(equal? str "low") 99])))


; Edges/predicates are stored in a hash table enabling quick search
; input: data from .tsv file (list) with edge/predicate count no less than 100("low" level), 1000("medium" level),
; or 10000("high" level); or a desired number(e.g. greater than 500(int)) 
; format: (((subject . object) . (predicates)) ... ) => hash table

(define make-edge-hash
  (lambda (ls level/num)
    (let ((pred*-hash (make-immutable-hash)))
      (if (string? level/num)
          (make-edge-hash-helper ls pred*-hash (level=>min-count level/num))
          (make-edge-hash-helper ls pred*-hash level/num)))))

(define make-edge-hash-helper
  (lambda (ls pred*-hash min-count)
    (cond
      [(null? ls) pred*-hash]
      [else
       (let ((sub-obj-pr (cons (caar ls) (cadar ls)))
             (pred (caddar ls))
             (count (car (cdddar ls))))
         (if (> count min-count)
             (let ((pred-hash
                    (hash-update pred*-hash sub-obj-pr (lambda (v) (set-add v pred)) '())))
               (make-edge-hash-helper (cdr ls) pred-hash min-count))
             (make-edge-hash-helper (cdr ls) pred*-hash min-count)))])))

; return a hash table having pred as key and a list of prefix-pairs(connected by the key predicate) as value
; edge/predicate count no less than 100("low" level), 1000("medium" level), or 10000("high" level)
; or a desired number(e.g. greater than 500(int)) 
(define preds=>database-pair
  (lambda (ls level/num)
    (let ((database-pair*-hash (make-immutable-hash)))
      (if (string? level/num)
          (preds=>database-pair-helper ls database-pair*-hash (level=>min-count level/num))
          (preds=>database-pair-helper ls database-pair*-hash level/num)))))

(define preds=>database-pair-helper
  (lambda (ls database-pair*-hash min-count)
    (cond
      [(null? ls) database-pair*-hash]
      [else
       (let ((sub-obj-pr (cons (caar ls) (cadar ls)))
             (pred (caddar ls))
             (count (car (cdddar ls))))
         (if (> count min-count)
             (let ((database-pair-hash
                    (hash-update database-pair*-hash pred (lambda (v) (set-add v sub-obj-pr)) '())))
               (preds=>database-pair-helper (cdr ls) database-pair-hash min-count))
             (preds=>database-pair-helper (cdr ls) database-pair*-hash min-count)))])))

;===================================================shorest path==============================================

; TODO shortest path containing predicate "pred" from "start" curie prefix
; TODO string search for pred

; shortest path containing predicate "pred" from "start" curie prefix to an unknown curie prefix
; thoughts:
; 1. find which two nodes (n1 and n2) contain pred
; 2. find the shorest paths from start to n1. Then n2 is the end node.
; 2. find the shorest paths from start to n2.  Then n1 is the end node.
; 3. combine all the "shortest" paths
; 4. sort the result from above, return the shortest path(s)

(define shortest-path*-contain-pred-unknown-end
  (lambda (all-hash start pred)
    (match-define (list node-hash edge-hash pred/prefix-pr-hash) all-hash)
    (let ((prefix-pr-ls (hash-ref pred/prefix-pr-hash pred '())))
      (let ((undir-prefix-pr-ls (cdr (pr-ls=>undirected-prs-ls prefix-pr-ls (list (list))))))
            (cdr (shortest-path*-contain-pred-unknown-end-helper node-hash undir-prefix-pr-ls start (list (list))))))))

(define shortest-path*-contain-pred-unknown-end-helper
  (lambda (node-hash prefix-ls start path-ls)
    (cond
      [(null? prefix-ls) path-ls]
      [else
       (let ((n1 (caar prefix-ls))
             (n2 (cdar prefix-ls)))
         (cond
           [(equal? start n1)
            (let ((new-path (list n1 n2)))
              (shortest-path*-contain-pred-unknown-end-helper node-hash (cdr prefix-ls) start (append path-ls (list new-path))))]
           [(equal? start n2)
            (shortest-path*-contain-pred-unknown-end-helper node-hash (cdr prefix-ls) start path-ls)]
           [else
             (let ((start=>n1-path (BFS node-hash start n1)))
              (if start=>n1-path
                  (let ((new-path (append start=>n1-path (list n2))))
                    (shortest-path*-contain-pred-unknown-end-helper node-hash (cdr prefix-ls) start (append path-ls (list new-path))))
                  (shortest-path*-contain-pred-unknown-end-helper node-hash (cdr prefix-ls) start path-ls)))]))])))
  




; shortest path containing predicate "pred" from "start" curie prefix to "end" curie prefix
; thoughts:
; 1. find which two nodes (n1 and n2) contain pred
; 2. find the shorest paths from start to n1 and from n2 to end; combine two paths
; 2. find the shorest paths from start to n2 and from n1 to end; combine two paths
; 3. combine all the "shortest" paths
; 4. sort the result from above, return the shortest path(s)

; return all the ("shortest") paths containing predicate pred
(define shortest-path*-contain-pred
  (lambda (all-hash start end pred)
    (match-define (list node-hash edge-hash pred/prefix-pr-hash) all-hash)
    (let ((prefix-pr-ls (hash-ref pred/prefix-pr-hash pred '())))
      (if (or (set-member? prefix-pr-ls (cons start end)) (set-member? prefix-pr-ls (cons end start)))
          (list (list start end))
          (let ((undir-prefix-pr-ls (cdr (pr-ls=>undirected-prs-ls prefix-pr-ls (list (list))))))
            (cdr (shortest-path*-contain-pred-helper node-hash undir-prefix-pr-ls start end (list (list)))))))))

(define shortest-path*-contain-pred-helper
  (lambda (node-hash prefix-ls start end path-ls)
    (cond
      [(null? prefix-ls) path-ls]
      [else
       (let ((n1 (caar prefix-ls))
             (n2 (cdar prefix-ls)))
         (cond
           [(equal? start n1)
             (let ((n2=>end-path (BFS node-hash n2 end)))
               (if n2=>end-path
                   (let ((new-path (append (list n1) n2=>end-path)))
                         (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end (append path-ls (list new-path))))
                   (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end path-ls)))]
           [(equal? end n2)
             (let ((start=>n1-path (BFS node-hash start n1)))
               (if start=>n1-path
                   (let ((new-path (append start=>n1-path (list n2))))
                     (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end (append path-ls (list new-path))))
                   (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end path-ls)))]
           [(or (equal? start n2) (equal? end n1))
            (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end path-ls)]
           [else
             (let ((start=>n1-path (BFS node-hash start n1))
                  (n2=>end-path (BFS node-hash n2 end)))
              (if (and start=>n1-path n2=>end-path)
                  (let ((new-path (append start=>n1-path n2=>end-path)))
                    (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end (append path-ls (list new-path))))
                  (shortest-path*-contain-pred-helper node-hash (cdr prefix-ls) start end path-ls)))]))])))

; sort the result from shortest-path*-contain-pred, and 
; return the shortest path(s) in a list
(define find-shortest-path-ls
  (lambda (path-ls)
    (let ((ordered-path-ls
           (sort path-ls
                 #:key length <)))
      (let ((shortest-path-ls (list (car ordered-path-ls))))
        (find-shortest-path-ls-helper ordered-path-ls shortest-path-ls)))))

(define find-shortest-path-ls-helper
  (lambda (ordered-path-ls shortest-path-ls)
    (cond
      [(or (null? ordered-path-ls) (null? (cdr ordered-path-ls))) shortest-path-ls]
      [else
       (let ((first (car ordered-path-ls))
             (second (cadr ordered-path-ls)))
         (if (and (not (null? (cdr ordered-path-ls)))
                  (equal? (length first)
                          (length second)))
             (find-shortest-path-ls-helper (cdr ordered-path-ls)
                                    (append shortest-path-ls (list second)))
             shortest-path-ls))])))


; return the shorest path(s) from start to end containg predicate pred
; with the corresponding predicate(s) connect pairs of databases
(define shortest-path-with-pred
  (lambda (path-ls edge-hash)
    (shortest-path-with-pred-helper path-ls edge-hash (mlist (list)))))

(define shortest-path-with-pred-helper
  (lambda (path-ls edge-hash acc)
    (for-each
     (lambda (path)
       (set-mcdr! acc (append (mcdr acc) (list (path-with-pred-undirected path edge-hash)))))
     (find-shortest-path-ls path-ls))
    (mcdr acc)))
      
; convert a list of pair to be undirected
; example: ((d1 . d2) (d5 . d7) (d6 . d6)) => ((d1 . d2) (d2 . d1) (d5 . d7) (d7 . d5) (d6 . d6))
(define pr-ls=>undirected-prs-ls
  (lambda (pr-ls undirected-pr-ls)
    (cond
      [(null? pr-ls) undirected-pr-ls]
      [else
       (let ((sub (caar pr-ls))
             (obj (cdar pr-ls)))
         (if (not (equal? sub obj))
             (let ((one-dir-prls (append undirected-pr-ls (list (car pr-ls)))))
               (let ((two-dir-prls (append one-dir-prls (list (cons obj sub)))))
                 (pr-ls=>undirected-prs-ls (cdr pr-ls) two-dir-prls)))
             (pr-ls=>undirected-prs-ls (cdr pr-ls) (append undirected-pr-ls (list (car pr-ls))))))])))


; unweighted shorest path - Breadth First Search
; input: node-hash, a strart node/vertex, an end node/vertex
(define BFS
  (lambda (node-hash start dest)
    (if (equal? start dest) (list start dest)
        (let ((Q (mlist start))
              (visited (mutable-set start))
              (predecessor (make-hash)))
          ; whether exist a path from start to dest: (BFS-helper graph Q dest visited pred) (true or false)
          (if (BFS-helper node-hash Q dest visited predecessor)
              (let ((cdr-path (get-path start dest predecessor)))
                (cons start cdr-path))
              false)))))

; whether exist a path from start to dest
; return true/false
(define BFS-helper
  (lambda (graph Q end visited pred)
    (cond
      [(set-member? visited end) true]
      [(null? Q) false]
      [else
       ; v:parent and w:child
       (let ((v (mcar Q))
             (Q* (mcdr Q)))
         (for-each
          (lambda (w)
            (when (not (set-member? visited w))
              (begin
                (set-add! visited w)
                (hash-set! pred w v)
                (set-mcdr! Q (mappend (mcdr Q) (mlist w))))))
          (hash-ref graph v '()))
         ;(display visited)
         ;(display (mcdr Q))
         (BFS-helper graph (mcdr Q) end visited pred))])))

(define get-path
  (lambda (start dest pred)
    (let ((path (list dest)))
      (get-path-helper start dest pred path))))

(define get-path-helper
  (lambda (start dest pred path)
    (let ((pred-of-dest (hash-ref pred dest '())))
          (cond
            [(equal? pred-of-dest start) path]
            [else
             (let ((path* (cons pred-of-dest path)))
               (get-path-helper start pred-of-dest pred path*))]))))


;; Next: find the corresponding edges/predicates of each database from the result


;retun the corresponding edges/predicates of each database(subject object) pair in one path
;format: (((subject . object) (pred1 pred2 ... )) ... ) => list of lists

(define path-with-pred-undirected
  (lambda (ls pred*-hash)
    (let ((sub-obj-pr-ls (cdr (ls=>pr-ls-undirected ls (list (list))))))
      (let ((pred-ls (sub-obj-pr-ls=>preds sub-obj-pr-ls pred*-hash)))
        (cdr (combine-sub-obj-pr-pred sub-obj-pr-ls pred-ls (list (list))))))))

;map each subject object pair as the key to find the corresponding predicates
;return list of predicates list
;format: ((pred1 pred2 ...) (pred1 pred2 ...) ...)
(define sub-obj-pr-ls=>preds
  (lambda (sub-obj-pr-ls pred*-hash)
    (map
     (lambda (k) (hash-ref pred*-hash k '()))
     sub-obj-pr-ls)))

;take a list of database name (d1 d2 d3 ...)
;return a list of database pairs by order ((d1 . d2) (d2 . d3) ...) -> same as saying from d1 to d2, d2 to d3, ...
;only works for directed edges/graph
(define ls=>pr-ls-directed
  (lambda (ls accu-ls-ls)
    (cond
      [(null? (cdr ls)) accu-ls-ls]
      [else
       (let ((from (car ls))
             (to (cadr ls)))
         (ls=>pr-ls-directed (cdr ls) (append accu-ls-ls (list (cons from to)))))])))

;for undirected graph: ((d1 . d2) (d2 . d1) (d2 . d3) (d3 . d2) ...))
(define ls=>pr-ls-undirected
  (lambda (ls accu-ls-ls)
    (cond
      [(null? (cdr ls)) accu-ls-ls]
      [else
       (let ((from (car ls))
             (to (cadr ls)))
         (if (equal? from to)
             (ls=>pr-ls-undirected (cdr ls) (append accu-ls-ls (list (cons from to))))
             (ls=>pr-ls-undirected (cdr ls) (append accu-ls-ls (list (cons from to) (cons to from))))))])))

;recursively combine each subject object pair to the corresponding list of predicates
;format: (((subject . object) (pred1 pred2 ... )) ... ) => list of lists
(define combine-sub-obj-pr-pred
  (lambda (pr-ls pred-ls accu)
    (cond
      [(null? pr-ls) accu]
      [else
       (if (not (null? (car pred-ls)))
           (combine-sub-obj-pr-pred (cdr pr-ls) (cdr pred-ls) (append accu (list (list (car pr-ls) (car pred-ls)))))
           (combine-sub-obj-pr-pred (cdr pr-ls) (cdr pred-ls) accu))])))

;=============================================== visualization ==========================================================


(define path*-with-pred=>set-ls
  (lambda (path*-with-pred st)
    (cond
      [(null? path*-with-pred) st]
      [else
       (path*-with-pred=>set-ls (cdr path*-with-pred) (path*-with-pred=>set-ls-helper (car path*-with-pred) st))])))

(define path*-with-pred=>set-ls-helper
  (lambda (path*-ls st)
    (cond
      [(null? path*-ls) st]
      [else
         (let ((subject-curie-prefix (caaar path*-ls))
               (object-curie-prefix (cdaar path*-ls))
               (predicate-string (string-join (cadar path*-ls) "\n")))
           (let ((new-st (set-add st (list subject-curie-prefix object-curie-prefix predicate-string))))
             (path*-with-pred=>set-ls-helper (cdr path*-ls) new-st)))])))

(define path*-ls=>concept-set
  (lambda (path*-ls concept-set)
      (let ((shortest-path-ls (find-shortest-path-ls path*-ls)))
        (shortest-path-ls=>concept-set shortest-path-ls concept-set))))


(define shortest-path-ls=>concept-set
  (lambda (shortest-path-ls concept-set)
    (cond
      [(null? shortest-path-ls) concept-set]
      [else
       (for-each
        (lambda(p)
          (set-add! concept-set p))
        (car shortest-path-ls))
       (shortest-path-ls=>concept-set (cdr shortest-path-ls) concept-set)])))

; write a JASON file containing the nodes and edges information for visualization
(define visualize-with-web
  (lambda (shortest-path*-with-pred)
    (with-output-to-file "shorest-path-graph-vars.js"
      (lambda ()
        (printf "var nodes = new vis.DataSet([\n")
        (for-each
         (lambda(c)
           (printf "{ id: ~s, label: ~s},\n" c c))
         (set->list (path*-ls=>concept-set (car shortest-path*-with-pred) (mutable-set))))
        (printf "]);\n")
        (printf "var edges = new vis.DataSet([\n")
        (set-map
         (path*-with-pred=>set-ls (cadr shortest-path*-with-pred) (set))
         (lambda (ls)
           (match ls
             [`(,subject-curie-prefix ,object-curie-prefix ,predicate-string)
              (printf "{ from: ~s, to: ~s, arrows: \"to\", predicate: ~s},\n" subject-curie-prefix object-curie-prefix predicate-string)])))
        (printf "]);\n"))
      #:mode 'text
      #:exists 'replace)))


;===============================================useful helper functions==================================================

(define build-undirected-graph
  (lambda (ls level/num)
    (list (make-undirected-node-hash ls level/num)
    (make-edge-hash ls level/num))))

;(match-define (list node-hash edge-hash) (build-undirected-graph rtx2-data 2669662))

; load all required hash tables for finding the shortest path
; input: data from .tsv file; edge/predicate count level or number 
(define load-all-hash
  (lambda (ls level/num)
    (list
     (make-undirected-node-hash ls level/num)
     (make-edge-hash ls level/num)
     (preds=>database-pair ls level/num))))

;(match-define (list node-hash edge-hash pred/prefix-pr-hash) (load-all-hash rtx2-data 2669662))

(define shortest-path*
  (lambda (all-hash start end pred)
    (match-define (list node-hash edge-hash pred/prefix-pr-hash) all-hash)
    (cond
      [(equal? pred #f)
       (list (BFS node-hash start end))]
      [(equal? end #f)
       (shortest-path*-contain-pred-unknown-end all-hash start pred)]
      [else
       (shortest-path*-contain-pred all-hash start end pred)])))
       
(define shortest-path*-with-pred
  (lambda (all-hash start end pred)
    (match-define (list node-hash edge-hash pred/prefix-pr-hash) all-hash)
    (cond
      [(equal? pred #f)
       (let ((path (list (BFS node-hash start end))))
         (list path (shortest-path-with-pred path edge-hash)))]
      [(equal? end #f)
       (let ((path (shortest-path*-contain-pred-unknown-end all-hash start pred)))
         (list path (shortest-path-with-pred path edge-hash)))]
      [else
       (let ((path (shortest-path*-contain-pred all-hash start end pred)))
         (list path (shortest-path-with-pred path edge-hash)))])))

(define single-path=>path-with-pred
  (lambda (all-hash single-path)
    (if (list? (car single-path))
        (error "The second argument should be one single path. e.g. (node-1 node-2)")
        (path-with-pred-undirected single-path (cadr all-hash)))))
