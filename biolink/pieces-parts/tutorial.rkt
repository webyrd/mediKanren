#lang racket
(require
  "../db.rkt"
  "query.rkt")

#|
This line binds the identifier S to the result of the following expression,
which takes the first concept containing the curie string "UniProtKB:P51587."

Functions Used:
keep: returns the first n elements of a list, documented in common.rkt
find-concepts: finds all concepts connected to a list of curie strings, documented in common.rkt
|#
(define S (keep 1 (find-concepts #t (list "UniProtKB:P51587"))))

#|
These lines run the knowledge graph to find edges that use S as a subject and returns the associated concepts and edges as
name=>concepts and name=>edges.

Functions Used:
match-define: takes a list of names and an expression and binds each name to its corresponding value in the expressions, documented in Racket Documentation
run/graph: 
|#
(match-define
  (list name=>concepts name=>edges)
  (run/graph
   ((S S)
    (O #f))
   ((S->O #f))
   (S S->O O)))

#|
 Defines Es as the list of edges retrieved from name=>edges (associated with S, which is the concept associated with the curie "UniProtKB:P51587.")
name=>edges was defined using match-define, and it contains the edges that correspond to the subject concept defined above.

Functions used:
hash-ref: from racket,returns value for the key ('S->O) in hash (name=>edges)
|#

(define Es (hash-ref name=>edges 'S->O))

#|
Next, we sought to find concepts that specifically came from the GO ontology. In order to do this, we had to
extract the provider from a list of edges.

Functions Used:
define: in this case, define is being used to create a procedure (named "provider"), documented in Racket Documentation
assoc: assoc takes in a variable and a list and returns the first element of the list whose value is equal to the varaible,
documented in Racket Documentation

|#
(define (provider e)
  (let ((a (assoc "provided_by" (list-ref e 5))))
    (if a
        (cdr a)
        a)))

;; This line uses sort and remove-duplicates, as well as the provider function above, to get all the provider names from the edges in Es
(sort (remove-duplicates (map provider Es)) string<=?)

;; This line filters out the edges in Es that have the GO ontology (gene_ontology) as a provider
;; (define goEs (filter (lambda (e) (string=? "gene_ontology" (provider e))) Es))

(define go-provider-string "(\"http://purl.obolibrary.org/obo/go-plus.owl\")")

(define goEs (filter (lambda (e) (string=? go-provider-string (provider e))) Es))

;; This line checks the number of such edges using length
(length (filter (lambda (e) (string=? go-provider-string (provider e))) Es))

;; This line binds the identifier E to the first edge in Es that has the GO ontology as a provider
(define E (car (filter (lambda (e) (string=? go-provider-string (provider e))) Es)))


#|
 This function takes an edge and gets the subject concept and object concept that the edge relates along
with the predicate associated with the edge and returns them in a list that is formatted to show the relationship.
 returns: (subject-concept-name edge-predicate object-concept-name)

 Functions Used:
concept->name, edge->subject, edge->pred, edge->object, documented in common.rkt
|# 
(define (story E)
  (list
   (concept->name (edge->subject E))
   (cdr (edge->pred E))
   (concept->name (edge->object E))))

(story E)
;; maps the story function onto all of the individual edges in the list Es
(map story Es)
;; maps the story function onto all individual edges in the list goES
(map story goEs)
;; creates an anonymous function that takes the cdr of the predicate of an edge
;; which finds all of the possible predicate names (i.e. physically_interacts_with, participates_in)
;; then, this line maps that function onto all the edges in Es
(remove-duplicates (map (lambda (E) (cdr (edge->pred E))) Es))

#|
 This creates a mutable hash table and sets it to *counters*

Functions Used:
make-hash: creates a hashtable, documented in Racket Documentation
|#
(define *counters* (make-hash))
(define (inc-count! key)
  (hash-update! *counters*
		key
		(lambda (x) (+ x 1))
		(lambda () 1)))

;; This line defines f and sets it equal to the file opened by the open-input-file function
(define f (open-input-file "../data/rtx2/edges.scm"))
(define e (read f))

#|
This reads through the file defined as f and counts the number of "provided_by" predicates that appear in the file

Functions used:
flush-output, read, eof-object?, begin, close-input-port: This reads through the file defined as f and counts
the number of "provided_by" predicates that appear in the file, documented in Racket Documentation

(let loop ((n 0))
  (when (= 0 (modulo n 10000))
    (display ".")
    (flush-output))
  (let ((r (read f)))
    (if (eof-object? r)
	(begin
	  (close-input-port f)
	  *counters*)
	(begin
	  (inc-count! (cdr (assoc "provided_by" (vector-ref r 3))))
	  (loop (+ 1 n))))))
|#

;; We can also use a miniKanren query instead:
(define edges (run 10000 (e) (edgeo e)))
(define e  (car edges))
(remove-duplicates
 (map (lambda (e)
          (let ((p (assoc "provided_by" (list-ref e 5))))
            (if p
                (cdr p)
                #f)))
        edges))

#|
This defines qb as the query/graph subsequently defined
The query/graph is defined with an arbitrary subject concept (X), an object concept (rhobtb2) that corresponds with
the CURIE "UMLS:C1425762", any edges (X->rhobtb2) that relate any subject concept to the rhobtb2 object concept, and the pathway
that encompasses the (subject edge object) relationship

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
(define qb (time (query/graph
                  ((X       #f)
                   (rhobtb2 "UMLS:C1425762"))
                  ((X->rhobtb2 #f))
                  (X X->rhobtb2 rhobtb2))))


#|
This defines qo as the run of the query/graph
The query/graph is defined with a subject concept (S) that corresponds with the CURIE "UniProtKB:P51587", an arbitrary
object concept (O), any edges (S->O) that relate the S subject concept to any object concept, and the pathway
that encompasses the (subject edge object) relationship.

The query answers: What are all the concepts reachable from this one gene in 1-hop?

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
(define qo (time (query/graph
		 (
		  (S "UniProtKB:P51587")
		  (O #f)
		  )
		 ((S->O #f))
		 (S S->O O))))
(pretty-print (time (report/query qo)))
(pretty-print (time (ranked-paths qo)))


;; defines a list of the providers that indicate the GO ontology
(define go-providers
  (list
   "(\"http://purl.obolibrary.org/obo/go-plus.owl\")"
   "(\"http://purl.bioontology.org/ontology/OMIM\")"))


;; function that takes in a provider and checks if it is a GO provider using the list defined above (go-providers)
(define ((edge/provider? p) e)
  (member (provider e) go-providers))

;; function that takes in a provider and checks if it is not a GO provider
(define ((edge/not-provider? p) e)
  (not ((edge/provider? p) e)))

#|
This defines qa as the run of the query/graph subsequently defined, which found two different paths from subject to object,
separating them them based on whether the edges in between were provided by the GO ontology or not.

The query/graph is defined with a subject concept (S) that corresponds with the CURIE "UniProtKB:P51587", an arbitrary
object concept (O), any edges (S-1>O) that relate the S subject concept to any object concept provided that the edges
are of the GO ontology, and any edges (S-2>O) that relate the S subject concept to any object concept provided that the
edges are not of the GO ontology.

This query answers: What are all the concepts related to the initial gene that go through the GO provider and also not through the GO provider? There has to be two 1-hop ways to get through the concept.

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
(define qa (time (query/graph
		 (
		  (S "UniProtKB:P51587")
		  (O #f)
		  )
		 ((S-1>O #f (edge/not-provider? 'go))
		  (S-2>O #f (edge/provider? 'go)))
		 (S S-1>O O)
		 (S S-2>O O))))

;; prints a report of the queries from the run of the query graph stored in qa
(pretty-print (time (report/query qa)))
;; prints the ranked paths in qa
(pretty-print (time (ranked-paths qa)))

#|
This defines q as the run of the query/graph subsequently defined, which found two different paths from subject to object,
separating them them based on whether the edges in between were provided by the GO ontology or not.

The query/graph is defined with a subject concept (S) that corresponds with the CURIE "UniProtKB:P51587", arbitrary
object concepts (O1/O2), any edges (S->O1) that relate the S subject concept to any object concept provided that the edges
are of the GO ontology, and any edges (S->O2) that relate the S subject concept to any object concept provided that the
edges are not of the GO ontology.

This query relaxes the previous query so that the final concepts can be different.

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
(define q (time (query/graph
		 (
		  (S "UniProtKB:P51587")
		  (O1 #f)
		  (O2 #f)
		  )
		 ((S->O1 #f (edge/not-provider? 'go))
		  (S->O2 #f (edge/provider? 'go)))
		 (S S->O1 O1)
		 (S S->O2 O2))))

;; TODO: does not seem to work in rtx2
#|
This function checks whether the relation in a pathway is "involved-in," which
would indicate that the relationship is part of the GO ontology
|#
(define (go-pathway? e)
  (string=? "involved_in"
	    (cdr (assoc "relation" (list-ref e 5)))))

(define (unique-gene? g)
  (not (string=? "UniProtKB:P51587" g)))

#|
This defines qc as the run of the query/graph subsequently defined, which found two different paths from subject to object,
separating them them based on whether the edges in between were provided by the GO ontology or not.

The query/graph is defined with a subject concept (S1) that corresponds with the CURIE "UniProtKB:P51587", arbitrary
object concept O, gene subject concept S2, any edges (S1->O) that relate the S subject concept to any object concept,
and any edges (S2->O) that relate the S2 gene concept to any object concept provided that these paths are go pathways.

This query answers: What are other genes in the same pathway?

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
;; TODO: create a variant where the sources are different. --> this is left as is because it is difficult to do
(define qc (time (query/graph
		 (
		  (S1 "UniProtKB:P51587")
		  (O #f)
		  (S2 gene)
		  )
		 ((S1->O #f go-pathway?)
		  (S2->O #f go-pathway?))
		 (S1 S1->O O)
		 (S2 S2->O O))))

;; prints a report of the queries from the run of the query graph stored in qc
(pretty-print (time (report/query qc)))

(define qd (time (query/graph
		 (
		  (S1 "UniProtKB:P51587")
		  (O #f)
		  (S2 gene)
		  )
		 ((S1->O #f go-pathway?)
		  (S2->O #f go-pathway?))
		 (S1 S1->O O)
		 (S2 S2->O O))))

#|
 Each edge is given a score based on a set of parameters, and every edge with the same score is put in a composite edge that can be identified by that score.
The function then returns a list of these composite edges organized by score, with the highest scoring composite edge is first.
This line defines r as the list returned by (ranked-paths qc)

 Functions Used: 
ranked-paths: ranked-paths takes a query/graph and uses a ranking system to return a list of composite edges from query/graph ranked by confidence, documented in query.rkt
|#
(define r (ranked-paths qc))
(cadr r)
;; prints the ranked-paths
(pretty-print (time (ranked-paths qc)))

;; finds the edges from qc that are O->M
(edges/query qc 'O->M)
;; finds the the number of genes that are in the same pathway
(length (curies/query qc 'O))
(length (edges/query qc 'O))


#|
This defines qcomp as a query/graph .
The query/graph is defined with a subject concept (S) that corresponds with the CURIE "UniProtKB:P51587", an
object concept (O) that must be a gene, any edges (S->O) that relate the S subject concept to any gene object concept, and
 the pathway that encompasses the (subject edge object) relationship

Functions Used:
query/graph: runs the query graph given subject and object concepts, documented in query.rkt
|#
(define qcomp (time (query/graph
		 (
		  (S "UniProtKB:P51587")
		  (O gene)
		  )
		 ((S->O #f))
		 (S S->O O))))

;; prints the queries and the ranked paths
(pretty-print (time (report/query qcomp)))
(define rcomp (ranked-paths qcomp))
rcomp


