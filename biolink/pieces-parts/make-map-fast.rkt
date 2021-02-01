#lang racket

(require
 racket/date
 racket/runtime-path)

#|

Generate tab-separed value (TSV) files describing the mappings between
concepts for a specified Knowledge Graph (KG).


Here are the tab-sepated columns for each of the generated TSV files.

CURIE-prefix to CURIE-prefix mapping:

subject concept CURIE-prefix
object concept CURIE-prefix
predicate string
edge counts



concept-category to concept-category mapping:

subject concept category
object concept category
predicate string
edge counts



**CAUTION**

This implementation reads from the KG's raw data files to reduce the
running time and (especially) to reduce the (RAM) memory footprint.
As a result, this code makes assumptions about the formats of the raw
'biolink' KG files, which is fragile.  This code would need to be
modified to work with the 'biolink2' KG file format, for
example (although rewriting the code to take advantage of the
'biolink2' improvements may make more sense).

|#



;; *** Change this string to match the name of the KG you want to map! ***
;(define kg-name "rtx2_2020_09_16")
;(define kg-name "textminingprovider")
;(define kg-name "pr-owl")
;(define kg-name "co-occur")
(define kg-name "orange")


#|
Define paths to the critical data files for the given KG.
|#
(define-runtime-path path:root "..")
(define (path/root relative-path)   (build-path path:root relative-path))
(define path:data                   (path/root "data"))
(define (path/data relative-path)   (build-path path:data kg-name relative-path))
(define path:kg-maps                (build-path path:root "kg-maps"))
(define path:kg-maps/kg-name        (build-path path:kg-maps kg-name))
(define (path/kg-map/kg-name relative-path) (build-path path:kg-maps/kg-name relative-path))

(define predicates-file-path (path/data "predicates.scm"))
(define categories-file-path (path/data "categories.scm"))
(define concepts-file-path   (path/data "concepts.scm"))
(define edges-file-path      (path/data "edges.scm"))


;; Create the directories to hold the KG maps, if they do not exist already
(unless (directory-exists? path:kg-maps)
  (printf "directory ~s not found\ncreating directory ~s\n" path:kg-maps path:kg-maps)
  (make-directory path:kg-maps))

(unless (directory-exists? path:kg-maps/kg-name)
  (printf "directory ~s not found\ncreating directory ~s\n" path:kg-maps/kg-name path:kg-maps/kg-name)
  (make-directory path:kg-maps/kg-name))


#|
Define file names and paths for the output TSV files.
|#
(define TSV-file-name-suffix ".tsv")

(define CURIE-prefix_to_CURIE-prefix-file-name-prefix "subject-prefix_object-prefix_predicate_count")
(define CURIE-prefix_to_CURIE-prefix-file-name
  (string-append CURIE-prefix_to_CURIE-prefix-file-name-prefix TSV-file-name-suffix))
(define CURIE-prefix_to_CURIE-prefix-file-path
  (path/kg-map/kg-name CURIE-prefix_to_CURIE-prefix-file-name))

(define category_to_category-file-name-prefix "subject-category_object-category_predicate_count")
(define category_to_category-file-name
  (string-append category_to_category-file-name-prefix TSV-file-name-suffix))
(define category_to_category-file-path
  (path/kg-map/kg-name category_to_category-file-name))

;; Represents a CURIE that does not conform to the standard <prefix>:<suffix> CURIE format.
(define NON-STANDARD-CURIE-STRING "*non-standard CURIE*")



#|
Takes a Racket date object and returns a string containing the
hour:minute:second + date in a pretty format.
|#
(define date->pretty-time/date
  (lambda (d)
    (let ((h (date-hour d))
          (m (date-minute d))
          (s (date-second d)))
      (let ((num->pretty-string
             (lambda (n)
               (if (< n 10)
                   (string-append "0" (number->string n))
                   (number->string n)))))
        (format "~a:~a:~a ~a"
                (num->pretty-string h)
                (num->pretty-string m)
                (num->pretty-string s)
                (date->string d))))))


(printf "\n\nProcessing KG ~s\n" kg-name)
(printf "started processing at ~s\n" (date->pretty-time/date (current-date)))
(define start-seconds (current-seconds))

#|
Open a text file with the given file path, reading in each row, and
returning a vector of those values. Closes the file automatically.
|#
(define file->vector
  (lambda (file-path)
    (with-input-from-file file-path
      (lambda ()
        (let loop ((ls '())
                   (x (read)))
          (cond
            ((eof-object? x) (list->vector (reverse ls)))
            (else (loop (cons x ls) (read))))))
      #:mode 'text)))

#|
Vector containing all the predicate strings for the given KG,
in the same order as in the KG's 'predicates.scm' file.
|#
(define predicates-vector (file->vector predicates-file-path))
(printf "created predicates vector with ~s entries\n" (vector-length predicates-vector))

#|
Vector containing all the category strings for the given KG,
in the same order as in the KG's 'categories.scm' file.
|#
(define categories-vector (file->vector categories-file-path))
(printf "created categories vector with ~s entries\n" (vector-length categories-vector))


#|
Returns the number of rows in a file.
|#
(define count-number-of-rows
  (lambda (file-path)
    (printf "counting the number of rows in file ~s\n" file-path)
    (time
     (with-input-from-file file-path
       (lambda () (let loop ((i 0) (x (read-char)))
                    (cond
                      [(eof-object? x) i]
                      [(char=? x #\newline) (loop (add1 i) (read-char))]
                      [else (loop i (read-char))])))
       #:mode 'text))))


#|
Takes a string representing a CURIE.

If the string is a "standard" CURIE of the form

<prefix>:<suffix>

then 'get-curie-prefix' returns the string <prefix>.

Otherwise, the string is not a CURIE in the standard format,
in which case 'get-curie-prefix' returns #f.
|#
(define get-curie-prefix
  (lambda (curie)
    (cond
      [(string-contains? curie ":")
       (let ((curie-prefix (car (string-split curie ":" #:trim? #f))))
         curie-prefix)]
      [else #t])))

#|
Number of distinct concepts in the KG.
|#
(define num-concepts (count-number-of-rows concepts-file-path))
(printf "KG ~s contains ~s distinct concepts\n" kg-name num-concepts)


#|
Vector that maps concept indices (natural numbers) to CURIE prefixes.

To find the CURIE prefix for the concept with index N, reference the
Nth entry in 'concept->curie-prefix-vector'.  For example, this
expression will return the CURIE prefix for concept 7:

(vector-ref concept->curie-prefix-vector 7)

If the CURIE string for a concept does not match the standard
<prefix>:<suffix> format, the vector will instead contain the special
string bound to the NON-STANDARD-CURIE-STRING constant.
|#
(define concept->curie-prefix-vector (make-vector num-concepts))

#|
Vector that maps concept indices (natural numbers) to category indices (natural numbers).

To find the category index for the concept with index N, reference the
Nth entry in 'concept->category-vector'.  For example, this expression
will return the category index for concept 7:

(vector-ref concept->category-vector 7)

To map the category index to the category name, reference the category
index in 'categories-vector':

(vector-ref categories-vector (vector-ref concept->category-vector 7))
|#
(define concept->category-vector (make-vector num-concepts))

(time
 (with-input-from-file concepts-file-path
   (lambda ()
     (let loop ((i 0) ;; number of concepts read in
                (good-curie-count 0) ;; number of CURIEs in the standard <prefix>:<suffix> format
                (non-standard-curie-count 0) ;; number of CURIEs that don't match the standard format
                (x (read)))
       (when (zero? (modulo i (expt 10 5)))
         (printf "read ~s concepts, with ~s good CURIES and ~s non-standard CURIES so far...\n"
                 i good-curie-count non-standard-curie-count))
       (cond
         ((eof-object? x)
          (printf "finished reading all ~s concepts, with \n~s good CURIES and ~s non-standard CURIES\n"
                  i good-curie-count non-standard-curie-count)
          (void))
         (else
          (let ((concept-vec x))
            (let ((curie (vector-ref concept-vec 0))
                  (category (vector-ref concept-vec 1)))
              (begin
                ;; set the category for the current concept in the
                ;; 'concept->category-vector' vector
                (vector-set! concept->category-vector i category)
                (let ((curie-prefix (get-curie-prefix curie)))
                  (cond
                    [curie-prefix ;; We were able to get the prefix of the CURIE string
                     (begin
                       (vector-set! concept->curie-prefix-vector i curie-prefix)
                       (loop (add1 i)
                             (add1 good-curie-count)
                             non-standard-curie-count
                             (read)))]
                    [else ;; the CURIE string does not match the standard <prefix>:<suffix> format
                     (begin
                       (vector-set! concept->curie-prefix-vector i NON-STANDARD-CURIE-STRING)
                       (loop (add1 i)
                             good-curie-count
                             (add1 non-standard-curie-count)
                             (read)))])))))))))
   #:mode 'text))

(printf "populated concept->curie-prefix-vector with ~s entries\n" (vector-length concept->curie-prefix-vector))
(printf "populated concept->category-vector with ~s entries\n" (vector-length concept->category-vector))


;; functions that map ids (natural numbers) to strings
(define identity (lambda (x) x))
(define concept-id->curie-prefix (lambda (x) (vector-ref concept->curie-prefix-vector x)))
(define category-id->category-name (lambda (x) (vector-ref categories-vector x)))
(define predicate-id->predicate-name (lambda (x) (vector-ref predicates-vector x)))


#|
Hash table containing a mapping between the key:

(list subject-id object-id predicate-id)

and:

edge-count

where 'edge-count' is the number of entries in the hash-table
that match the key.
|#
(define subject-prefix/object-prefix/predicate-id-hash (make-hash))

#|
Hash table containing a mapping between the key:

(list subject-category-id object-category-id predicate-id)

and:

edge-count

where 'edge-count' is the number of entries in the hash-table
that match the key.
|#
(define subject-category-id/object-category-id/predicate-id-hash (make-hash))

(time
 (with-input-from-file edges-file-path
   (lambda ()
     (let loop ((i 0)
                (x (read)))
       (when (zero? (modulo i (expt 10 5)))
         (printf "read \n~s edges so far...\n" i))
       (cond
         ((eof-object? x) (void))
         (else
          (let ((edge-vec x))
            (let ((subject-id (vector-ref edge-vec 0))
                  (predicate-id (vector-ref edge-vec 1))
                  (object-id (vector-ref edge-vec 2)))
              (let ((subject-prefix (concept-id->curie-prefix subject-id))
                    (object-prefix (concept-id->curie-prefix object-id))
                    (subject-category-id (vector-ref concept->category-vector subject-id))
                    (object-category-id (vector-ref concept->category-vector object-id)))
                (let ((subject-prefix/object-prefix/predicate-id-key
                       (list subject-prefix object-prefix predicate-id))
                      (subject-category-id/object-category-id/predicate-id-key
                       (list subject-category-id object-category-id predicate-id)))
                  (let ((subject-prefix/object-prefix/predicate-id-edge-count
                         (let ((c (hash-ref subject-prefix/object-prefix/predicate-id-hash
                                            subject-prefix/object-prefix/predicate-id-key
                                            #f)))
                           (if c (add1 c) 1)))
                        (subject-category-id/object-category-id/predicate-id-edge-count
                         (let ((c (hash-ref subject-category-id/object-category-id/predicate-id-hash
                                            subject-category-id/object-category-id/predicate-id-key
                                            #f)))
                           (if c (add1 c) 1))))
                    (begin
                      ;;
                      (hash-set! subject-prefix/object-prefix/predicate-id-hash
                                 subject-prefix/object-prefix/predicate-id-key
                                 subject-prefix/object-prefix/predicate-id-edge-count)
                      ;;
                      (hash-set! subject-category-id/object-category-id/predicate-id-hash
                                 subject-category-id/object-category-id/predicate-id-key
                                 subject-category-id/object-category-id/predicate-id-edge-count)
                      ;;
                      (loop (add1 i) (read))))))))))))
   #:mode 'text))


(printf "populated subject-prefix/object-prefix/predicate-id-hash hash table with ~s entries\n"
        (hash-count subject-prefix/object-prefix/predicate-id-hash))
(printf "subject-prefix/object-prefix/predicate-id-hash hash table: ~s\n"
        subject-prefix/object-prefix/predicate-id-hash)

(printf "populated subject-category-id/object-category-id/predicate-id-hash hash table with ~s entries\n"
        (hash-count subject-category-id/object-category-id/predicate-id-hash))
(printf "subject-category-id/object-category-id/predicate-id-hash hash table: ~s\n"
        subject-category-id/object-category-id/predicate-id-hash)



#|
'print-tab-separated-line' takes a list of values, and prints each
value in the list, separated by tab characters. A newline-character,
instead of a tab character, is printed after the last value in the
list.
|#
(define print-tab-separated-line
  (lambda (args)
    (cond
      ((null? args) (newline))
      ((null? (cdr args))
       (begin
         (display (car args))
         (print-tab-separated-line (cdr args))))
      (else
       (begin
         (display (car args))
         (display #\tab)
         (print-tab-separated-line (cdr args)))))))

#|
Sorts and writes the hash-table information to a TSV file,
with the "prettified" values in the 'key' list written first,
followed by the edge counts.

'tsv-file-path' is the path to the TSV file to be written

'map-hash-table' is the hash table whose prettified information is to
be written to the TSV

'column-names-string' must be a format string

'prettify-functions' must be a list of functions, the same length as 'key'
|#
(define sort-and-write-map-hash-table-to-tsv-file
  (lambda (tsv-file-path map-hash-table column-names-string prettify-functions)
    (printf "writing output to ~s...\n" tsv-file-path)
    (let ((list-of-list-of-strings
           (map
            (lambda (key)
              (let ((pretty-values (map (lambda (f a) (f a)) prettify-functions key))
                    (edge-count-string (format "~s" (hash-ref map-hash-table key))))
                (let ((string-ls (append pretty-values (list edge-count-string))))
                  string-ls)))
            (hash-keys map-hash-table))))
      (let ((sorted-list-of-list-of-strings
             (sort
              list-of-list-of-strings
              (lambda (los1 los2)
                (cond
                  [(string-ci<? (car los1) (car los2)) #t]
                  [(and (string-ci=? (car los1) (car los2))
                        (string-ci<? (cadr los1) (cadr los2)))
                   #t]
                  [(and (string-ci=? (car los1) (car los2))
                        (string-ci=? (cadr los1) (cadr los2))
                        (string-ci<? (caddr los1) (caddr los2)))
                   #t]
                  [else #f])))))
        (with-output-to-file tsv-file-path
          (lambda ()
            (printf column-names-string)
            (for-each print-tab-separated-line sorted-list-of-list-of-strings))
          #:mode 'text
          #:exists 'replace)))))

#|
Generate TSV file with the subject CURIE prefix, predicate, object
CURIE prefix, and edge counts
|#
(sort-and-write-map-hash-table-to-tsv-file
 CURIE-prefix_to_CURIE-prefix-file-path
 subject-prefix/object-prefix/predicate-id-hash
 "subject CURIE prefix\tobject CURIE prefix\tpredicate\tedge count\n"
 (list
  identity
  identity
  predicate-id->predicate-name))

#|
Generate TSV file with the subject concept category, object concept
category, predicate, and edge counts
|#
(sort-and-write-map-hash-table-to-tsv-file
 category_to_category-file-path
 subject-category-id/object-category-id/predicate-id-hash
 "subject category\tobject category\tpredicate\tedge count\n"
 (list
  category-id->category-name
  category-id->category-name
  predicate-id->predicate-name))

(define end-seconds (current-seconds))

(printf "Finished processing KG ~s\n" kg-name)
(printf "finished processing at ~s\n" (date->pretty-time/date (current-date)))
(printf "~s seconds elapsed wall-time\n\n" (- end-seconds start-seconds))
