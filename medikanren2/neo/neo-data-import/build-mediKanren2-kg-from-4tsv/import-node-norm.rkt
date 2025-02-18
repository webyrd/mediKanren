#lang racket/base

(require json
         racket/string
         racket/pretty
         "../../dbKanren/test/equivalence-database.rkt"
         "../../dbKanren/dbk/database.rkt"
         "../../dbKanren/dbk/enumerator.rkt"
         "../transform-2tsv-to-4tsv-kgs/transform-utils.rkt"
        racket/runtime-path
        racket/file
        racket/system)

(define BASE "../../neo-data/raw_downloads_from_kge_archive/")
(define NODE-NORM-DIRECTORY (string-append BASE "NodeNorm-24oct/"))
(define RTX-KG2-EDGE (string-append BASE "rtx-kg2-v2.10.0/" "data_01_RAW_KGs_rtx_kg2_v2.10.0_validated_rtx-kg2_2.10.0_edges.tsv"))

#|
***
extract the "same_as" edges from RTX-KG2
***
|#
(define rtx-kg2-edges-in (open-input-file RTX-KG2-EDGE))
(define same-as-export-out (open-output-file (string-append NODE-NORM-DIRECTORY "rtx-kg2-same-as.jsonl")))

(let* ((header (read-line rtx-kg2-edges-in 'any))
       (header (string-split header "\t" #:trim? #f)))
  (let loop ((id 0)
             (line-str (read-line rtx-kg2-edges-in 'any)))
    (when (zero? (modulo id 1000000))
      (printf "processing edges line ~s\n" id))
    (cond
      ((eof-object? line-str)
       (close-input-port rtx-kg2-edges-in)
       (printf "finished extracting same_as edges from RTX-KG2\n"))
      (else
       (let* ((line (efficient-no-trim-tab-string-split line-str))
              (predicate (list-ref line (find-index header "predicate"))))
         (when (equal? predicate "biolink:same_as")
           (let* ((subject (list-ref line (find-index header "subject")))
                  (object (list-ref line (find-index header "object")))
                  (h (hash 'subject subject
                           'object object))
                  (js (jsexpr->string h)))
             (fprintf same-as-export-out "~a\n" js)))
         (loop (add1 id) (read-line rtx-kg2-edges-in 'any)))))))

#|
***
1. remove the empty lines from each .jsonl in the Node Norm KG directory
2. merge the non-empty cleaned .jsonl files into one
***
|#

;; Find all .jsonl files in the Node Norm KG directory
(define input-files
  (filter (lambda (p)
            (string-suffix? p ".jsonl" ))
          (map path->string (directory-list NODE-NORM-DIRECTORY))))

;; Create a directory to store cleaned files.
(define cleaned-dir (string-append NODE-NORM-DIRECTORY "cleaned"))
(unless (directory-exists? cleaned-dir)
  (make-directory cleaned-dir))

;; Clean each .jsonl file using jq if the file is not empty.
(for-each
 (lambda (in-file-str)
   (let ((in-file-full-str (string-append NODE-NORM-DIRECTORY in-file-str)))
     (if (> (file-size (string->path in-file-full-str)) 0)
         (let* ((out-file-str (string-append cleaned-dir "/" in-file-str ".cleaned"))
                (jq-cmd (format "jq -R -c 'select(length > 0) | fromjson' ~a > ~a" in-file-full-str out-file-str)))
           (system* "/bin/sh" "-c" jq-cmd)
           (printf "Cleaned ~a -> ~a\n" in-file-full-str out-file-str))
         (printf "Skipping empty file ~a\n" in-file-full-str))))
 input-files)


;; Merge all cleaned files into one final file.
(define merged-file (string-append cleaned-dir "/" "NodeNorm-RTXKG2-Merged.jsonl"))
;; Collect only those cleaned files that were created (exist)
(define cleaned-files
  (map
   (lambda (f) (string-append cleaned-dir "/" f))
   (filter (lambda (p)
             (string-suffix? p ".cleaned"))
           (map path->string (directory-list cleaned-dir)))))
(printf "There are ~a cleaned files. Starting to merge these files.\n" (length cleaned-files))
(define cat-cmd
  (format "cat ~a > ~a"
          (string-join cleaned-files " ")
          merged-file))
(system* "/bin/sh" "-c" cat-cmd)

(printf "Merged cleaned files into ~a\n" merged-file)


#|
***
import the equivalence dbk from the merged .jsonl generated from above
***
|#
(define-runtime-path path.here "../../neo-data/")

(define ((json-port-enumerator in) yield)
  (let loop ()
    (let ((line (read-bytes-line in)))
      (cond
        [(eof-object? line) (close-input-port in)]
        #;[(bytes=? #"" line) (loop)]
        [else
         (let* ((js      (bytes->jsexpr line))
                (subject (string->bytes/utf-8 (hash-ref js 'subject)))
                (object  (string->bytes/utf-8 (hash-ref js 'object))))
           (yield (list subject object))
           (loop))]))))

(define db.equiv
  (build-equivalence-database
    (build-path path.here "equivalence.db")
    (json-port-enumerator (open-input-file merged-file))))