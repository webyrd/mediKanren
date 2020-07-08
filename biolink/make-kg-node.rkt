#lang racket
(provide (all-defined-out))

#|
name-file.node.scm
:ID

name-file.node-props.scm
:ID propname value
|#


(define directory-path
  "data/covid19/"
  ;; "data/textminingprovider/"
  )
(define nodes-file
  "merged-kg_nodes.tsv"
  ;; "sample-craft-nodes.v0.1.kgx.tsv"
  )
(define export-path directory-path)
(define node-export-path
  (format
   "~acovid19.node.tsv"
   ;; "~atextminingprovider.node.tsv"
   export-path))
(define node-props-export-path
  (format
   "~acovid19.nodeprop.tsv"
   ;; "~atextminingprovider.nodeprop.tsv"
   export-path))

(define nodes-export-file
  (open-output-file node-export-path))
(fprintf nodes-export-file ":ID\n")
(define node-props-export-file
  (open-output-file node-props-export-path))
(fprintf node-props-export-file ":ID\tpropname\tvalue\n")
(define input-nodes
  (open-input-file (format "~a~a" directory-path nodes-file)))
(let* ((header (read-line input-nodes))
       (header (string-split header "\t" #:trim? #f)))
  (let loop ((seen-nodes (set))
             (line-str (read-line input-nodes)))
    (cond
      ((eof-object? line-str)
       (close-input-port input-nodes)
       (close-output-port nodes-export-file)
       (close-output-port node-props-export-file))
      (else
        (let* ((line (string-split line-str "\t" #:trim? #f))
               (node (car line)))
          (when (set-member? seen-nodes node)
            (error 'make-kg-node (format "already seen node: ~a" node)))
          (fprintf nodes-export-file "~a\n" node)
          (let loop-inner ((props (cdr line))
                           (headers (cdr header)))
            (when (not (null? props))
              (unless (string=? "" (car props))
                (fprintf node-props-export-file "~a\t~a\t~s\n" node (car headers) (car props)))
              (loop-inner (cdr props) (cdr headers))))
          (loop
            (set-add seen-nodes node)
            (read-line input-nodes)))))))
