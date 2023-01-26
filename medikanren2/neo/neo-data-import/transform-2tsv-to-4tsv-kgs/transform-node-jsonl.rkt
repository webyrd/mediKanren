#lang racket

(require json)
(provide transform-node-jsonl)

#|
Output node and node-props file formats:
<name-file>.node.scm
:ID
<name-file>.node-props.scm
:ID propname value
|#

(define transform-node-jsonl
  (lambda (nodes-file-import-path
           node-file-export-path
           node-props-file-export-path)

    (printf "transform-node-jsonl called\n")
    (printf "input nodes jsonl: ~s\n" nodes-file-import-path)
    (printf "output node tsv: ~s\n" node-file-export-path)
    (printf "output node props tsv: ~s\n" node-props-file-export-path)
    
    (define node-out
      (open-output-file node-file-export-path))
    (fprintf node-out ":ID\n")
    (define node-props-out
      (open-output-file node-props-file-export-path))
    (fprintf node-props-out ":ID\tpropname\tvalue\n")

    (define nodes-in
      (open-input-file nodes-file-import-path))

    (let loop ((i 0)
               (seen-nodes (set))
               (line-json (read-json nodes-in)))
      (when (zero? (modulo i 100000))
        (printf "processing nodes line ~s\n" i))

      (cond
        [(eof-object? line-json)
         (close-input-port nodes-in)
         (close-output-port node-out)
         (close-output-port node-props-out)
         (printf "finished processing nodes\n\n")]
        [else
         (let ((id (hash-ref line-json 'id #f)))
           (when id
             (when (set-member? seen-nodes id)
               (error 'make-kg-node (format "already seen node: ~a" id)))
             (fprintf node-out "~a\n" id)
             ;; TODO: the value of equivalent_identifiers is a list of curies '(GO:0097172)
             ;; what does it like in the node-props.scm of rtx-kg2?
             (let loop-inner ((propnames (remove 'id (hash-keys line-json))))
               (when (not (null? propnames)) 
                 (let* ((propname (car propnames))
                        (value (hash-ref line-json propname #f)))
                   (unless (equal? "" value)
                     (fprintf node-props-out "~a\t~a\t~a\n" id propname value)))
                 (loop-inner (cdr propnames)))))
           (loop
            (add1 i)
            (set-add seen-nodes id)
            (read-json nodes-in)))]))))
