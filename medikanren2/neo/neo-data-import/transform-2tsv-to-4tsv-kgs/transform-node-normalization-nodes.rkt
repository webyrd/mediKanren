#lang racket

(require json
         "../../dbKanren/dbk/database.rkt")
(provide transform-node-normalization-nodes)

;; when calling the procedure btree-ref-or-set!
;; it will add x to bt if x does not exist in bt, otherwise bt does not change.
(define (btree-member!? bt x)
  (let ((count (btree-count bt)))
    (not (= count (btree-ref-or-set! bt x)))))


#|
Output node-props file format:
node_normalization.node-props.tsv
:ID propname value

Note: Only ids are preserved in node_normalization.node-props.tsv from nodes.jsonl.
|#

(define transform-node-normalization-nodes
  (lambda ()
    (define nodes-file-import-path "../../neo-data/raw_downloads_from_kge_archive/node_normalization/nodes.jsonl")
    (define node-props-file-export-path "../../neo-data/raw_downloads_from_kge_archive_transformed_to_4tsv/node_normalization/node_normalization.nodeprop.tsv")

    (printf "input nodes jsonl: ~s\n" nodes-file-import-path)
    (printf "output node props tsv: ~s\n" node-props-file-export-path)

    (define node-props-out
      (open-output-file node-props-file-export-path))
    (fprintf node-props-out ":ID\tpropname\tvalue\n")

    (define nodes-in
      (open-input-file nodes-file-import-path))

    (define seen-nodes (make-btree))

    (let loop ((i 0)
               (line-json (read-json nodes-in)))
      (when (zero? (modulo i 100000))
        (printf "processing nodes line ~s\n" i)
        (printf "the count of seen-nodes ~s\n" (btree-count seen-nodes)))

      (cond
        [(eof-object? line-json)
         (close-input-port nodes-in)
         (close-output-port node-props-out)
         (printf "finished processing nodes\n\n")]
        [else
         (let ((id (hash-ref line-json 'id #f)))
           (when id
             (unless (btree-member!? seen-nodes (string->bytes/utf-8 id))
                 (fprintf node-props-out "~a\tname\tnoname\n" id)))
           (loop (add1 i) (read-json nodes-in)))]))))


