#lang racket/base

(provide transform-generic)

;; transform-node-and-edge is a pair (transform-node . transform-edge) containing
;; - a transform node function/procedure (transform-node-jsonl or transform-node-tsv)
;; - a transform edge function/procedure (transform-edge-jsonl or transform-edge-tsv)

(define transform-generic
  (lambda (import-directory-path
           export-directory-path
           nodes-import-file-name
           edges-import-file-name
           export-base-file-name
           transform-node-and-edge)
    
    (define node-export-file-name
      (string-append
       export-base-file-name
       ".node.tsv"))
    (define node-props-export-file-name
      (string-append
       export-base-file-name
       ".nodeprop.tsv"))

    (define edge-export-file-name
      (string-append
       export-base-file-name
       ".edge.tsv"))
    (define edge-props-export-file-name
      (string-append
       export-base-file-name
       ".edgeprop.tsv"))
    (define qualified-edge-export-file-name
      (string-append
       export-base-file-name
       ".qualifiededge.tsv"))

        ;; --- nodes ---
    (define nodes-file-import-path
      (string-append
       import-directory-path
       nodes-import-file-name))

    (define node-file-export-path
      (string-append
       export-directory-path
       node-export-file-name))
    (define node-props-file-export-path
      (string-append
       export-directory-path
       node-props-export-file-name))

        ;; --- edges ---
    (define edges-file-import-path
      (string-append
       import-directory-path
       edges-import-file-name))

    (define edge-file-export-path
      (string-append
       export-directory-path
       edge-export-file-name))
    (define edge-props-file-export-path
      (string-append
       export-directory-path
       edge-props-export-file-name))
    (define qualified-edge-file-export-path
      (string-append
       export-directory-path
       qualified-edge-export-file-name))

    (let ((transform-node (car transform-node-and-edge))
          (transform-edge (cdr transform-node-and-edge)))
      (transform-node nodes-file-import-path
                      node-file-export-path
                      node-props-file-export-path)

      (transform-edge edges-file-import-path
                      edge-file-export-path
                      edge-props-file-export-path
                      qualified-edge-file-export-path)

    )))