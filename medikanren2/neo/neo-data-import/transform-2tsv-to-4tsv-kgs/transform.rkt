#lang racket
(provide transform)
(require "transform-node-tsv.rkt"
         "transform-edge-tsv.rkt")

(define transform
  (lambda (import-directory-path
           export-directory-path
           nodes-import-file-name
           edges-import-file-name
           export-base-file-name)

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

    (transform-node-tsv nodes-file-import-path
                        node-file-export-path
                        node-props-file-export-path)

    (transform-edge-tsv edges-file-import-path
                        edge-file-export-path
                        edge-props-file-export-path)

    ))

