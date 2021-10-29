#lang racket/base
(provide synonym)

(require json "../base.rkt" (except-in racket/match ==))


(define edge-keys '(subject object))

(define kgid 'kgx-synonym)

(define-relation/table synonym
  'path                 (if (cfg:config-ref 'migrated-to-new-db-versioning)
                          (path-for-database kgid 'synonym)
                          "kgx-synonym/010")
  'attribute-names edge-keys
  'attribute-types '(string string)
  'indexes '((object subject))
  'source-stream   (value/syntax (source-stream-by-lines-from-strelpath (source-path-for-database kgid "KGX_NN_data_edges.jsonl")))
  'attribute-names    edge-keys
  'map                (value/syntax
                        (lambda (js)
                          (define jsexpr (string->jsexpr js))
                          (map (lambda (k) (hash-ref jsexpr k)) edge-keys)))

  )

(database-extend-relations!
  'kgx-synonym
  'synonym synonym)
