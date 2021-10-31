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
  ;*** cook like this ***
  ;    (able to open stream but not able to test existence)
  ;'source-stream   (value/syntax (source-stream-by-lines-from-strelpath (source-path-for-database kgid "KGX_NN_data_edges.jsonl")))

  ;*** experiments ***
  ;'source-file-path (in:file (source-path-for-database kgid "KGX_NN_data_edges.jsonl") 'format 'jsonl)
  ;'source-file-path (source-stream-by-lines-from-strelpath (source-path-for-database kgid "KGX_NN_data_edges.jsonl"))

  ;*** consume like this (points to nonexistent upstream file) ***
  ;    (able to test nonexistence and move on, but not able to build)
  'source-file-path (source-path-for-database kgid "KGX_NN_data_edges.jsonl")
  'attribute-names    edge-keys
  'map                (value/syntax
                        (lambda (js)
                          (define jsexpr (string->jsexpr js))
                          (map (lambda (k) (hash-ref jsexpr k)) edge-keys)))

  )

(database-extend-relations!
  'kgx-synonym
  'synonym synonym)
