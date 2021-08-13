#lang racket
(provide
 (all-defined-out))
(require http/request)
(require http/head)
(require json)
(require racket/dict)
(require "metadata.rkt")
(require "../db/dispatch-build-kg-indexes.rkt")
(require "process-tbi.rkt")
(require "cmd-helpers.rkt")
(require "current-source.rkt")
(require "main-params.rkt")
(require "kge-params.rkt")

(require chk)

(define (uri-kge-base) (dict-ref (config) 'uri-kge))

(define (uri-kge path) (format "~a/~a" (uri-kge-base) path))

(define path-catalog "archive/catalog")

(define (path-version-metadata kgid ver)
  ;; example:
  ;;   archive/yeast-sri-reference-kg-tsv/1.0/metadata
  (format "archive/~a/~a/metadata" kgid ver))

(define (path-version-download kgid ver)
  ;; example:
  ;;   archive/yeast-sri-reference-kg-tsv/1.0/download
  (format "archive/~a/~a/download" kgid ver))

;; Based on https://lisp.sh/fetch-a-url/
(define (http-request uri
                      #:redirects [redirects 10]
                      #:http-version [http-version "1.1"]
                      #:method [method "GET"]
                      #:data [data #""]
                      #:data-length [data-length #f]
                      #:heads [heads empty]
                      #:entity-reader [entity-reader read-entity/bytes])
  (if (and (bytes? data)
           (bytes=? data #"")
           (eq? data-length #f))
      (call/input-request http-version
                          method
                          uri
                          heads
                          entity-reader
                          #:redirects redirects)
      (call/output-request http-version
                           method
                           uri
                           data
                           data-length
                           heads
                           entity-reader
                           #:redirects redirects)))

(define (log-thunk fn name-fn . args)
  (writeln `(
             (event . begin)
             (method-name . ,name-fn)
             (args . ,args)))
  (let* ((
          v (fn)
          ))
    (writeln `(
               (event . end)
               (method-name . ,name-fn)
               (args . ,args)
               (result . ,v)))
    v
    ))

(define (headers-kge)
  `((Cookie . ,(format "AIOHTTP_SESSION=~a" (kge-token)))))

(define (fetch-kge uri #:redirects [redirects 10])
  (unless (string? (kge-token))
    (error "Must specify a session token for kge via kge-token"))
  (log-thunk
   (lambda ()
     (http-request (uri-kge uri)
                   #:heads (headers-kge)
                   #:redirects redirects))
   'http-request
   uri))

(define (parse-kge-versions jsex)
  (append-map
   (lambda (kgid)
     (for/list ((ver (dict-ref (dict-ref jsex kgid) 'versions)))
       `(idver ,(symbol->string kgid) ,ver)))
   (dict-keys jsex)))


(define (fetch-recent-kge-versions)
  (let* (
         (response (fetch-kge path-catalog))
         (jsex (bytes->jsexpr response))
         )
    (parse-kge-versions
     jsex)))

(define (fetch-recent-kgmeta idvers)
  (for/list ((idver idvers))
    (match idver
      (`(idver ,kgid ,ver)
       (let* (
              (response (fetch-kge (path-version-metadata kgid ver)))
              (jsex (bytes->jsexpr response))
              )
         ;(displayln (bytes->string/utf-8 response))
         jsex)))))

(define (ver-from-kgmeta kgmeta)
  (dict-ref (dict-ref kgmeta 'fileset) 'fileset_version))

(define (kgid-from-kgmeta kgmeta)
  ;(printf "kgid-from-kgmeta kgmeta=~a\n" kgmeta)
  (dict-ref (dict-ref kgmeta 'provider) 'kg_id))

(define (day-from-kgmeta kgmeta)
  (dict-ref (dict-ref kgmeta 'fileset) 'date_stamp))

(define (tbi-from-kgmeta kgmeta)
  (define tsec-requested (floor (/ (current-milliseconds) 1000)))
  (define duration-max 3600)
  (define ver-mi "2.0")
  (task-build-index
   (kge-coord
    (kgid-from-kgmeta kgmeta) ;"yeast-sri-reference-kg"    ; kgid
    (ver-from-kgmeta kgmeta)  ;"1.0"                       ; ver
    (day-from-kgmeta kgmeta)  ;"2021-08-01"                ; day
    )
   tsec-requested
   duration-max
   ver-mi
   ))

(define (tbi-known-format? tbi)
  (define kgec (task-build-index-kgec tbi))
  (match (dispatch-build-kg (kge-coord-kgid kgec) (kge-coord-ver kgec))
    ('unknown-format #f)
    ; TODO: fail fast if expected keys are not present?
    (else #t)))


(define (fetch-payload-to-disk tbi)
  (define kgec (task-build-index-kgec tbi))
  (define kgid (kge-coord-kgid kgec))
  (define ver (kge-coord-ver kgec))
  (define uri (format "~a/~a" (uri-kge-base) (path-version-download kgid ver)))
  (define afile-archive (path->string (build-path (adir-temp) (payload-from-kgec kgec))))
  (printf "will output file to ~a\n" afile-archive)
  (define cmds
    `((() () ("curl" ,uri
                     "-H" ,(format "Cookie: AIOHTTP_SESSION=~a" (kge-token))
                     "--compressed" "--location" "--output" ,afile-archive))
      (() () ("ls" "-l" ,(adir-temp)))))
  (run-cmds cmds))



(module+ test
  (chk
   #:do (define kges (parse-kge-versions 
                      `#hasheq(
                               (semantic-medline-database . #hasheq((name . "Semantic Medline Database") (versions . ())))
                               (yeast-sri-reference-kg-tsv . #hasheq((name . "yeast-sri-reference-kg-tsv") (versions . ("1.0")))))))
   #:do (displayln kges)
   #:= (length kges) 1)
  
  (define kgmeta1
    `#hasheq(
             (fileset . #hasheq(
                                (biolink_model_release . "2.2.0")
                                (date_stamp . "2021-07-28")
                                (files . (
                                          #hasheq((original_name . "yeast-sri-reference-kg-tsv_1.0.tar.gz"))
                                          #hasheq((original_name . "edges.tsv"))
                                          #hasheq((original_name . "nodes_edges.tsv"))
                                          #hasheq((original_name . "nodes.tsv"))
                                          ))
                                (fileset_version . "1.0")
                                (size . 3025.872700691223)
                                (status . "Loaded")
                                ))
             (provider . #hasheq(
                                 (kg_description . "    A reduced version of sri-reference-kg made to fit in indexed form within the quota of Github Actions, made by focusing on the nodes most related to model organism S. Cerevisiae and its SGD database (https://www.yeastgenome.org/).  Used in the mediKanren continuous integration suite.\n    \n    The upstream input file:\n    \n        3f8f7c80dc9c39dc291468fc1ab6ae9b80b5630c  sri-reference-kg-0.3.0.tar.gz\n    \n    with contents:\n    \n        6180716630 Oct 28  2020 sri-reference-kg-0.3.0_edges.tsv\n        3103906077 Oct 28  2020 sri-reference-kg-0.3.0_nodes.tsv\n    \n    Has been postprocessed with the following two scripts:\n    \n        https://github.com/webyrd/mediKanren/blob/e642bff12afc5abf7f9b737203454b1bfc0374ec/medikanren2/util/data-import-workaround/remove_cr.pl\n        https://github.com/webyrd/mediKanren/blob/e642bff12afc5abf7f9b737203454b1bfc0374ec/medikanren2/util/storage-size-workaround/s-cerevisiae-kg-ref-ll.py\n    \n    To produce the data given.")
                                 (kg_id . "yeast-sri-reference-kg-tsv") 
                                 (kg_name . "yeast-sri-reference-kg-tsv") 
                                 (license_name . "Creative-Commons-4.0") 
                                 (license_url . "https://creativecommons.org/licenses/by/4.0/legalcode") 
                                 (terms_of_service . "https://en.wikipedia.org/wiki/As_is") 
                                 (translator_component . "ARA") 
                                 (translator_team . "Unsecret Agent")))))
  (chk
   (#:= (kgid-from-kgmeta kgmeta1) "yeast-sri-reference-kg-tsv"))
  (chk
   (#:= (ver-from-kgmeta kgmeta1) "1.0"))
  (chk
   (#:do (tbi-from-kgmeta kgmeta1))
   (#:t #t))
  )
