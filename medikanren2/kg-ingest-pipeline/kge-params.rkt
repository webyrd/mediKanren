#lang racket
(provide
    kge-token
    with-kge-token)
(require "current-source.rkt")
(require json)

(define kge-token (make-parameter 'kge-token-placeholder))

(define (with-kge-token thunk-run)
  (define afile-token 
    (simplify-path (build-path (adir-current-source) 'up 'up 'up "browserbot_kge_client" "session_token.json")))
  (define st-token (file->string afile-token))
  (define jsexpr-token (dict-ref (string->jsexpr st-token) 'value))
  (parameterize ((kge-token jsexpr-token))
    (thunk-run)))

