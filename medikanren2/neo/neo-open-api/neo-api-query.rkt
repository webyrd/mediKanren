#lang racket
(provide
 ;;
 api-query
 path.query
 ;;
 url.genetics.prod
 url.genetics.test
 url.genetics.ci
 ;;
 )
(require
 json
 net/url)

(define url.genetics.prod
  "https://genetics-kp.transltr.io/genetics_provider/trapi/v1.3")
(define url.genetics.test
  "https://genetics-kp.test.transltr.io/genetics_provider/trapi/v1.3")
(define url.genetics.ci
  "https://genetics-kp.ci.transltr.io/genetics_provider/trapi/v1.3")

(define path.query
  "/query")


(define (api-query url-string (optional-post-jsexpr (void)))
  (printf "##### making api-query call to url:\n~s\n" url-string) 
  (define-values (status headers in)
    (time (if (void? optional-post-jsexpr)
              (http-sendrecv/url
               (string->url url-string)
               #:method "GET")
              (http-sendrecv/url
               (string->url url-string)
               #:method "POST"
               #:data (jsexpr->string optional-post-jsexpr)
               #:headers '("Content-Type: application/json; charset=utf-8")))))
  (define response-string (time (port->string in)))
  (printf "status:\n~s\n" status)
  (define OK-status? (bytes=? #"HTTP/1.1 200 OK" status))
  (if OK-status?
      (hash 'status status
            'headers headers
            'response (string->jsexpr response-string))
      (begin
        (printf "!!!!!! unexpected non-OK status returned from ~s:\n" url-string)
        (pretty-print `(,status ,headers ,response-string))
        (hash 'status status
              'headers headers
              'response (hash)))))
(define (js-query edges nodes)
  (hash 'message
        (hash 'query_graph
              (hash 'edges edges
                    'nodes nodes))))
