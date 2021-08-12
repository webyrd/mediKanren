#lang racket
(provide
 (struct-out kge-coord-t)
 (struct-out task-build-index-t)
 payload-from-kgec
 kge-coord-mapping
 task-build-index-mapping
 )
(require json-mapping)
(require chk)


; mnemonic: kgec
(struct kge-coord
  (
   kgid
   ver
   day
   ) #:transparent
  #:name kge-coord-t
  ;    #:constructor-name kge-coord-new
  )

(struct task-build-index
  (
   kgec
   ; dbwrapper      ; no: dispatch-build-kg computes this dynamically
   tsec-requested   ; TODO: log
   duration-max     ; TODO: add gnu timeout
   ; see payload-from-kgec: payload          ; TODO: can this be static?
   ver-mi           ; TODO: add to output filename
   ) #:transparent
  #:name task-build-index-t
  ;    #:constructor-name task-build-index-new
  )

(define (payload-from-kgec kgec)
  (format "~a_~a.tar.gz" (kge-coord-kgid kgec) (kge-coord-ver kgec)))

(define kge-coord-mapping
  (json-mapping
   (object kge-coord
           [kgid : string]
           [ver : string]
           [day : string]
           )))

(define task-build-index-mapping
  (json-mapping
   (object task-build-index
           [kgec : kge-coord-mapping] ; for lists: (list kge-coord-mapping)]
           ;[dbwrapper : string]
           [tsec-requested : number]
           [duration-max : number]
           ;[payload : (list string)]
           [ver-mi : string]
           )))

