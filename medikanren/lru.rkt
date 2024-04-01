#lang racket
(provide
 make-lru
 lru-ref
 )
(require chk)


(define (assert k st)
  (when (not k)
      (raise (format "assertion failure: ~a" st))))

(struct payload (k v))

(struct lrun (
              (older #:mutable)
              payload
              (newer #:mutable)))

(struct lru (
             behind-ref
             hash
             (lrun-oldest #:mutable)
             (lrun-newest #:mutable)
             (num-entries #:mutable)
             num-entries-max))

(define (make-lru behind-ref #:num-entries-max (num-entries-max 1000))
  (lru
   behind-ref
   (make-hash)
   #f
   #f
   0
   num-entries-max))

;;; Add a newest entry to the lru.
(define (lru-put-newest ths payload1)
  (let* (
         (lrun1 (lru-lrun-newest ths))
         (k (payload-k payload1))
         (lrun0 (lrun lrun1 payload1 #f))
         )
    ; connect lrun fields
    ; lrun0.older is already connected
    (set-lrun-older! lrun0 lrun1) 
    (when lrun1
        (set-lrun-newer! lrun1 lrun0))
    ; connect lru fields
    (set-lru-lrun-newest! ths lrun0)
    (when (not (lru-lrun-oldest ths)) ; are we brand new?
        (set-lru-lrun-oldest! ths lrun0))
    (set-lru-num-entries! ths (+ (lru-num-entries ths) 1))
    (hash-set! (lru-hash ths) k lrun0)))

(define (lru-remove ths lrun1)
  (let* (
         (payload1 (lrun-payload lrun1))
         (k (payload-k payload1))
         (lrun0 (lrun-older lrun1))
         (lrun2 (lrun-newer lrun1))
         )
    (if lrun0
        (set-lrun-newer! lrun0 lrun2)
        (begin
          ; we are removing the oldest
          (set-lru-lrun-oldest! ths lrun2)
          (set-lrun-older! lrun2 #f)))
    (if lrun2
        (set-lrun-older! lrun2 lrun0)
        (begin
          ; we are removing the newest
          (set-lru-lrun-newest! ths lrun0) 
          (set-lrun-newer! lrun0 #f)))
    (set-lru-num-entries! ths (- (lru-num-entries ths) 1))
    (hash-remove! (lru-hash ths) k)))


;;; If the lru is full, remove the oldest entry.
(define (lru-evict ths)
  (when (> (lru-num-entries ths) (lru-num-entries-max ths))
      (let* ((lrun1 (lru-lrun-oldest ths)))
        (lru-remove ths lrun1))))

;;; Make the entry with key k the newest entry.
(define (lru-freshen ths k)
  (let* (
         (lrun1 (hash-ref (lru-hash ths) k))
         (payload1 (lrun-payload lrun1))
          )
    (lru-remove ths lrun1)
    (lru-put-newest ths payload1)))


;;; Fetch item from lru cache, or if absent, from ref-behind.
(define (lru-ref ths k)
  (match (hash-ref! (lru-hash ths) k #f)
    (#f
     (let ((v ((lru-behind-ref ths) k)))
       (lru-put-newest ths (payload k v))
       (lru-evict ths)
       v))
    (lrun
     (when (>= (lru-num-entries ths) 2) ; freshen 1 entry is noop
         (lru-freshen ths k))
     (payload-v (lrun-payload lrun)))))



(module+ test
  (define (make-test-hash n)
    (define h (make-hash))
    (for ((i (range n)))
      (hash-set! h i (+ i 100)))
    h)

  ;; do we get a correct value back on a miss?
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (define v (lru-ref l 5))
   #:= v 105)

  ;; do we get a correct value back on a hit?
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:do (define v (lru-ref l 5))
   #:= v 105)

  ;; do we initialize properly?
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (define v (lru-ref l 5))
   #:t (lru-lrun-newest l)
   #:t (lru-lrun-oldest l))

  ;; when the oldest entry becomes the newest entry, is the next eviction correct?
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:do (lru-ref l 7)
   #:do (lru-ref l 5)
   #:do (lru-ref l 8)
   #:t (not (hash-has-key? (lru-hash l) 6)))

  ;; does num-entries grow as expeced?
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:= (lru-num-entries l) 1)

  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:= (lru-num-entries l) 2)

  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:do (lru-ref l 7)
   #:= (lru-num-entries l) 3)

  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define l (make-lru (lambda (k) (hash-ref h1 k #f)) #:num-entries-max 3))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:do (lru-ref l 7)
   #:do (lru-ref l 8)
   #:= (lru-num-entries l) 3)

  ;; do we make the expected number of upstream calls?

  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define num-calls (box 0))
   #:do (define (incr) (set-box! num-calls (+ 1 (unbox num-calls))))
   #:do (define l (make-lru
                   (lambda (k) (incr) (hash-ref h1 k #f))
                   #:num-entries-max 2))
   #:do (lru-ref l 5)
   #:= (unbox num-calls) 1)
  
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define num-calls (box 0))
   #:do (define (incr) (set-box! num-calls (+ 1 (unbox num-calls))))
   #:do (define l (make-lru
                   (lambda (k) (incr) (hash-ref h1 k #f))
                   #:num-entries-max 2))
   #:do (lru-ref l 5)
   #:do (lru-ref l 5)
   #:= (unbox num-calls) 1)
  
  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define num-calls (box 0))
   #:do (define (incr) (set-box! num-calls (+ 1 (unbox num-calls))))
   #:do (define l (make-lru
                   (lambda (k) (incr) (hash-ref h1 k #f))
                   #:num-entries-max 2))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:do (lru-ref l 5)
   #:= (unbox num-calls) 2)

  (chk
   #:do (define h1 (make-test-hash 100))
   #:do (define num-calls (box 0))
   #:do (define (incr) (set-box! num-calls (+ 1 (unbox num-calls))))
   #:do (define l (make-lru
                   (lambda (k) (incr) (hash-ref h1 k #f))
                   #:num-entries-max 2))
   #:do (lru-ref l 5)
   #:do (lru-ref l 6)
   #:do (lru-ref l 7)
   #:do (lru-ref l 5)
   #:= (unbox num-calls) 4)
  )
