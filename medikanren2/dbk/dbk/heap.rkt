#lang racket/base
(provide
  heap-top
  heap!
  heap-remove!
  heap-replace!
  heap-sink!
  heap-add!
  )

(define (heap-top         h)         (vector-ref h 0))
(define (heap!         <? h end)     (let loop ((i (- (quotient end 2) 1)))
                                      (when (<= 0 i)
                                        (heap-sink! <? h end i)
                                        (loop (- i 1)))))
(define (heap-remove!  <? h end)     (vector-set! h 0 (vector-ref h (- end 1))) (heap-sink! <? h (- end 1) 0))
(define (heap-replace! <? h end top) (vector-set! h 0 top)                      (heap-sink! <? h    end    0))
(define (heap-sink!    <? h end i)   (let loop ((i i))
                                      (let ((ileft  (+ i i 1))
                                            (iright (+ i i 2)))
                                        (cond ((<= end ileft))  ; done
                                              ((<= end iright)
                                               (let ((p (vector-ref h i))
                                                     (l (vector-ref h ileft)))
                                                 (when (<? l p)
                                                   (vector-set! h i     l)
                                                   (vector-set! h ileft p))))
                                              (else (let ((p (vector-ref h i))
                                                          (l (vector-ref h ileft))
                                                          (r (vector-ref h iright)))
                                                      (cond ((<? l p) (cond ((<? r l) (vector-set! h i      r)
                                                                                      (vector-set! h iright p)
                                                                                      (loop iright))
                                                                            (else     (vector-set! h i      l)
                                                                                      (vector-set! h ileft  p)
                                                                                      (loop ileft))))
                                                            ((<? r p) (vector-set! h i      r)
                                                                      (vector-set! h iright p)
                                                                      (loop iright)))))))))
(define (heap-add!     <? h end v)   (let loop ((i end))
                                       (if (= i 0)
                                         (vector-set! h i v)
                                         (let* ((iparent (- (quotient (+ i 1) 2) 1))
                                                (pv      (vector-ref h iparent)))
                                           (cond ((<? v pv) (vector-set! h i pv)
                                                            (loop iparent))
                                                 (else      (vector-set! h i v)))))))
