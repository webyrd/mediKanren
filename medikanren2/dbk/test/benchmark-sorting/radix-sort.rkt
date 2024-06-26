#lang racket/base
(require racket/fixnum racket/match racket/vector racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vector->enumerator v (start 0) (end (vector-length v)))
  (define len (min end (vector-length v)))
  (unsafe-vector->enumerator v (min start len) len))

(define ((unsafe-vector->enumerator v (start 0) (end (unsafe-vector*-length v))) k!)
  (let loop ((i start))
    (when (unsafe-fx< i end)
      (k!   (unsafe-vector*-ref v i))
      (loop (unsafe-fx+ i 1)))))

(define ((enumerator-append e.0 e.1) k!)
  (e.0 k!)
  (e.1 k!))

(define ((enumerator-msd-radix-sort en t->key key-byte-count) k!)
  (define size.shift           8)
  (define mask.shift         255)
  (define count.parts        256)
  (define count.buffer        32)
  (define count.initial.parts  8)
  (define growth-factor.parts  2)
  (unless (fixnum? key-byte-count) (error "key-byte-count must be a fixnum" key-byte-count))
  (let enumerate ((shift (unsafe-fx* 8 (unsafe-fx- key-byte-count 1))) (en en))
    (define parts     (make-vector count.parts))
    (define pos.parts (make-vector count.parts 0))
    (define buf       (make-vector count.parts))
    (define pos       (make-vector count.parts 0))
    (let loop ((i 0))
      (when (unsafe-fx< i count.parts)
        (unsafe-vector*-set! parts i (make-vector count.initial.parts))
        (unsafe-vector*-set! buf   i (make-vector count.buffer))
        (loop (unsafe-fx+ i 1))))
    (en (lambda (t)
          ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
          (let* ((key     (unsafe-fxand mask.shift (fxrshift (t->key t) shift)))
                 (buf.k   (unsafe-vector*-ref buf key))
                 (pos.k   (unsafe-vector*-ref pos key))
                 (pos.k+1 (unsafe-fx+ pos.k 1)))
            (unsafe-vector*-set! buf.k pos.k t)
            (if (unsafe-fx= pos.k+1 count.buffer)
              (begin
                (unsafe-vector*-set! pos key 0)
                (unsafe-vector*-set! buf key (make-vector count.buffer))
                (let* ((parts.k       (unsafe-vector*-ref parts     key))
                       (pos.parts.k   (unsafe-vector*-ref pos.parts key))
                       (pos.parts.k+1 (unsafe-fx+ pos.parts.k 1))
                       (len.parts.k   (unsafe-vector*-length parts.k)))
                  (unsafe-vector*-set! pos.parts key pos.parts.k+1)
                  (if (unsafe-fx= pos.parts.k+1 len.parts.k)
                    (let ((parts.k.new (make-vector (unsafe-fx* len.parts.k growth-factor.parts))))
                      (unsafe-vector*-set!  parts key parts.k.new)
                      (vector-copy! parts.k.new 0 parts.k)
                      (unsafe-vector*-set!  parts.k.new pos.parts.k buf.k))
                    (unsafe-vector*-set! parts.k pos.parts.k buf.k))))
              (unsafe-vector*-set! pos key pos.k+1)))))
    (let ((k!/en (if (unsafe-fx= shift 0)
                   (lambda (en) (en k!))
                   (lambda (en) (enumerate (unsafe-fx- shift size.shift) en)))))
      (let loop ((i 0))
        (when (unsafe-fx< i count.parts)
          (let ((parts.i     (unsafe-vector*-ref parts     i))
                (pos.parts.i (unsafe-vector*-ref pos.parts i))
                (buf.i       (unsafe-vector*-ref buf       i))
                (pos.i       (unsafe-vector*-ref pos       i)))
            (unsafe-vector*-set! parts i #f)
            (unsafe-vector*-set! buf   i #f)
            (k!/en (lambda (k!)
                     ((unsafe-vector->enumerator parts.i 0 pos.parts.i)
                      (lambda (part) ((unsafe-vector->enumerator part 0 count.buffer) k!)))
                     ((unsafe-vector->enumerator buf.i 0 pos.i) k!)))
            (loop (unsafe-fx+ i 1))))))))

(define (vector-radix-sort! v t->key key-byte-count (start 0) (end (vector-length v)))
  (define len (min end (vector-length v)))
  (unsafe-vector-radix-sort! v t->key key-byte-count (min start len) len))

(define (unsafe-vector-radix-sort! v t->key key-byte-count (start 0) (end (unsafe-vector*-length v)))
  (define size.shift               8)
  (define mask.shift             255)
  (define count.parts            256)
  (define count.buffer           512)
  (define count.initial.parts      8)
  (define growth-factor.parts      2)
  (define count.initial.buffers 1024)
  (define growth-factor.buffers    2)
  (unless (fixnum? key-byte-count) (error "key-byte-count must be a fixnum" key-byte-count))
  (define shift.final (unsafe-fx* 8 (unsafe-fx- key-byte-count 1)))
  (define buffers     (make-vector count.initial.buffers))
  (define pos.buffers 0)
  (define (new-buffer)
    (if (unsafe-fx= pos.buffers 0)
      (make-vector count.buffer)
      (begin (set! pos.buffers (unsafe-fx- pos.buffers 1))
             (unsafe-vector*-ref buffers pos.buffers))))
  (define (free-buffer b)
    (vector-fill! b 0)
    (when (unsafe-fx= pos.buffers (unsafe-vector*-length buffers))
      (let ((buffers.new (make-vector (unsafe-fx* pos.buffers growth-factor.buffers))))
        (vector-copy! buffers.new 0 buffers)
        (set! buffers buffers.new)))
    (unsafe-vector*-set! buffers pos.buffers b)
    (set! pos.buffers (unsafe-fx+ pos.buffers 1)))
  (let loop ((shift 0))
    (define parts     (make-vector count.parts))
    (define pos.parts (make-vector count.parts 0))
    (define buf       (make-vector count.parts))
    (define pos       (make-vector count.parts 0))
    (let loop ((i 0))
      (when (unsafe-fx< i count.parts)
        (unsafe-vector*-set! parts i (make-vector count.initial.parts))
        (unsafe-vector*-set! buf   i (new-buffer))
        (loop (unsafe-fx+ i 1))))
    (let loop ((i start))
      (when (unsafe-fx< i end)
        (let* ((t       (unsafe-vector*-ref v i))
               ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
               (key     (unsafe-fxand mask.shift (fxrshift (t->key t) shift)))
               (buf.k   (unsafe-vector*-ref buf key))
               (pos.k   (unsafe-vector*-ref pos key))
               (pos.k+1 (unsafe-fx+ pos.k 1)))
          (unsafe-vector*-set! buf.k pos.k t)
          (if (unsafe-fx= pos.k+1 count.buffer)
            (begin
              (unsafe-vector*-set! pos key 0)
              (unsafe-vector*-set! buf key (new-buffer))
              (let* ((parts.k       (unsafe-vector*-ref parts     key))
                     (pos.parts.k   (unsafe-vector*-ref pos.parts key))
                     (pos.parts.k+1 (unsafe-fx+ pos.parts.k 1))
                     (len.parts.k   (unsafe-vector*-length parts.k)))
                (unsafe-vector*-set! pos.parts key pos.parts.k+1)
                (if (unsafe-fx= pos.parts.k+1 len.parts.k)
                  (let ((parts.k.new (make-vector (unsafe-fx* len.parts.k growth-factor.parts))))
                    (unsafe-vector*-set!  parts key parts.k.new)
                    (vector-copy! parts.k.new 0 parts.k)
                    (unsafe-vector*-set!  parts.k.new pos.parts.k buf.k))
                  (unsafe-vector*-set! parts.k pos.parts.k buf.k))))
            (unsafe-vector*-set! pos key pos.k+1)))
        (loop (unsafe-fx+ i 1))))
    (let loop.all ((k 0) (j start))
      (when (unsafe-fx< k count.parts)
        (let ((parts.k     (unsafe-vector*-ref parts     k))
              (pos.parts.k (unsafe-vector*-ref pos.parts k))
              (buf.k       (unsafe-vector*-ref buf       k))
              (pos.k       (unsafe-vector*-ref pos       k)))
          (let loop ((i 0) (j j))
            (if (unsafe-fx< i pos.parts.k)
              (let ((part (unsafe-vector*-ref parts.k i)))
                (let loop ((i 0))
                  (when (unsafe-fx< i count.buffer)
                    (unsafe-vector*-set! v (unsafe-fx+ j i) (unsafe-vector*-ref part i))
                    (loop (unsafe-fx+ i 1))))
                (free-buffer part)
                (loop (unsafe-fx+ i 1) (unsafe-fx+ j count.buffer)))
              (let loop ((i 0))
                (if (unsafe-fx= i pos.k)
                  (begin (free-buffer buf.k)
                         (loop.all (unsafe-fx+ k 1) (unsafe-fx+ j pos.k)))
                  (begin (unsafe-vector*-set! v (unsafe-fx+ j i) (unsafe-vector*-ref buf.k i))
                         (loop (unsafe-fx+ i 1))))))))))
    (when (unsafe-fx< shift shift.final)
      (loop (unsafe-fx+ shift size.shift)))))

(define (counting-radix-sort-helper v.src start.src end.src v.0 start.0 v.1 start.1 t->key key-byte-count)
  (define size.shift    8)
  (define mask.shift  255)
  (define count.parts 256)
  (unless (fixnum? key-byte-count) (error "key-byte-count must be a fixnum" key-byte-count))
  (define len         (unsafe-fx- end.src start.src))
  (define shift.final (unsafe-fx* 8 (unsafe-fx- key-byte-count 1)))
  (define offset      (make-vector count.parts))
  (let loop.shift ((shift      0)
                   (v.src      v.src) (start          start.src) (end          end.src)
                   (v.tgt      v.0)   (start.tgt      start.0)   (end.tgt      (unsafe-fx+ start.0 len))
                   (v.tgt.next v.1)   (start.tgt.next start.1)   (end.tgt.next (unsafe-fx+ start.1 len)))
    (vector-fill! offset 0)
    (let loop.count ((i start))
      (when (unsafe-fx< i end)
        ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
        (let ((key (unsafe-fxand mask.shift (fxrshift (t->key (unsafe-vector*-ref v.src i)) shift))))
          (unsafe-vector*-set! offset key (unsafe-fx+ (unsafe-vector*-ref offset key) 1)))
        (loop.count (unsafe-fx+ i 1))))
    (let loop.offset ((k 0) (current start.tgt))
      (when (unsafe-fx< k count.parts)
        (let ((cardinality (unsafe-vector*-ref offset k)))
          (unsafe-vector*-set! offset k current)
          (loop.offset (unsafe-fx+ k 1) (unsafe-fx+ current cardinality)))))
    (let loop.copy ((i start))
      (when (unsafe-fx< i end)
        ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
        (let* ((t     (unsafe-vector*-ref v.src i))
               (key   (unsafe-fxand mask.shift (fxrshift (t->key t) shift)))
               (off.k (unsafe-vector*-ref offset key)))
          (unsafe-vector*-set! offset key (unsafe-fx+ off.k 1))
          (unsafe-vector*-set! v.tgt off.k t))
        (loop.copy (unsafe-fx+ i 1))))
    (when (unsafe-fx< shift shift.final)
      (loop.shift (unsafe-fx+ shift size.shift)
                  v.tgt      start.tgt      end.tgt
                  v.tgt.next start.tgt.next end.tgt.next
                  v.tgt      start.tgt      end.tgt))))

(define (counting-radix-sort! v t->key key-byte-count (start 0) (end (vector-length v)))
  (define len         (- end start))
  (define v.workspace (make-vector len))
  (counting-radix-sort-helper v start end v.workspace 0 v start t->key key-byte-count)
  (when (odd? key-byte-count)
    (vector-copy! v start v.workspace 0 len)))

(define (counting-radix-sort v t->key key-byte-count (start 0) (end (vector-length v)))
  (define len           (- end start))
  (define v.workspace.0 (make-vector len))
  (define v.workspace.1 (make-vector len))
  (counting-radix-sort-helper v start end v.workspace.0 0 v.workspace.1 0 t->key key-byte-count)
  (if (odd? key-byte-count)
    v.workspace.0
    v.workspace.1))

(define ((enumerator-lsd-radix-sort en t->key key-byte-count) k!)
  (define size.shift               8)
  (define mask.shift             255)
  (define count.parts            256)
  (define count.buffer           512)
  (define count.initial.parts      8)
  (define growth-factor.parts      2)
  (define count.initial.buffers 1024)
  (define growth-factor.buffers    2)
  (unless (fixnum? key-byte-count) (error "key-byte-count must be a fixnum" key-byte-count))
  (define shift.final (unsafe-fx* 8 (unsafe-fx- key-byte-count 1)))
  (define buffers     (make-vector count.initial.buffers))
  (define pos.buffers 0)
  (define (new-buffer)
    (if (unsafe-fx= pos.buffers 0)
      (make-vector count.buffer)
      (begin (set! pos.buffers (unsafe-fx- pos.buffers 1))
             (unsafe-vector*-ref buffers pos.buffers))))
  (define (free-buffer b)
    (vector-fill! b 0)
    (when (unsafe-fx= pos.buffers (unsafe-vector*-length buffers))
      (let ((buffers.new (make-vector (unsafe-fx* pos.buffers growth-factor.buffers))))
        (vector-copy! buffers.new 0 buffers)
        (set! buffers buffers.new)))
    (unsafe-vector*-set! buffers pos.buffers b)
    (set! pos.buffers (unsafe-fx+ pos.buffers 1)))
  (let enumerate ((shift 0) (en en))
    (define parts     (make-vector count.parts))
    (define pos.parts (make-vector count.parts 0))
    (define buf       (make-vector count.parts))
    (define pos       (make-vector count.parts 0))
    (let loop ((i 0))
      (when (unsafe-fx< i count.parts)
        (unsafe-vector*-set! parts i (make-vector count.initial.parts))
        (unsafe-vector*-set! buf   i (new-buffer))
        (loop (unsafe-fx+ i 1))))
    (en (lambda (t)
          ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
          (let* ((key     (unsafe-fxand mask.shift (fxrshift (t->key t) shift)))
                 (buf.k   (unsafe-vector*-ref buf key))
                 (pos.k   (unsafe-vector*-ref pos key))
                 (pos.k+1 (unsafe-fx+ pos.k 1)))
            (unsafe-vector*-set! buf.k pos.k t)
            (if (unsafe-fx= pos.k+1 count.buffer)
              (begin
                (unsafe-vector*-set! pos key 0)
                (unsafe-vector*-set! buf key (new-buffer))
                (let* ((parts.k       (unsafe-vector*-ref parts     key))
                       (pos.parts.k   (unsafe-vector*-ref pos.parts key))
                       (pos.parts.k+1 (unsafe-fx+ pos.parts.k 1))
                       (len.parts.k   (unsafe-vector*-length parts.k)))
                  (unsafe-vector*-set! pos.parts key pos.parts.k+1)
                  (if (unsafe-fx= pos.parts.k+1 len.parts.k)
                    (let ((parts.k.new (make-vector (unsafe-fx* len.parts.k growth-factor.parts))))
                      (unsafe-vector*-set!  parts key parts.k.new)
                      (vector-copy! parts.k.new 0 parts.k)
                      (unsafe-vector*-set!  parts.k.new pos.parts.k buf.k))
                    (unsafe-vector*-set! parts.k pos.parts.k buf.k))))
              (unsafe-vector*-set! pos key pos.k+1)))))
    (define (en.new k!)
      (let loop ((i 0))
        (when (unsafe-fx< i count.parts)
          (let ((parts.i     (unsafe-vector*-ref parts     i))
                (pos.parts.i (unsafe-vector*-ref pos.parts i))
                (buf.i       (unsafe-vector*-ref buf       i))
                (pos.i       (unsafe-vector*-ref pos       i)))
            (let loop ((i 0))
              (if (unsafe-fx= i pos.parts.i)
                (begin ((unsafe-vector->enumerator buf.i 0 pos.i) k!)
                       (free-buffer buf.i))
                (begin (let ((part (unsafe-vector*-ref parts.i i)))
                         ((unsafe-vector->enumerator part 0 count.buffer) k!)
                         (free-buffer part))
                       (loop (unsafe-fx+ i 1))))))
          (loop (unsafe-fx+ i 1)))))
    (if (unsafe-fx= shift shift.final)
      (en.new k!)
      (enumerate (unsafe-fx+ shift size.shift) en.new))))

(define (in-place-radix-sort! v t->key key-byte-count (start 0) (end (vector-length v)))
  (define threshold 268435456)
  ;(define threshold 67108864)
  ;(define threshold 16777216)
  ;(define threshold  65535)
  ;(define threshold  8192)
  ;(define threshold  4096)
  ;(define threshold  1024)
  ;(define threshold  256)
  ;(define threshold  0)
  (define size.shift    8)
  (define mask.shift  255)
  (define count.parts 256)
  (unless (fixnum? key-byte-count) (error "key-byte-count must be a fixnum" key-byte-count))
  (let loop.shift ((shift (unsafe-fx* 8 (unsafe-fx- key-byte-count 1))) (start start) (end end))
    ;; TODO: we can use unsafe-fxrshift if t->key is guaranteed to produce a fixnum
    (define (t->byte t) (unsafe-fxand mask.shift (fxrshift (t->key t) shift)))
    (define count  (make-vector count.parts))
    (define offset (make-vector count.parts))
    (let loop.count ((i start))
      (when (unsafe-fx< i end)
        (let ((key (t->byte (unsafe-vector*-ref v i))))
          (unsafe-vector*-set! count key (unsafe-fx+ (unsafe-vector*-ref count key) 1)))
        (loop.count (unsafe-fx+ i 1))))
    (let loop.offset ((k 0) (current start))
      (when (unsafe-fx< k count.parts)
        (let ((cardinality (unsafe-vector*-ref count k)))
          (let loop.advance ((i current))
            (if (and (unsafe-fx< i end) (unsafe-fx= (t->byte (unsafe-vector*-ref v i)) k))
              (loop.advance (unsafe-fx+ i 1))
              (let ((next (unsafe-fx+ current cardinality)))
                (unsafe-vector*-set! offset k i)
                (unsafe-vector*-set! count  k next)
                (loop.offset (unsafe-fx+ k 1) next)))))))
    (let loop.move.k ((k 0) (i.part.start start))
      (when (unsafe-fx< k count.parts)
        (let* ((i.start (unsafe-vector*-ref offset k))
               (i.end   (unsafe-vector*-ref count k)))
          (let loop.move ((i i.start))
            (when (unsafe-fx< i i.end)
              (let ((t (unsafe-vector*-ref v i)))
                (let loop.swap ((t.current t)
                                (key       (t->byte t)))
                  (let* ((off.k (unsafe-vector*-ref offset key))
                         (t.next (unsafe-vector*-ref v off.k)))
                    (unsafe-vector*-set! v      off.k t.current)
                    (unsafe-vector*-set! offset key   (unsafe-fx+ off.k 1))
                    (let ((key (t->byte t.next)))
                      (if (unsafe-fx= key k)
                        (begin (unsafe-vector*-set! v i t.next)
                               (loop.move (unsafe-fx+ i 1)))
                        (loop.swap t.next key))))))))
          (when (and (unsafe-fx< 0 shift) (unsafe-fx< i.part.start i.end))
            (if (unsafe-fx<= (unsafe-fx- i.end i.part.start) threshold)
              (counting-radix-sort! v t->key (/ shift 8) i.part.start i.end)
              (loop.shift (unsafe-fx- shift size.shift) i.part.start i.end)))
          (loop.move.k (unsafe-fx+ k 1) i.end))))))

;; TODO: factor out enumerator-radix-partition ?
;; simplify t->key interface to return exactly a byte, discard key-byte-count parameter

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Benchmark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;#|
(define key-byte-count 4)
;(define count.node (arithmetic-shift 1 28))
;(define count.node (arithmetic-shift 1 27))
(define count.node (arithmetic-shift 1 25))
;|#

#|
(define key-byte-count 3)
;(define count.node (arithmetic-shift 1 24))
(define count.node (arithmetic-shift 1 22))
;|#

#|
(define key-byte-count 2)
(define count.node (arithmetic-shift 1 16))
;|#

(define count.edge (* count.node 16))

(displayln "allocating nodes:")
(displayln count.node)
(define nodes (time (make-vector count.node)))
(displayln "allocating edges:")
(displayln count.edge)

;#|
(define edges (time (make-vector count.edge)))
(define en.edges (unsafe-vector->enumerator edges 0 count.edge))

(displayln "building edges")
(time (let loop ((i 0))
        (when (< i count.edge)
          (vector-set! edges i (random count.node))
          ;(vector-set! edges i (cons (random count.node) (random count.node)))
          (loop (+ i 1)))))
;|#

#|
(define (en.edges k!)
  (let loop ((i 0))
    (when (< i count.edge)
      (k! (random count.node))
      ;(k! (cons (random count.node) (random count.node)))
      (loop (+ i 1)))))
;|#

;(define edge->key car)
(define (edge->key e) e)

#|
(displayln "sorting edges")
(time (vector-sort!
        edges
        (lambda (e.0 e.1) (< (edge->key e.0) (edge->key e.1)))))
;|#

#|
(displayln "computing node degrees inlined")
(time (let loop ((i 0))
        (when (unsafe-fx< i count.edge)
          (let ((key (unsafe-vector*-ref edges i)))
            (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1)))
          (loop (unsafe-fx+ i 1)))))
;|#

#|
(displayln "computing node degrees")
(time (en.edges
        (lambda (edge)
         (define key (edge->key edge))
         ;(vector-set! nodes key (+ (vector-ref nodes key) 1))
         (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1))
         )))
;|#

#|
(displayln "computing node degrees inlined after lsd-radix sorting edges in-place")
;(time (unsafe-vector-radix-sort! edges edge->key key-byte-count))
;(time (counting-radix-sort! edges edge->key key-byte-count))
(let ((v.workspace (time (make-vector count.edge))))
  (time (collect-garbage))
  (time (counting-radix-sort-helper edges 0 count.edge v.workspace 0 edges 0 edge->key key-byte-count))
  (when (odd? key-byte-count)
    (time (vector-copy! edges 0 v.workspace 0 count.edge))))

(time (let loop ((i 0))
        (when (unsafe-fx< i count.edge)
          (let ((key (unsafe-vector*-ref edges i)))
            (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1)))
          (loop (unsafe-fx+ i 1)))))
;|#

#|
(displayln "computing node degrees inlined after in-place-msd-radix sorting edges")
(time (in-place-radix-sort! edges edge->key key-byte-count))
(time (let loop ((i 0))
        (when (unsafe-fx< i count.edge)
          (let ((key (unsafe-vector*-ref edges i)))
            (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1)))
          (loop (unsafe-fx+ i 1)))))
;|#

#|
(displayln "computing node degrees while lsd-radix sorting edges")
(time ((enumerator-lsd-radix-sort en.edges edge->key key-byte-count)
       (lambda (edge)
         (define key (edge->key edge))
         ;(vector-set! nodes key (+ (vector-ref nodes key) 1))
         (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1))
         )))
;|#

#|
(displayln "computing node degrees while msd-radix sorting edges")
(time ((enumerator-msd-radix-sort en.edges edge->key key-byte-count)
       (lambda (edge)
         (define key (edge->key edge))
         ;(vector-set! nodes key (+ (vector-ref nodes key) 1))
         (unsafe-vector*-set! nodes key (unsafe-fx+ (unsafe-vector*-ref nodes key) 1))
         )))
;|#

;(displayln 'in-place-radix-sort!) (time (in-place-radix-sort! edges edge->key key-byte-count))
(displayln 'counting-radix-sort!) (time (counting-radix-sort! edges edge->key key-byte-count))
;(displayln 'unsafe-vector-radix-sort!) (time (unsafe-vector-radix-sort! edges edge->key key-byte-count))
(time (let ((previous #f))
        (en.edges
          (lambda (edge)
            (when (and previous (< (edge->key edge) previous))
              (error "not sorted!" previous (edge->key edge)))
            (set! previous (edge->key edge))))))

#;(let ((previous #f))
  ((enumerator-lsd-radix-sort en.edges edge->key key-byte-count)
   (lambda (edge)
     (when (and previous (< (edge->key edge) previous))
       (error "not sorted!" previous (edge->key edge)))
     (set! previous (edge->key edge)))))

#;(let ((previous #f))
  ((enumerator-msd-radix-sort en.edges edge->key key-byte-count)
   (lambda (edge)
     (when (and previous (< (edge->key edge) previous))
       (error "not sorted!" previous (edge->key edge)))
     (set! previous (edge->key edge)))))
