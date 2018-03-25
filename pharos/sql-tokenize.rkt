#lang racket/base
(require
  "mk.rkt"
  "mk-parse.rkt"
  racket/port
  )

;; Tokenization
(define chars-ws '(#\space #\tab #\newline #\return #\vtab #\page))
(define chars-hws '(#\space #\tab))

(define chars-quote '(#\' #\" #\`))
(define chars-bracket '(#\( #\) #\[ #\] #\{ #\}))
(define chars-separator '(#\, #\; #\: #\.))
(define chars-symbolic (append chars-quote chars-bracket chars-separator))

(define skip-ws (skip-while chars-ws))
(define skip-hws (skip-while chars-hws))

(define (token* ts)
  (define chars-ws&symbolic (append chars-symbolic chars-ws))
  (define (token t)
    (seq skip-ws
         (or/p (fresh/p (c)
                 (== (list c) t)
                 ((remember (one-of chars-symbolic)) c))
               ((many+-until chars-ws&symbolic) t))))
  (seq ((many* token) ts) skip-ws end))

(define (tokenize str)
  (define results (run* (ts) ((token* ts) (string->list str) '())))
  (when (= 0 (length results)) (error "failed to tokenize"))
  (when (< 1 (length results))
    (error "ambiguous tokenization: count=" (length results)))
  (map list->string (car results)))

(write (tokenize (port->string)))

;; First attempt at token*, factoring out split-ws*.  It's a lot more involved.
;(define (split-ws* blocks)
  ;(define (non-ws* block)
    ;(seq skip-ws
         ;((many+ (remember (none-of chars-ws))) block)
         ;(or/p end (forget (one-of chars-ws)))))
  ;(seq ((many* non-ws*) blocks) skip-ws end))

;(define (split-symbolic* bs)
  ;(define (block s)
    ;(or/p (lambda (in out)
            ;(fresh (c)
              ;(== (list c) s)
              ;(((remember (one-of chars-symbolic)) c) in out)))
          ;((many+-until chars-symbolic) s)))
  ;(seq ((many* block) bs) end))

;(define (appendo l s ls)
  ;(conde
    ;((== '() l) (== s ls))
    ;((fresh (a d ms)
       ;(== `(,a . ,d) l)
       ;(== `(,a . ,ms) ls)
       ;(appendo d s ms)))))

;(define (flatteno xss flattened)
  ;(conde
    ;((== '() xss) (== '() flattened))
    ;((fresh (xs yss yflattened)
       ;(== `(,xs . ,yss) xss)
       ;(flatteno yss yflattened)
       ;(appendo xs yflattened flattened)))))

;(define (mapo R xs zs)
  ;(conde
    ;((== '() xs) (== '() zs))
    ;((fresh (a d y ys)
       ;(== `(,a . ,d) xs)
       ;(== `(,y . ,ys) zs)
       ;(R a y)
       ;(mapo R d ys)))))

;(define (token* ts)
  ;(define (parse in bs) ((split-symbolic* bs) in '()))
  ;(lambda (in out)
    ;(fresh (bs tss)
      ;((split-ws* bs) in out)
      ;(mapo parse bs tss)
      ;(flatteno tss ts))))
