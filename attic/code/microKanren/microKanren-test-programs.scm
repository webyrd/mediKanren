
(define-syntax test-check
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (errorf 'test-check
               "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
               'tested-expression expected produced)))))))

(define a-and-b
  (conj 
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh 
    (lambda (b) 
      (disj
       (== b 5)
       (== b 6))))))

(define fives
  (lambda (x)
    (disj
     (== x 5)      
     (lambda (a/c)
       (lambda ()
         ((fives x) a/c))))))

(define appendo
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (== `(,a . ,res) out)
                (lambda (s/c)
                  (lambda ()
                    ((appendo d s res) s/c))))))))))))))

(define appendo2
  (lambda (l s out)
    (disj
     (conj (== '() l) (== s out))
     (call/fresh
      (lambda (a)
        (call/fresh
         (lambda (d)
           (conj
            (== `(,a . ,d) l)
            (call/fresh
             (lambda (res)
               (conj
                (lambda (s/c)
                  (lambda ()
                    ((appendo2 d s res) s/c)))
                (== `(,a . ,res) out))))))))))))

(define call-appendo
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo l s out)
               (== `(,l ,s ,out) q)))))))))))

(define call-appendo2
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (appendo2 l s out)
               (== `(,l ,s ,out) q)))))))))))

(define call-appendo3
  (call/fresh
   (lambda (q)
     (call/fresh
      (lambda (l)
        (call/fresh
         (lambda (s)
           (call/fresh
            (lambda (out)
              (conj
               (== `(,l ,s ,out) q)
               (appendo l s out)))))))))))

(define ground-appendo (appendo '(a) '(b) '(a b)))

(define ground-appendo2  (appendo2 '(a) '(b) '(a b)))

(define relo
  (lambda (x)
    (call/fresh
     (lambda (x1)
       (call/fresh
        (lambda (x2)
          (conj
           (== x `(,x1 . ,x2))
           (disj
            (== x1 x2)
            (lambda (s/c)
              (lambda () ((relo x) s/c)))))))))))

(define many-non-ans
  (call/fresh
   (lambda (x)
     (disj
      (relo `(5 . 6))
      (== x 3)))))

