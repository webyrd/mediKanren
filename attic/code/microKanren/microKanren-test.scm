(load "microKanren-test-programs.scm")

(test-check "second-set t1"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (car $))
  '(((#(0) . 5)) . 1))

(test-check "second-set t2"
  (let (($ ((call/fresh (lambda (q) (== q 5))) empty-state)))
    (cdr $))
  '())

(test-check "second-set t3"
  (let (($ (a-and-b empty-state)))
    (car $))
  '(((#(1) . 5) (#(0) . 7)) . 2))

(test-check "second-set t3, take"
  (let (($ (a-and-b empty-state)))
    (take 1 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)))

(test-check "second-set t4"
  (let (($ (a-and-b empty-state)))
    (car (cdr $)))
  '(((#(1) . 6) (#(0) . 7)) . 2))

(test-check "second-set t5"
  (let (($ (a-and-b empty-state)))
    (cdr (cdr $)))
  '())

(test-check "who cares"
  (let (($ ((call/fresh (lambda (q) (fives q))) empty-state)))
    (take 1 $))
  '((((#(0) . 5)) . 1)))

(test-check "take 2 a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take 2 $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "take-all a-and-b stream"
  (let (($ (a-and-b empty-state)))
    (take-all $))
  '((((#(1) . 5) (#(0) . 7)) . 2)
    (((#(1) . 6) (#(0) . 7)) . 2)))

(test-check "ground appendo"
  (car ((ground-appendo empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "ground appendo2"
  (car ((ground-appendo2 empty-state)))
  '(((#(2) b) (#(1)) (#(0) . a)) . 3))

(test-check "appendo"
  (take 2 (call-appendo empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4)
    (((#(0) #(1) #(2) #(3)) (#(2) . #(6)) (#(5)) (#(3) #(4) . #(6)) (#(1) #(4) . #(5))) . 7)))

(test-check "appendo2"
  (take 2 (call-appendo2 empty-state))
  '((((#(0) #(1) #(2) #(3)) (#(2) . #(3)) (#(1))) . 4) (((#(0) #(1) #(2) #(3)) (#(3) #(4) . #(6)) (#(2) . #(6)) (#(5)) (#(1) #(4) . #(5))) . 7)))

(test-check "reify-1st across appendo"
  (map reify-1st (take 2 (call-appendo empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "reify-1st across appendo2"
  (map reify-1st (take 2 (call-appendo2 empty-state)))
  '((() _.0 _.0) ((_.0) _.1 (_.0 . _.1))))

(test-check "many non-ans"
  (take 1 (many-non-ans empty-state))
  '((((#(0) . 3)) . 1)))