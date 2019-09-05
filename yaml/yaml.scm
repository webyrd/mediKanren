(load "biolink-model.scm")
(define model biolink-model)

(define (rel l1 l2)
  (map (lambda (c) (cons (car c)
                    (let ((p (assoc l2 (cdr c))))
                      (if p (cdr p) #f))))
       (cdr (assoc l1 model))))

(define categories-is-a (rel "classes" "is-a"))

(define predicates-is-a (rel "slots" "is-a"))

