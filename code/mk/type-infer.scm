(load "mk.scm")

(define !-
  (lambda (exp env t)
    (conde
      [(symbolo exp) (lookupo exp env t)]
      [(fresh (x e t-x t-e)
         (== `(lambda (,x) ,e) exp)
         (symbolo x)
         (not-in-envo 'lambda env)
         (== `(-> ,t-x ,t-e) t)
         (!- e `((,x . ,t-x) . ,env) t-e))]
      [(fresh (rator rand t-x)
         (== `(,rator ,rand) exp)
         (!- rator env `(-> ,t-x ,t))
         (!- rand env t-x))])))

(define lookupo
  (lambda (x env t)
    (fresh (rest y v)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== y x) (== v t))
        ((=/= y x) (lookupo x rest t))))))

(define not-in-envo
  (lambda (x env)
    (conde
      ((== '() env))
      ((fresh (y v rest)
         (== `((,y . ,v) . ,rest) env)
         (=/= y x)
         (not-in-envo x rest))))))
