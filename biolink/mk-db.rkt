#lang racket/base
(provide
  (all-from-out "mk.rkt")
  make-db

  db:edgeo
  db:concepto
  db:categoryo
  db:predicateo

  db:~cui-concepto
  db:~name-concepto
  db:~predicateo
  db:~categoryo
  )

(require
  "db.rkt"
  "mk.rkt"
  "repr.rkt"
  (except-in racket/match ==)
  racket/stream
  )

(define (vconcept->details db v)
  (define catid (concept-category v))
  (list (concept-cui v)
        (concept-name v)
        (cons catid (vector-ref (db:category* db) catid))
        (concept-props v)))
(define ((i&v->i&d db) i&v) (cons (car i&v) (vconcept->details db (cdr i&v))))

(define (stream-refo i&v* iv)
  (let loop ((i&v* i&v*))
    (if (stream-empty? i&v*) fail
      (let* ((i&v (stream-first i&v*)))
        (conde
          ((== `(,(car i&v) . ,(cdr i&v)) iv))
          ((loop (stream-rest i&v*))))))))

(define (db:~cui-concepto db ~cui concept)
  (stream-refo
    (stream-map (i&v->i&d db) (db:~cui->cid&concept* db ~cui)) concept))
(define (db:~name-concepto db ~name concept)
  (stream-refo
    (stream-map (i&v->i&d db) (db:~name->cid&concept* db ~name)) concept))
(define (db:~predicateo db ~predicate predicate)
  (stream-refo (db:~predicate->pid&predicate* db ~predicate) predicate))
(define (db:~categoryo db ~category category)
  (stream-refo (db:~category->catid&category* db ~category) category))

(define (vector-refo e->v v* iv)
  (when (not (vector? v*)) (error "vector-refo vector must be ground:" v* iv))
  (project (iv)
    (cond ((and (pair? iv) (integer? (car iv)))
           (== (cons (car iv) (e->v (vector-ref v* (car iv)))) iv))
          (else
            (define len (vector-length v*))
            (let loop ((i 0))
              (if (<= len i) fail
                (conde
                  ((== (cons i (e->v (vector-ref v* i))) iv))
                  ((loop (+ i 1))))))))))

(define (db:predicateo db predicate)
  (vector-refo (lambda (x) x) (db:predicate* db) predicate))

(define (db:categoryo db category)
  (vector-refo (lambda (x) x) (db:category* db) category))

(define (db:concepto db concept)
  (define (v->concept v) (vconcept->details db v))
  (project (concept)
    (match concept
      (`(,(? var?) ,_ ,_ (,(and catid (? integer?)) . ,_) . ,_)
        (displayln `(concept by category: ,catid))
        (define cid* (db:catid->cid* db catid))
        (define len (vector-length cid*))
        (let loop ((i 0))
          (if (<= len i) fail
            (let ((cid (vector-ref cid* i)))
              (conde
                ((== (cons cid (v->concept (vector-ref (db:concept* db) cid)))
                     concept))
                ((loop (+ i 1))))))))
      (_ (vector-refo v->concept (db:concept* db) concept)))))

(define (db:edgeo db edge)
  (fresh (predicate subject-cid object-cid subject-cat object-cat details)
    (== `((,subject-cid ,subject-cat)
          ,predicate
          (,object-cid ,object-cat)
          ,details) edge)
    (project (predicate subject-cid object-cid subject-cat object-cat details)
      (cond ((not (var? subject-cid))
             (let loop ((e* (db:subject->edge-stream
                              db subject-cid
                              (and (not (var? predicate)) predicate)
                              (and (not (var? object-cat)) object-cat)
                              (and (not (var? object-cid)) object-cid))))
               (if (stream-empty? e*) fail
                 (let* ((e (stream-first e*))
                        (pid (edge-predicate e))
                        (catid (edge-dst-category e))
                        (cid (edge-dst e))
                        (eid (edge-eid e))
                        (concept (db:cid->concept db subject-cid))
                        (src-catid (concept-category concept))
                        (cat (db:catid->category db src-catid)))
                   (conde
                     ((== pid predicate)
                      (== src-catid subject-cat)
                      (== cid object-cid)
                      (== catid object-cat)
                      (== `((,(vector->list concept) ,cat)
                            (,(db:pid->predicate db pid)
                              ,(vector->list (db:eid->edge db eid))
                              ,eid)
                            (,(vector->list (db:cid->concept db cid))
                              ,(db:catid->category db catid))) details))
                     ((loop (stream-rest e*))))))))

            ((not (var? object-cid))
             (let loop ((e* (db:object->edge-stream
                              db object-cid
                              (and (not (var? predicate)) predicate)
                              (and (not (var? subject-cat)) subject-cat)
                              (and (not (var? subject-cid)) subject-cid))))
               (if (stream-empty? e*) fail
                 (let* ((e (stream-first e*))
                        (pid (edge-predicate e))
                        (catid (edge-dst-category e))
                        (cid (edge-dst e))
                        (eid (edge-eid e))
                        (concept (db:cid->concept db object-cid))
                        (src-catid (concept-category concept))
                        (cat (db:catid->category db src-catid)))
                   (conde
                     ((== pid predicate)
                      (== src-catid object-cat)
                      (== cid subject-cid)
                      (== catid subject-cat)
                      (== `((,(vector->list (db:cid->concept db cid))
                              ,(db:catid->category db catid))
                            (,(db:pid->predicate db pid)
                              ,(vector->list (db:eid->edge db eid))
                              ,eid)
                            (,(vector->list concept) ,cat)) details))
                     ((loop (stream-rest e*))))))))

            (else (let loop ((eid&e/props* (db:eid&edge/props-stream db)))
                    (if (stream-empty? eid&e/props*) fail
                      (let* ((eid&e (stream-first eid&e/props*))
                             (eid (car eid&e))
                             (e (cdr eid&e))
                             (scid (edge/props-subject e))
                             (pid (edge/props-pid e))
                             (ocid (edge/props-object e))
                             (subject (db:cid->concept db scid))
                             (object  (db:cid->concept db ocid))
                             (scatid (concept-category subject))
                             (ocatid (concept-category object))
                             (scat (db:catid->category db scatid))
                             (ocat (db:catid->category db ocatid)))
                        (conde
                          ((== pid predicate)
                           (== scid subject-cid)
                           (== scatid subject-cat)
                           (== ocid object-cid)
                           (== ocatid object-cat)
                           (== `((,(vector->list subject) ,scat)
                                 (,(db:pid->predicate db pid)
                                   ,(vector->list e)
                                   ,eid)
                                 (,(vector->list object) ,ocat)) details))
                          ((loop (stream-rest eid&e/props*))))))))))))
