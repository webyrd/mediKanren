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
  (define (edge-propso eid scid ocid pid eprops)
    (define e/p (db:eid->edge db eid))
    (fresh ()
      (== (edge/props-subject e/p) scid)
      (== (edge/props-object e/p) ocid)
      (== (edge/props-pid e/p) pid)
      (== (edge/props-props e/p) eprops)))
  (define (edges-by-Xo cid->stream cid pid dcatid dcid eid)
    (project (cid pid dcatid dcid eid)
      (let loop ((e* (cid->stream db cid
                                  (and (not (var? pid)) pid)
                                  (and (not (var? dcatid)) dcatid)
                                  (and (not (var? dcid)) dcid))))
        (if (stream-empty? e*) fail
          (conde
            ((== (edge-eid (stream-first e*)) eid))
            ((loop (stream-rest e*))))))))
  (fresh (scid scui sname scatid scat sprops
               ocid ocui oname ocatid ocat oprops eid pid pred eprops)
    (== `(,eid (,scid . (,scui ,sname (,scatid . ,scat) . ,sprops))
               (,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops))
               (,pid . ,pred) ,eprops) edge)
    (project (scid scui sname scatid scat sprops
                   ocid ocui oname ocatid ocat oprops eid pid pred eprops)
      (let* ((edges-by-subject
               (edges-by-Xo db:subject->edge-stream scid pid ocatid ocid eid))
             (edges-by-object
               (edges-by-Xo db:object->edge-stream ocid pid scatid scid eid))
             (subject (db:concepto db `(,scid . (,scui ,sname (,scatid . ,scat)
                                                       . ,sprops))))
             (object (db:concepto db `(,ocid . (,ocui ,oname (,ocatid . ,ocat)
                                                      . ,oprops))))
             (subject-edges (fresh () subject edges-by-subject))
             (object-edges (fresh () object edges-by-object)))
        (cond ((not (var? eid)) succeed)
              ((not (var? scid)) edges-by-subject)
              ((not (var? ocid)) edges-by-object)
              ((not (var? scatid)) subject-edges)
              ((not (var? ocatid)) object-edges)
              ((not (var? scat))
               (fresh () (db:categoryo db `(,scatid . ,scat)) subject-edges))
              ((not (var? ocat))
               (fresh () (db:categoryo db `(,ocatid . ,ocat)) object-edges))
              ((not (andmap var? (list scui sname scat sprops))) subject-edges)
              ((not (andmap var? (list ocui oname ocat oprops))) object-edges)
              (else subject-edges))))
    (project (eid) (edge-propso eid scid ocid pid eprops))
    (db:predicateo db `(,pid . ,pred))
    (db:concepto db `(,scid . (,scui ,sname (,scatid . ,scat) . ,sprops)))
    (db:concepto db `(,ocid . (,ocui ,oname (,ocatid . ,ocat) . ,oprops)))))
