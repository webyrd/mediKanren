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
  racket/stream
  )

(define (db:~string-valueo db:~string->id&value* db ~string id value)
  (project (db:~string->id&value* ~string)
    (if (or (var? db:~string->id&value*) (var? ~string))
      (error "fuzzy search string must be ground:" ~string)
      (let loop ((id&value* (db:~string->id&value* db ~string)))
        (if (stream-empty? id&value*) fail
          (let* ((i&v (stream-first id&value*))
                 (i (car i&v))
                 (v (cdr i&v)))
            (conde
              ((== i id) (== v value))
              ((loop (stream-rest id&value*))))))))))

(define ((i&c->i&c-list db) i&c)
  (define c (cdr i&c))
  (cons (car i&c) (list (vector->list c)
                        (db:catid->category db (concept-category c)))))

(define (db:~name->cid&concept-list* db ~name)
  (stream-map (i&c->i&c-list db) (db:~name->cid&concept* db ~name)))

(define (db:~cui->cid&concept-list* db ~cui)
  (stream-map (i&c->i&c-list db) (db:~cui->cid&concept* db ~cui)))

(define (db:~cui-concepto db ~cui cid concept)
  (db:~string-valueo db:~cui->cid&concept-list* db ~cui cid concept))
(define (db:~name-concepto db ~name cid concept)
  (db:~string-valueo db:~name->cid&concept-list* db ~name cid concept))
(define (db:~predicateo db ~predicate pid predicate)
  (db:~string-valueo
    db:~predicate->pid&predicate* db ~predicate pid predicate))
(define (db:~categoryo db ~category catid category)
  (db:~string-valueo
    db:~category->catid&category* db ~category catid category))

(define (vector-refo v* iv)
  (when (not (vector? v*)) (error "vector-refo vector must be ground:" v* iv))
  (project (iv)
    (cond ((and (pair? iv) (integer? (car iv)))
           (== (cons (car iv) (vector-ref v* (car iv))) iv))
          (else
            (define len (vector-length v*))
            (let loop ((i 0))
              (if (<= len i) fail
                (conde
                  ((== (cons i (vector-ref v* i)) iv))
                  ((loop (+ i 1))))))))))

(define (db:predicateo db predicate)
  (vector-refo (db:predicate* db) predicate))

(define (db:categoryo db category)
  (vector-refo (db:category* db) category))

(define (db:concepto db cid concept)
  (fresh (cat concept-details cui catid rest)
    (== `(,concept-details ,cat) concept)
    (== `(,cui ,catid . ,rest) concept-details)
    (project (cid catid)
      (let ((ground
              (lambda (cid)
                (let ((c (db:cid->concept db cid)))
                  (fresh ()
                    (== (vector->list c) concept-details)
                    (== (db:catid->category db (concept-category c)) cat))))))
        (cond ((not (var? cid)) (ground cid))
              ((not (var? catid))
               (let loop ((cid* (vector->list (db:catid->cid* db catid))))
                 (if (null? cid*) fail
                   (conde
                     ((== (car cid*) cid) (ground (car cid*)))
                     ((loop (cdr cid*)))))))
              (else
                (let loop ((c&c* (db:cid&concept-stream db)))
                  (if (stream-empty? c&c*) fail
                    (let* ((c&c (stream-first c&c*))
                           (c (cdr c&c)))
                      (conde
                        ((== (car c&c) cid)
                         (== (vector->list c) concept-details)
                         (== (db:catid->category db (concept-category c)) cat))
                        ((loop (stream-rest c&c*)))))))))))))

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
