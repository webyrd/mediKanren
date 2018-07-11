#lang racket/base
(provide
  db:edgeo
  db:concepto
  db:categoryo
  db:predicateo
  )

(require
  "db.rkt"
  "mk.rkt"
  "repr.rkt"
  racket/stream
  )

;; TODO: fuzzy name search.

(define (vector-refo v* ix value)
  (define len (vector-length v*))
  (let loop ((i 0))
    (if (<= len i) fail
      (conde
        ((== i ix) (== (vector-ref v* i) value))
        ((loop (+ i 1)))))))

(define (db:predicateo db pid predicate)
  (vector-refo (db:predicate* db) pid predicate))

(define (db:categoryo db catid category)
  (vector-refo (db:category* db) catid category))

(define (db:concepto db cid concept)
  (fresh (cat concept-details)
    (== `(,concept-details ,cat) concept)
    (project (cid)
      (if (not (var? cid))
        (let ((c (db:cid->concept db cid)))
          (== (vector->list c) concept-details)
          (== (db:catid->category db (concept-category c)) cat))
        (let loop ((c&c* (db:cid&concept-stream db)))
          (if (stream-empty? c&c*) fail
            (let* ((c&c (stream-first c&c*))
                   (c (cdr c&c)))
              (conde
                ((== (car c&c) cid)
                 (== (vector->list c) concept-details)
                 (== (db:catid->category db (concept-category c)) cat))
                ((loop (stream-rest c&c*)))))))))))

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
