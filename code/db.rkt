#lang racket/base
(provide
  id=>predicate
  predicate=>id
  id=>semtype
  semtype=>id
  semtype-id=>cui*
  concept*
  cui=>concept
  fuzzy-name->concept*
  fuzzy-name*->concept*
  subject->edge*
  object->edge*
  subject&object->edge*
  subject*->edge*
  object*->edge*
  subject*&object*->edge*
  subject->edge*/stream
  object->edge*/stream
  predicate->edge*/stream
  edge*/stream
  concept-pretty
  edge-pretty
  direct-edge*
  )

(require
  "concept.rkt"
  "edge.rkt"
  "read.rkt"
  racket/list
  racket/stream
  racket/string
  )

(define (file->hash path) (make-immutable-hash (read-all-from-file path)))
(define (file->id=>name path)
  (define x* (read-all-from-file path))
  (make-immutable-hash (map cons (range (length x*)) x*)))
(define (file->name=>id path)
  (define x* (read-all-from-file path))
  (make-immutable-hash (map cons x* (range (length x*)))))

(define id=>predicate (file->id=>name "semmed/PREDICATE.scm"))
(define predicate=>id (file->name=>id "semmed/PREDICATE.scm"))

(define id=>semtype (file->id=>name "semmed/SEMTYPE.scm"))
(define semtype=>id (file->name=>id "semmed/SEMTYPE.scm"))

(define semtype-id=>cui* (file->hash "semmed/cui-by-semtype.scm"))

(define concept* (read-all-from-file "semmed/concept.scm"))
(define cui=>concept
  (make-immutable-hash (map (lambda (c) (cons (concept-cui c) c)) concept*)))

(define (fuzzy-name->concept* concept* name case-insensitive?)
  (define needle (if case-insensitive? (string-downcase name) name))
  (define c->name (if case-insensitive?
                    (lambda (c) (string-downcase (concept-name c)))
                    concept-name))
  (filter (lambda (c) (string-contains? (c->name c) needle)) concept*))
(define (fuzzy-name*->concept* concept* names case-insensitive?)
  (foldl (lambda (n c*) (fuzzy-name->concept* c* n case-insensitive?))
         concept* names))

(define subject=>pos (file->hash "semmed/edge-by-subject/index.scm"))
(define in-detail-ebs (open-input-file "semmed/edge-by-subject/detail.bin"))
(define object=>pos (file->hash "semmed/edge-by-object/index.scm"))
(define in-detail-ebo (open-input-file "semmed/edge-by-object/detail.bin"))
(define predicate=>pos (file->hash "semmed/edge-by-predicate/index.scm"))
(define in-detail-ebp (open-input-file "semmed/edge-by-predicate/detail.bin"))

(define (maybe-bytes->edge bs) (if (eof-object? bs) bs (bytes->edge bs)))

(define (edge*-contiguous pos in-detail continue? p?)
  (file-position in-detail pos)
  (let loop ((edge (maybe-bytes->edge (read-edge-bytes in-detail))))
    (if (and (not (eof-object? edge)) (continue? edge))
      (if (p? edge)
        (let build-edge ((pubs (list (edge-pub-info edge))))
          (define e-next (maybe-bytes->edge (read-edge-bytes in-detail)))
          (if (edge-meaning=? edge e-next)
            (build-edge (cons (edge-pub-info e-next) pubs))
            (cons (edge-pubrefs-set edge pubs) (loop e-next))))
        (loop (maybe-bytes->edge (read-edge-bytes in-detail))))
      '())))

(define (cui->edge* cui=>pos in-detail cui p?)
  (define (cui-matches? e) (= cui (edge-src e)))
  (define pos (hash-ref cui=>pos cui #f))
  (if pos (edge*-contiguous pos in-detail cui-matches? p?) '()))

(define (subject->edge* cui p?)
  (cui->edge* subject=>pos in-detail-ebs cui p?))
(define (object->edge* cui p?)
  (cui->edge* object=>pos in-detail-ebo cui p?))
(define (subject&object->edge* cui-s cui-o p?)
  (subject->edge* cui-s (lambda (e) (and (= cui-o (edge-dst e)) (p? e)))))
(define (subject*->edge* cui* p?)
  (for/fold ((edge* '())) ((cui cui*)) (append (subject->edge* cui p?) edge*)))
(define (object*->edge* cui* p?)
  (for/fold ((edge* '())) ((cui cui*)) (append (object->edge* cui p?) edge*)))
(define (subject*&object*->edge* cui-s* cui-o* p?)
  (for*/fold ((edge* '()))
             ((cui-s cui-s*) (cui-o cui-o*))
             (append (subject&object->edge* cui-s cui-o p?) edge*)))

(define (edge*-contiguous/stream pos in-detail continue? p?)
  (let loop ((pos pos) (set-pos? #t))
    (define edge (begin (when set-pos? (file-position in-detail pos))
                        (maybe-bytes->edge (read-edge-bytes in-detail))))
    (define pos-next (+ pos edge-byte-size))
    (if (and (not (eof-object? edge)) (continue? edge))
      (if (p? edge)
        (let build-edge ((pos pos-next) (pubs (list (edge-pub-info edge))))
          (define e-next (maybe-bytes->edge (read-edge-bytes in-detail)))
          (if (edge-meaning=? edge e-next)
            (build-edge (+ pos edge-byte-size)
                        (cons (edge-pub-info e-next) pubs))
            (stream-cons (edge-pubrefs-set edge pubs) (loop pos #t))))
        (loop pos-next #f))
      '())))

(define (cui->edge*/stream cui=>pos in-detail cui p?)
  (define (cui-matches? e) (= cui (edge-src e)))
  (define pos (hash-ref cui=>pos cui #f))
  (if pos (edge*-contiguous/stream pos in-detail cui-matches? p?) '()))

(define (subject->edge*/stream cui p?)
  (cui->edge*/stream subject=>pos in-detail-ebs cui p?))
(define (object->edge*/stream cui p?)
  (cui->edge*/stream object=>pos in-detail-ebo cui p?))
(define (predicate->edge*/stream predicate subject-type object-type p?)
  (define (edge*/stream pos set-pos?)
    (define edge (begin (when set-pos? (file-position in-detail-ebp pos))
                        (maybe-bytes->edge (read-edge-bytes in-detail-ebp))))
    (define pos-next (+ pos edge-byte-size))
    (if (and (not (eof-object? edge))
             (= predicate (edge-predicate edge))
             (= subject-type (edge-src-type edge))
             (= object-type (edge-dst-type edge)))
      (if (p? edge)
        (stream-cons edge (edge*/stream pos-next #t))
        (edge*/stream pos-next #f))
      '()))
  (define pos (hash-ref predicate=>pos
                        (vector predicate subject-type object-type) #f))
  (if pos (edge*/stream pos #t) '()))

(define (edge*/stream p?)
  (edge*-contiguous/stream 0 in-detail-ebs (lambda (_) #t) p?))

(define (concept-pretty c)
  (vector (concept-cui c) (concept-name c)
          (map (lambda (tn) (vector (hash-ref id=>semtype (vector-ref tn 0))
                                    (vector-ref tn 1)))
               (concept-type c))))

(define (edge-pretty e)
  (vector (concept-pretty (hash-ref cui=>concept (edge-src e)))
          (concept-pretty (hash-ref cui=>concept (edge-dst e)))
          (hash-ref id=>predicate (edge-predicate e))
          (hash-ref id=>semtype (edge-src-type e))
          (hash-ref id=>semtype (edge-dst-type e))
          (edge-pub-info e)))

(define (direct-edge* c*1 c*2)
  (map edge-pretty (subject*&object*->edge* (map concept-cui c*1)
                                            (map concept-cui c*2)
                                            (lambda (e) #t))))
