#lang racket/base
(provide
 get-biolink-version
 predicate-deprecated?
 class-deprecated?
 predicate-mixin?
 class-mixin?
 predicate-symmetric?
 ;;
 all-predicates
 all-classes
 ;;
 get-inverse-predicate
 ;;
 get-predicate-children
 get-class-children
 ;;
 get-predicate-descendents 
 get-class-descendents
 ;;
 get-predicate-descendents*
 get-class-descendents*
 ;;
 get-predicate-proper-descendents 
 get-class-proper-descendents
 ;;
 get-predicates-from-mixin
 get-classes-from-mixin)
(require
 racket/pretty
 racket/runtime-path
 racket/set
 racket/string
 yaml)

;; (define BIOLINK_YAML_FILE "../neo-biolink/biolink_2_4_8/biolink-model.yaml")
(define BIOLINK_YAML_FILE "../neo-biolink/biolink_3_0_3/biolink-model.yaml")
;; (define BIOLINK_YAML_FILE "../neo-biolink/biolink_3_1_1/biolink-model.yaml")

(define-runtime-path path.here ".")
(define bl-path (build-path path.here BIOLINK_YAML_FILE))

(printf "loading biolink YAML file from '~s'...\n" bl-path)

(define ip (open-input-file bl-path))
(define biolink (time (read-yaml ip)))

(define (get-biolink-version)
  (hash-ref biolink "version"))

(printf "finished loading biolink YAML file from '~s'\n" BIOLINK_YAML_FILE)
(printf "loaded biolink version '~s'\n" (get-biolink-version))

(define yaml-predicate-name-to-biolink-name
  (lambda (str)
    (string-append "biolink:" (string-replace str " " "_"))))


(define yaml-class-name-to-biolink-name
  (lambda (str)
    (string-append "biolink:" (string-replace (string-titlecase str) " " ""))))


;; list of predicates
(define slots (hash-keys (hash-ref biolink "slots")))

;; list of classes
(define classes (hash-keys (hash-ref biolink "classes")))

(define all-predicates (list->set (map yaml-predicate-name-to-biolink-name slots)))
(define all-classes (list->set (map yaml-class-name-to-biolink-name classes)))

(define predicate-deprecated?
  (let ((predicate-deprecated-hash (make-hash)))
    (for-each
      (lambda (pred-yaml-str)
        (let ((deprecated-flag (hash-ref (hash-ref (hash-ref biolink "slots") pred-yaml-str) "deprecated" #f)))
          (when deprecated-flag
            (let ((pred-biolink-name (yaml-predicate-name-to-biolink-name pred-yaml-str)))
              (hash-set! predicate-deprecated-hash pred-biolink-name #t)))))
      slots)
    (lambda (pred)
      (hash-ref predicate-deprecated-hash pred #f))))


(define class-deprecated?
  (let ((class-deprecated-hash (make-hash)))
    (for-each
      (lambda (class-yaml-str)
        (let ((deprecated-flag (hash-ref (hash-ref (hash-ref biolink "classes") class-yaml-str) "deprecated" #f)))
          (when deprecated-flag
            (let ((class-biolink-name (yaml-class-name-to-biolink-name class-yaml-str)))
              (hash-set! class-deprecated-hash class-biolink-name #t)))))
      classes)
    (lambda (class)
      (hash-ref class-deprecated-hash class #f))))


(define predicate-mixin?
  (let ((predicate-mixin-hash (make-hash)))
    (for-each
      (lambda (pred-yaml-str)
        (let ((mixin-flag (hash-ref (hash-ref (hash-ref biolink "slots") pred-yaml-str) "mixin" #f)))
          (when mixin-flag
            (let ((pred-biolink-name (yaml-predicate-name-to-biolink-name pred-yaml-str)))
              (hash-set! predicate-mixin-hash pred-biolink-name #t)))))
      slots)
    (lambda (pred)
      (hash-ref predicate-mixin-hash pred #f))))


(define class-mixin?
  (let ((class-mixin-hash (make-hash)))
    (for-each
      (lambda (class-yaml-str)
        (let ((mixin-flag (hash-ref (hash-ref (hash-ref biolink "classes") class-yaml-str) "mixin" #f)))
          (when mixin-flag
            (let ((class-biolink-name (yaml-class-name-to-biolink-name class-yaml-str)))
              (hash-set! class-mixin-hash class-biolink-name #t)))))
      classes)
    (lambda (class)
      (hash-ref class-mixin-hash class #f))))



(define predicate-symmetric?
  (let ((predicate-symmetric-hash (make-hash)))
    (for-each
      (lambda (pred-yaml-str)
        (let ((symmetric-flag (hash-ref (hash-ref (hash-ref biolink "slots") pred-yaml-str) "symmetric" #f)))
          (when symmetric-flag
            (let ((pred-biolink-name (yaml-predicate-name-to-biolink-name pred-yaml-str)))
              (hash-set! predicate-symmetric-hash pred-biolink-name #t)))))
      slots)
    (lambda (pred)
      (hash-ref predicate-symmetric-hash pred #f))))



(define get-inverse-predicate
  (let ((inverse-predicate-hash (make-hash)))
    (for-each
      (lambda (pred-yaml-str)
        (let ((inverse-pred-yaml-str (hash-ref (hash-ref (hash-ref biolink "slots") pred-yaml-str) "inverse" #f)))
          (when inverse-pred-yaml-str
            (let ((pred-biolink-name (yaml-predicate-name-to-biolink-name pred-yaml-str))
                  (inverse-pred-biolink-name (yaml-predicate-name-to-biolink-name inverse-pred-yaml-str)))
              (hash-set! inverse-predicate-hash pred-biolink-name inverse-pred-biolink-name)
              (hash-set! inverse-predicate-hash inverse-pred-biolink-name pred-biolink-name)))))
      slots)
    (lambda (pred)
      (hash-ref inverse-predicate-hash pred #f))))



(define get-predicate-children
  (let ((predicate-children-hash (make-hash)))
    (for-each
      (lambda (child-yaml-str)
        (let ((parent-yaml-str (hash-ref (hash-ref (hash-ref biolink "slots") child-yaml-str) "is_a" #f)))
          (when parent-yaml-str
            (let ((child-biolink-name (yaml-predicate-name-to-biolink-name child-yaml-str))
                  (parent-biolink-name (yaml-predicate-name-to-biolink-name parent-yaml-str)))
              (let ((children-set (hash-ref predicate-children-hash parent-biolink-name (set))))
                (hash-set! predicate-children-hash parent-biolink-name
                           (set-union (set child-biolink-name) children-set)))))))
      slots)
    (lambda (pred)
      (hash-ref predicate-children-hash pred (set)))))


(define get-predicate-descendents*
  (lambda (preds)
    (let loop ((preds preds)
               (desc (set)))
      (cond
        [(null? preds) desc]
        [else (let ((pred (car preds)))
                (let ((d (get-predicate-descendents pred)))
                  (loop (cdr preds) (set-union d desc))))]))))

(define get-predicate-descendents
  (lambda (pred)
    (set-add
     (get-predicate-proper-descendents pred)
     pred)))

(define get-predicate-proper-descendents
  (let ((predicate-proper-descendents-hash (make-hash)))
    (for-each
      (lambda (parent-yaml-str)
        (let ((parent-biolink-name (yaml-predicate-name-to-biolink-name parent-yaml-str)))
          (let loop ((to-process-set (get-predicate-children parent-biolink-name))
                     (processed-set (set parent-biolink-name))
                     (descendents-set (set)))
            (cond
              [(set-empty? to-process-set)
               (hash-set! predicate-proper-descendents-hash parent-biolink-name descendents-set)]
              [else
               (let ((process-element (set-first to-process-set))
                     (to-process-set (set-rest to-process-set)))
                 (cond
                   [(set-member? processed-set process-element)
                    (loop to-process-set processed-set descendents-set)]
                   [else
                    (let ((children-set (get-predicate-children process-element))
                          (descendents-set (set-add descendents-set process-element)))
                      (let ((to-process-set
                             (set-union
                              (set-subtract children-set processed-set)
                              to-process-set)))
                        (loop to-process-set
                              (set-add processed-set process-element)
                              descendents-set)))]))]))))
      slots)
    (lambda (pred)
      (hash-ref predicate-proper-descendents-hash pred (set)))))


(define get-class-children
  (let ((class-children-hash (make-hash)))
    (for-each
      (lambda (child-yaml-str)
        (let ((parent-yaml-str (hash-ref (hash-ref (hash-ref biolink "classes") child-yaml-str) "is_a" #f)))
          (when parent-yaml-str
            (let ((child-biolink-name (yaml-class-name-to-biolink-name child-yaml-str))
                  (parent-biolink-name (yaml-class-name-to-biolink-name parent-yaml-str)))
              (let ((children-set (hash-ref class-children-hash parent-biolink-name (set))))
                (hash-set! class-children-hash parent-biolink-name
                           (set-union (set child-biolink-name) children-set)))))))
      classes)
    (lambda (class)
      (hash-ref class-children-hash class (set)))))

(define get-class-descendents*
  (lambda (classes)
    (let loop ((classes classes)
               (desc (set)))
      (cond
        [(null? classes) desc]
        [else (let ((class (car classes)))
                (let ((d (get-class-descendents class)))
                  (loop (cdr classes) (set-union d desc))))]))))

(define get-class-descendents
  (lambda (class)
    (set-add
     (get-class-proper-descendents class)
     class)))

(define get-class-proper-descendents
  (let ((class-proper-descendents-hash (make-hash)))
    (for-each
      (lambda (parent-yaml-str)
        (let ((parent-biolink-name (yaml-class-name-to-biolink-name parent-yaml-str)))
          (let loop ((to-process-set (get-class-children parent-biolink-name))
                     (processed-set (set parent-biolink-name))
                     (descendents-set (set)))
            (cond
              [(set-empty? to-process-set)
               (hash-set! class-proper-descendents-hash parent-biolink-name descendents-set)]
              [else
               (let ((process-element (set-first to-process-set))
                     (to-process-set (set-rest to-process-set)))
                 (cond
                   [(set-member? processed-set process-element)
                    (loop to-process-set processed-set descendents-set)]
                   [else
                    (let ((children-set (get-class-children process-element))
                          (descendents-set (set-add descendents-set process-element)))
                      (let ((to-process-set
                             (set-union
                              (set-subtract children-set processed-set)
                              to-process-set)))
                        (loop to-process-set
                              (set-add processed-set process-element)
                              descendents-set)))]))]))))
      classes)
    (lambda (class)
      (hash-ref class-proper-descendents-hash class (set)))))


(define get-predicates-from-mixin
  (let ((predicates-from-mixin (make-hash)))
    (for-each
      (lambda (pred-yaml-str)
        (let ((mixins (hash-ref (hash-ref (hash-ref biolink "slots") pred-yaml-str) "mixins" #f)))
          (when mixins
            (let ((pred-biolink-name (yaml-predicate-name-to-biolink-name pred-yaml-str))
                  (mixin* (map yaml-predicate-name-to-biolink-name mixins)))
              (for-each
                (lambda (mixin-biolink-name)
                  (let ((pred-set-for-mixin (hash-ref predicates-from-mixin mixin-biolink-name (set))))
                    (hash-set! predicates-from-mixin mixin-biolink-name
                               (set-union (set pred-biolink-name) pred-set-for-mixin))))
                mixin*)))))
      slots)
    (lambda (pred)
      (hash-ref predicates-from-mixin pred (set)))))


(define get-classes-from-mixin
  (let ((classes-from-mixin (make-hash)))
    (for-each
      (lambda (class-yaml-str)
        (let ((mixins (hash-ref (hash-ref (hash-ref biolink "classes") class-yaml-str) "mixins" #f)))
          (when mixins
            (let ((class-biolink-name (yaml-class-name-to-biolink-name class-yaml-str))
                  (mixin* (map yaml-class-name-to-biolink-name mixins)))
              (for-each
                (lambda (mixin-biolink-name)
                  (let ((class-set-for-mixin (hash-ref classes-from-mixin mixin-biolink-name (set))))
                    (hash-set! classes-from-mixin mixin-biolink-name
                               (set-union (set class-biolink-name) class-set-for-mixin))))
                mixin*)))))
      classes)
    (lambda (class)
      (hash-ref classes-from-mixin class (set)))))
