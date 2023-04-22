#lang racket
(require "transform-utils.rkt")
(provide transform-edge-tsv)

#|
Output edge and edge-props file formats:

<name-file>.edge.tsv
:ID :START :END

<name-file>.edge-props.tsv
:ID propname value

<name-file>.qualifiededge.tsv
:ID predicate object-aspect object-direction subject object

|#

(define transform-edge-tsv
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path
           qualified-edge-file-export-path)

    (printf "transform-edge-tsv called\n")
    (printf "input edges tsv: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)
    (printf "output edge tsv: ~s\n" qualified-edge-file-export-path)
    
    (define edges-export-out
      (open-output-file edge-file-export-path))
    (fprintf edges-export-out ":ID\t:START\t:END\n")
    (define edge-props-out
      (open-output-file edge-props-file-export-path))
    (fprintf edge-props-out ":ID\tpropname\tvalue\n")
    (define qualified-edge-out
      (open-output-file qualified-edge-file-export-path))
    (fprintf qualified-edge-out ":ID\tpredicate\tobject-aspect\tobject-direction\tsubject\tobject\n")

    (define edges-in
      (open-input-file edges-file-import-path))
    
    (let* ((header (read-line edges-in))
           (header (string-split header "\t" #:trim? #f)))
      (let loop ((id 0)
                 (line-str (read-line edges-in)))
        (when (zero? (modulo id 100000))
          (printf "processing edges line ~s\n" id))

        (cond
          ((eof-object? line-str)
           (close-input-port edges-in)
           (close-output-port edges-export-out)
           (close-output-port edge-props-out)
           (close-output-port qualified-edge-out)
           (printf "finished processing edges\n\n"))
          (else
           (let ((line (efficient-no-trim-tab-string-split line-str)))
             (let ((predicate (list-ref line 1))
                   (object-aspect (list-ref line 8))
                   (object-direction (list-ref line 9))
                   (subject (list-ref line 0))
                   (object (list-ref line 2)))
               (fprintf edges-export-out "~a\t~a\t~a\n" id subject object)
               (fprintf qualified-edge-out "~a\t~a\t~a\t~a\t~a\t~a\n"
                        id predicate object-aspect object-direction subject object))
             (let loop-inner ((props line) ;; all the properties, including the subject and onbject
                              (headers header))
               (when (not (null? props))
                 (unless (string=? "" (car props))
                   (let ((propname (car headers))
                         (value (car props)))
                     (fprintf edge-props-out "~a\t~a\t~a\n" id propname value)))
                 (loop-inner (cdr props) (cdr headers))))
             (loop
              (add1 id)
              (read-line edges-in)))))))))
