#lang racket
(require "transform-utils.rkt")
(provide transform-edge-tsv)

#|
Output edge and edge-props file formats:

<name-file>.edge.scm
:ID :START :END

ex-row: 1 ENSEMBL:ENSG00000004059 ENSEMBL:ENSP00000000233

<name-file>.edge-props.scm
:ID propname value
|#

(define transform-edge-tsv
  (lambda (edges-file-import-path
           edge-file-export-path
           edge-props-file-export-path)

    (printf "transform-edge-tsv called\n")
    (printf "input edges tsv: ~s\n" edges-file-import-path)
    (printf "output edge tsv: ~s\n" edge-file-export-path)
    (printf "output edge props tsv: ~s\n" edge-props-file-export-path)
    
    (define edges-export-out
      (open-output-file edge-file-export-path))
    (fprintf edges-export-out ":ID\t:START\t:END\n")
    (define edge-props-out
      (open-output-file edge-props-file-export-path))
    (fprintf edge-props-out ":ID\tpropname\tvalue\n")

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
           (printf "finished processing edges\n\n"))
          (else
           (let ((line (efficient-no-trim-tab-string-split line-str)))
             (let ((start (car line)) ;; subject
                   (end (cadr line))) ;; object
               (fprintf edges-export-out "~a\t~a\t~a\n" id start end))
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
