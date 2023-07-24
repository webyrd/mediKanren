#lang racket
(require "transform-utils.rkt")
(provide transform-node-tsv)

#|
Output node and node-props file formats:

<name-file>.node.scm
:ID

<name-file>.node-props.scm
:ID propname value
|#

(define transform-node-tsv
  (lambda (nodes-file-import-path
           node-file-export-path
           node-props-file-export-path
           which-kg)

    (printf "transform-node-tsv called\n")
    (printf "input nodes tsv: ~s\n" nodes-file-import-path)
    (printf "output node tsv: ~s\n" node-file-export-path)
    (printf "output node props tsv: ~s\n" node-props-file-export-path)
    
    (define node-out
      (open-output-file node-file-export-path))
    (fprintf node-out ":ID\n")
    (define node-props-out
      (open-output-file node-props-file-export-path))
    (fprintf node-props-out ":ID\tpropname\tvalue\n")

    (define nodes-in
      (open-input-file nodes-file-import-path))
    
    (let* ((header (read-line nodes-in 'any))
           (header (string-split header "\t" #:trim? #f)))
      (let loop ((i 0)
                 (seen-nodes (set))
                 (line-str (read-line nodes-in 'any)))
        (when (zero? (modulo i 100000))
          (printf "processing nodes line ~s\n" i))

        (cond
          ((eof-object? line-str)
           (close-input-port nodes-in)
           (close-output-port node-out)
           (close-output-port node-props-out)
           (printf "finished processing nodes\n\n"))
          (else
           (let ((line (efficient-no-trim-tab-string-split line-str)))               
             (let ((id (cond
                         [(eq? which-kg 'text-mining) (car line)]
                         [(eq? which-kg 'rtx-kg2) (list-ref line 7)])))
               (when (set-member? seen-nodes id)
                 (error 'make-kg-node (format "already seen node: ~a" id)))
               (fprintf node-out "~a\n" id)
               (let loop-inner ((props (cdr line))
                                (headers (cdr header)))                   
                 (when (not (null? props))
                   (unless (string=? "" (car props))
                     (let ((propname (car headers))
                           (value (car props)))
                       (fprintf node-props-out "~a\t~a\t~a\n" id propname value)))
                   (loop-inner (cdr props) (cdr headers))))
               (loop
                (add1 i)
                (set-add seen-nodes id)
                (read-line nodes-in 'any))))))))))
