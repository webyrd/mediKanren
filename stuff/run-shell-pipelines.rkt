#lang racket
(provide 
    run-pipelines)
(require shell/pipeline)
(require chk)

;; shell/pipelines can represent a pipeline step without
;; running it, via pipeline-member-spec.  However, it seems to
;; lack ability to represent a whole pipeline without running it.
;; So as a workaround we'll use a list of lists to be able
;; to test command generation without running commands.

;;; Run a list of prepared pipelines
(define (run-pipelines pipelines)
  (define ret #f)
  (for* ((pipeline pipelines))
    (let* (
           (ks (car pipeline))
           (vs (cadr pipeline))
           (fout (if (and
                      (not (null? ks))
                      (equal? (last ks) '#:out)
                      (equal? (- (length ks) 1) (length vs)))
                     (open-output-string)
                     #f))
           (vs (if fout
                   (append vs (list fout))
                   vs))
           (p (keyword-apply run-subprocess-pipeline
                             ks
                             vs
                             (pipeline-member-spec (caddr pipeline))
                             (map (lambda (cmd) (pipeline-member-spec cmd)) (cdddr pipeline)))))
      (if (not (equal? 0 (pipeline-status p)))
          (raise (format "status ~a attempting to run ~s" (pipeline-status p) pipeline))
          (printf "status ok\n"))
      (when fout (set! ret (get-output-string fout)))))
  ret)

(module+ test
  ;; How to run a simple command
  (chk
   #:do (run-pipelines `((() () ("echo"))))
   #t)

  ;; How to run a simple command with parameters
  (chk
   #:do (run-pipelines `((() () ("echo" "-e"))))
   #t)

  ;; How to capture stdout to a file
  (chk
   #:do (with-handlers ((exn:fail? (lambda (ex) #t))) (delete-file "echo-out.tmp"))
   #:do (run-pipelines `(((#:out) ("echo-out.tmp") ("echo" "hello"))))
   #:t (file-exists? "echo-out.tmp"))

  ;; How to capture stdout to a port
  (chk
   #:do (define fout (open-output-string))
   #:do (run-pipelines `(((#:out) (,fout) ("echo" "-ne" "foo\\nbar\\n") ("sort"))))
   #:do (close-output-port fout)
   #:do (define st (get-output-string fout))
   #:= st "bar\nfoo\n")

  ;; How to capture capture stdout to a string
  (chk
   #:do (define st (run-pipelines `(((#:out) () ("echo" "-ne" "hello")))))
   #:= st "hello")

  ;; How to source stdin from a port and capture stdout to a string
  (chk
   #:do (define fin (open-input-string "foo\nbar\nbaz\n"))
   #:do (define st (string-trim (run-pipelines `(((#:in #:out) (,fin) ("wc" "-l"))))))
   #:do (close-input-port fin)
   #:= st "3")

  ;; How to source stdin from a port and capture stdout to a port
  (chk
   #:do (define fin (open-input-string "foo\nbar\nbaz\n"))
   #:do (define fout (open-output-string))
   #:do (run-pipelines `(((#:in #:out) (,fin ,fout) ("wc" "-l"))))
   #:do (close-input-port fin)
   #:do (close-output-port fout)
   #:do (define st (string-trim (get-output-string fout)))
   #:= st "3"))
