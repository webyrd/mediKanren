#!/usr/bin/env racket
#lang racket
;;(require racket)
(require racket/cmdline)
(require yaml)
(require shell/pipeline)
(require chk)

;; logging approach 1: Actually try to make a log receiver and make it work.  AFAICT
;;   involves creating a thread, configuring flushing, etc.  Probably better, but complicated.
;; logging approach 2: KISS but abuse the API.
;;
;; This is approach 2:
(define log-warning displayln)

;; *** generic utilities ***

;; TODO: see if dynamic-wind preserves crash stacktraces better than with-handlers.
(define (with-finally thunk-cleanup thunk-run)
  (with-handlers 
      ((exn:fail? (lambda (ex)
        (thunk-cleanup)
        (raise ex))))
    (let ((x (thunk-run)))
      (thunk-cleanup)
      x)))

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

(define (dorash #:dry-run dry-run cmds)
  (unless (equal? dry-run 'quiet)
    (printf "~s\n" cmds))
  (unless dry-run
    (run-pipelines cmds)))


;; *** application configuration ***
(struct config
    (
      afile-yaml
      uri-remote-archive
      adir-local-archive
      adir-storage
      adir-install
      do-config-scm
      dry-run
      (adir-temp #:mutable)
      (ardbs #:mutable)
    ) #:transparent
      #:name config-t
      #:constructor-name config-new
    )

;;; Mnemonic: "ARchive of DataBase"
(struct ardb 
  (
      configkey             ; For --write-config-scm.  Leave configkey blank
                            ; to prevent the ardb from adding to config.scm.
      reldir
      sha1sum
      versionOfMedikanren
      filename
      format
  ) #:transparent
    #:name ardb-t
  	#:constructor-name ardb-new
  )

;; wow lots of work to have the file be yaml
(define (construct-ardb node)
    (define mapping (construct-mapping node))
    (ardb-new
      (hash-ref mapping "configkey" (lambda () 'null))
      (hash-ref mapping "reldir")
      (hash-ref mapping "sha1sum")
      (hash-ref mapping "versionOfMedikanren")
      (hash-ref mapping "filename")
      (hash-ref mapping "format")))
(define (represent-ardb p)
    (define mapping (make-hash))
    (hash-set! mapping "configkey" (ardb-configkey p))
    (hash-set! mapping "reldir" (ardb-reldir p))
    (hash-set! mapping "sha1sum" (ardb-sha1sum p))
    (hash-set! mapping "versionOfMedikanren" (ardb-versionOfMedikanren p))
    (hash-set! mapping "filename" (ardb-filename p))
    (hash-set! mapping "format" (ardb-format p))
    (represent-mapping "!ardb" mapping))
(define ardb-representer
    (yaml-representer ardb? represent-ardb))
(define ardb-constructor
    (yaml-constructor ardb? "!ardb" construct-ardb))

(define (ardb->yaml ardb)
    (parameterize (
      [yaml-representers (list ardb-representer)]
      [yaml-constructors (list ardb-constructor)]
      )
        (yaml->string ardb)))

(define (styaml->ardb styaml)
    (parameterize (
      [yaml-representers (list ardb-representer)]
      [yaml-constructors (list ardb-constructor)]
      )
        (string->yaml* styaml #:allow-undefined? #t)))

(define (yamlfile->ardb absfile)
    (parameterize (
      [yaml-representers (list ardb-representer)]
      [yaml-constructors (list ardb-constructor)]
      )
        (file->yaml* absfile)))

(define (dataconfig config)
  (yamlfile->ardb (config-afile-yaml config)))


(define (path-ver-from-st v)
  (cond
    ((string-prefix? v "v1.") "medikanren1")
    ((string-prefix? v "v2.") "medikanren2")
    (else (error (format "unknown versionOfMedikanren: ~a" v)))))

(define (path-ver ardb)
  (let ((v (ardb-versionOfMedikanren ardb)))
    (path-ver-from-st v)))

(define (adir-temp-prefix uri-remote-archive ardb)
  (format "~a/~a/~a" uri-remote-archive (path-ver ardb)
          (ardb-filename ardb)))

(define (adir-temp-prefix-* uri-remote-archive ardb)
  (format "~a*" (adir-temp-prefix uri-remote-archive ardb)))

;; *** commands for extracting, checking, and installing ***
(define (cmd-to-cat ardb dir-archive config)
  (cond
   ((member "split" (ardb-format ardb))
    `("sh" "-c" ,(format "cat ~a" (adir-temp-prefix-* dir-archive ardb))))
    (else `("cat" ,(adir-temp-prefix dir-archive ardb)))))

(define (cmds-to-sha1 ardb dir-archive config)
  `(((#:out) ()
      ,(cmd-to-cat ardb dir-archive config)
      ("sha1sum")
      ("cut" "-c1-40")
      )))

(define (cmds-to-extract sha1 ardb dir-archive config)
  `((() ()
      ("mkdir" "-p" ,(format "~a/~a" (config-adir-storage config) sha1)))
    (() ()
      ,(cmd-to-cat ardb dir-archive config)
      ("tar" "xzf" "-" "-C" ,(format "~a/~a" (config-adir-storage config) sha1))
      )))

(define (cmds-to-symlink sha1 ardb config)
  (let* (
    (pathOwned (build-path (config-adir-install config) (path-ver ardb) "data" (ardb-reldir ardb)))
    (adirOwned (path->string pathOwned))
    (adirParent (path->string (simplify-path (build-path pathOwned 'up)))))
  `((() () ("mkdir" "-p"
            ,adirParent))
    (() () ("rm" "-f"
            ,adirOwned))
    (() () ("ln" "-s"
            ,(path->string (build-path (config-adir-storage config) sha1 (ardb-reldir ardb)))
            ,adirOwned)))))

(define (run-check-extract-link dir-archive config #:dry-run dry-run)
  (for ((ardb (config-ardbs config)))
    (let (
        (sha1-expected (ardb-sha1sum ardb))
        (sha1
          (if dry-run
            (ardb-sha1sum ardb)
            (string-trim #:left? #f
              (dorash #:dry-run dry-run (cmds-to-sha1 ardb dir-archive config))))))
      (if dry-run
        (begin
          (dorash #:dry-run dry-run (cmds-to-extract sha1-expected ardb dir-archive config))
          (dorash #:dry-run dry-run (cmds-to-symlink sha1-expected ardb config)))
        (if (equal? sha1 sha1-expected)
          (begin
            (dorash #:dry-run dry-run (cmds-to-extract sha1 ardb dir-archive config))
            (dorash #:dry-run dry-run (cmds-to-symlink sha1 ardb config)))
          (error (format "sha1 ~a != expected ~a" sha1 sha1-expected))
      )))))

;; *** commands for syncing from a remote source ***
(define (include-for-sync ardb)
  `("--include" ,(format "*~a/~a*" (path-ver ardb) (ardb-filename ardb))))

(define (includes-for-sync config)
  (append-map include-for-sync (config-ardbs config)))

(define (cmds-to-sync config)
  `((() () (
      "env" "AWS_EC2_METADATA_DISABLED=true"
        ;; fix: "<botocore.awsrequest.AWSRequest object at 0x7f623ae5b040>"
        ;; https://github.com/aws/aws-cli/issues/5262
      "aws" "s3" "sync" "--quiet"
      "--exclude" "*"
      ,@(includes-for-sync config)
      ,(config-uri-remote-archive config)
      ,(config-adir-temp config)
      ))))

(define (cmds-rm-r absd)
  `((() () ("echo" "TODO" "rm" "-rf" ,absd))))

(define (gen-config-scm ver ardbs)
  (let* (
      (ardbs1
        (filter
          (lambda (ardb) (string-prefix? (ardb-versionOfMedikanren ardb) ver))
          ardbs)))
    `((databases ,@(append-map (lambda (ardb)
          (if (equal? (ardb-configkey ardb) 'null) '() (list (string->symbol (ardb-configkey ardb)))))
        ardbs1)))))

;; *** commands to automatically populate config.scm ***
(define (write-config-scm ver config)
  (let* (
      (absf (format "~a/~a/config.scm" (config-adir-install config) (path-ver-from-st ver)))
      (_ (make-parent-directory* absf))
      (fout1 (open-output-file absf #:exists 'replace))
      (cfg (gen-config-scm ver (config-ardbs config))))
    (writeln cfg fout1)
    (close-output-port fout1)))

(define (write-configs-scm config)
  (write-config-scm "v1." config)
  (write-config-scm "v2." config))

;; *** main program ***
(define (validate-env config)
  (define s3-id (getenv "ncats_s3_id"))
  (define s3-secret (getenv "ncats_s3_secret"))

  (unless s3-id (log-warning "environment variable ncats_s3_id is missing.  awscli may be confused."))
  (unless s3-secret (log-warning "environment variable ncats_s3_secret is missing.  awscli may be confused.")))

(define (setup-teardown-run-install config)
  (validate-env config)
  (let* (
      (dry-run (config-dry-run config))
      (adir-temp (path->string (make-temporary-file "medikanren_~a" 'directory #f))))
    (set-config-adir-temp! config adir-temp)
    (with-finally
      (lambda () (dorash #:dry-run dry-run (cmds-rm-r adir-temp)))
      (lambda ()
          (dorash #:dry-run dry-run (cmds-to-sync config))
          (run-check-extract-link (config-adir-temp config) config #:dry-run dry-run)))))

(define (run-check-extract-link-only config)
  (let* (
      (dry-run (config-dry-run config))
        )
    (run-check-extract-link (config-adir-local-archive config) config #:dry-run dry-run)))


(define (run-main config)
  (cond
    ((not (config-afile-yaml config)) (error "--dir-root required"))
    ((not (config-adir-install config)) (error "--dir-install required"))
    ((not (config-adir-storage config)) (error "--dir-storage required"))
    ((not (or (config-uri-remote-archive config) (config-adir-local-archive config))) 
      (error "either --uri-remote-archive or --dir-local-archive is required"))
    (else
      (set-config-ardbs! config (append-map (lambda (x) x) (dataconfig config)))
      (cond
        ((config-uri-remote-archive config) (setup-teardown-run-install config))
        ((config-adir-local-archive config) (run-check-extract-link-only config))
        (else (error "Nothing to do.  Pass --help for usage.")))
      (when (config-do-config-scm config)
        (write-configs-scm config))
        )))

;; *** CLI parsing ***
(define (parse-configuration)
  (define afile-yaml (make-parameter #f))
  (define uri-remote-archive (make-parameter #f))
  (define adir-local-archive (make-parameter #f))
  (define adir-storage (make-parameter #f))
  (define adir-install (make-parameter #f))
  (define do-config-scm (make-parameter #f))
  (define dry-run (make-parameter #f))
  (command-line
   #:program "install-data-files.rkt"
   #:usage-help "install-data-files.rkt <options>"
   #:once-each
   [("--file-yaml") adir
                          "The absolute path to a data.yaml file"
                          (afile-yaml (string-trim #:left? #f adir))]
   [("--uri-remote-archive") uri
                          "The base URI of the remote data repository"
                          (uri-remote-archive (string-trim #:left? #f uri))]
   [("--dir-local-archive") adir
                          "The base URI of the remote data repository"
                          (adir-local-archive (string-trim #:left? #f adir))]
   [("--dir-storage") adir
                          "The base absolute directory for storing data"
                          (adir-storage (string-trim #:left? #f adir))]
   [("--dir-install") adir
                          "The base absolute directory for installing data"
                          (adir-install (string-trim #:left? #f adir))]
   [("--write-config-scm")
                          "Generate and overwrite config.scm files for extracted data"
                          (do-config-scm #t)]
   [("--dry-run")
                          "Print commands to run.  Do not run."
                          (dry-run #t)]
   #:args ()
   '())
   (config-new
      (afile-yaml)
      (uri-remote-archive)
      (adir-local-archive)
      (adir-storage)
      (adir-install)
      (do-config-scm)
      (dry-run)
      #f
      '()))

(module+ main
  (run-main (parse-configuration))
)

(module+ test
    (define ardb-sample (ardb-new "rtx2-20210204" "rtx2"
              "d56c1a0507b4e2c16f941214576af052f1279500" 
              "v2.0"
              "rtx2_2021_02_04.tar.gz"
              '()))
    (define config-sample-1 (config-new
            "/home/foo/root"
            "s3://bucket"
            #f
            "/path/to/storage"
            "/path/to/install"
            #f
            #t
            "/tmp/dir"
            (list ardb-sample)))
    (chk
      #:=
          (ardb->yaml ardb-sample)
          "!ardb\nsha1sum: d56c1a0507b4e2c16f941214576af052f1279500\nformat: []\nversionOfMedikanren: v2.0\nfilename: rtx2_2021_02_04.tar.gz\nreldir: rtx2\nconfigkey: rtx2-20210204\n"
    )
    (chk
      #:=
          (styaml->ardb (ardb->yaml ardb-sample))
          (list ardb-sample)
    )

    (chk
      (#:do
          (define ardb (car (styaml->ardb
            (string-join '(
              "!ardb"
              "sha1sum: d56c1a0507b4e2c16f941214576af052f1279500"
              "format: []"
              "versionOfMedikanren: v2.0"
              "filename: rtx2_2021_02_04.tar.gz"
              "reldir: rtx2")
              "\n"
            )))))
      (#:= (ardb-configkey ardb) 'null)
    )

    (chk
      (#:do
          (define configscm
            (gen-config-scm "v2." (car (styaml->ardb
              (string-join '(
                "---"
                "- !ardb"
                "  versionOfMedikanren: v2."
                "  configkey: yeast-sri-reference"
                "  reldir: nonempty"
                "  sha1sum: nonempty"
                "  size: 0"
                "  filename: nonempty"
                "  format:")
              "\n"
              ))))))
      (#:do (pretty-write configscm))
      (#:= (length (dict-ref configscm 'databases)) 1)
    )

    (chk
      (#:do
          (define configscm
            (gen-config-scm "v2." (car (styaml->ardb
              (string-join '(
                "---"
                "- !ardb"
                "  versionOfMedikanren: v2."
                "  reldir: nonempty"
                "  sha1sum: nonempty"
                "  size: 0"
                "  filename: nonempty"
                "  format:")
              "\n"
              ))))))
      (#:do (pretty-write configscm))
      (#:= (length (dict-ref configscm 'databases)) 0)
    )

    (chk
      (#:do
          (define configscm
            (gen-config-scm "v2." (car (styaml->ardb
              (string-join '(
                "---"
                "- !ardb"
                "  versionOfMedikanren: v2."
                "  configkey:"
                "  reldir: nonempty"
                "  sha1sum: nonempty"
                "  size: 0"
                "  filename: nonempty"
                "  format:")
              "\n"
              ))))))
      (#:do (pretty-write configscm))
      (#:= (length (dict-ref configscm 'databases)) 0)
    )

    (chk
      #:=
        (cmds-to-sync config-sample-1)
        `((() () (
            "env" "AWS_EC2_METADATA_DISABLED=true"
            "aws" "s3" "sync" "--quiet"
            "--exclude" "*"
            "--include" "*medikanren2/rtx2_2021_02_04.tar.gz*" 
            "s3://bucket" "/tmp/dir")))
    )

    (chk
      #:do (run-pipelines `((() () ("echo"))))
        #t)

    (chk
      #:do (run-pipelines `((() () ("echo" "-e"))))
        #t)

    (chk
      #:do (with-handlers ((exn:fail? (lambda (ex) #t))) (delete-file "echo-out.tmp"))
      #:do (run-pipelines `(((#:out) ("echo-out.tmp") ("echo" "hello"))))
      #:t (file-exists? "echo-out.tmp"))

    (chk
      #:do (define fout (open-output-string))
      #:do (run-pipelines `(((#:out) (,fout) ("echo" "-ne" "foo\\nbar\\n") ("sort"))))
      #:do (close-output-port fout)
      #:do (define st (get-output-string fout))
      #:= st "bar\nfoo\n")

    (chk
      #:do (define st (run-pipelines `(((#:out) () ("echo" "-ne" "hello")))))
      #:= st "hello")

    (chk
      #:do (run-check-extract-link (config-adir-temp config-sample-1) config-sample-1 #:dry-run 'quiet)
      #:t #t)
)
