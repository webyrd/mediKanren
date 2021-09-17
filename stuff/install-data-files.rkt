#!/usr/bin/env racket
#lang racket
;;(require racket)
(require racket/cmdline)
(require yaml)
(require shell/pipeline)
(require net/url-string)
(require "run-shell-pipelines.rkt")
(require (prefix-in cmd: "cmd-helpers.rkt"))
(require "symlink-tree.rkt")
(require racket/pretty)
(require chk)

;; logging approach 1: Actually try to make a log receiver and make it work.  AFAICT
;;   involves creating a thread, configuring flushing, etc.  Probably better, but complicated.
;; logging approach 2: KISS but abuse the API.
;;
;; This is approach 2:
(define (log-warning . args)
  (apply displayln args)
  (flush-output))

;; *** generic utilities ***

(define (with-finally thunk-cleanup thunk-run)
  (dynamic-wind
    (lambda () #f)
    thunk-run
    thunk-cleanup))

;; *** application configuration ***

(define afile-yaml (make-parameter #f))
(define uri-yaml (make-parameter #f))
(define uri-remote-archive (make-parameter #f))
(define adir-local-archive (make-parameter #f))
(define adir-storage (make-parameter #f))
(define adir-install (make-parameter #f))
(define do-config-scm (make-parameter #f))
(define omit-aws-workaround (make-parameter #f))
(define config-ardbs (make-parameter #f))     ; mutable
(define config-adir-temp (make-parameter #f)) ; mutable

;;; Mnemonic: "ARchive of DataBase"
(struct ardb 
  (
   kgid
   versionOfKg
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

(define (prefer-string v)
  (cond
    ((number? v) (number->string v))
    (else v)))

;; wow lots of work to have the file be yaml
(define (construct-ardb node)
  (define mapping (construct-mapping node))
  (ardb-new
   (hash-ref mapping "kgid" (lambda () 'null))
   (prefer-string (hash-ref mapping "versionOfKg" (lambda () 'null)))
   (hash-ref mapping "configkey" (lambda () 'null))
   (hash-ref mapping "reldir" (lambda () 'null))
   (hash-ref mapping "sha1sum")
   (hash-ref mapping "versionOfMedikanren")
   (hash-ref mapping "filename")
   (hash-ref mapping "format")))
(define (represent-ardb p)
  (define mapping (make-hash))
  (hash-set! mapping "kgid" (ardb-kgid p))
  (hash-set! mapping "versionOfKg" (ardb-versionOfKg p))
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

(define (styaml->ardbs styaml)
  (parameterize (
                 [yaml-representers (list ardb-representer)]
                 [yaml-constructors (list ardb-constructor)]
                 )
    (string->yaml* styaml #:allow-undefined? #t)))

(define (yamlfile->ardbs absfile)
  (parameterize (
                 [yaml-representers (list ardb-representer)]
                 [yaml-constructors (list ardb-constructor)]
                 )
    (append-map (lambda (x) x)
      (file->yaml* absfile))))

(define (sturl-dirname sturl)
  (define url1 (string->url sturl))
  (define path1 (url-path url1))
  (define path2 (take path1 (- (length path1) 1)))
  (url->string (struct-copy url url1 (path path2))))

(define (implicit-uri-remote?)
  (and (not (uri-remote-archive))
          (uri-yaml)))

(define (uri-remote)
  (if (implicit-uri-remote?)
    (sturl-dirname (uri-yaml))
    (uri-remote-archive)))

(define (yamluri->ardbs uri-yaml)
  (define afile-yaml-temp (path->string (build-path (string->path (config-adir-temp)) "tmp.yaml")))
  (parameterize
      ((cmd:dry-run #f))
    (cmd:run-cmds
    `(((#:out) () (,@(prefix-for-aws-workaround)
              "aws" "s3" "cp" "--quiet"
              ,uri-yaml
              ,afile-yaml-temp
              ))))
    (yamlfile->ardbs afile-yaml-temp)))

(define (dataconfig)
  (cond
    ((afile-yaml) (yamlfile->ardbs (afile-yaml)))
    ((uri-yaml) (yamluri->ardbs (uri-yaml)))
    (else (error "internal error"))))

(define (path-ver-from-st v)
  (match (map string->number (string-split (string-trim v "v") "."))
    (`(1 . ,_) "medikanren1")
    (`(2 . ,_) "medikanren2")
    (else (error (format "unknown versionOfMedikanren: ~a" v)))))

(define (path-ver ardb)
  (let ((v (ardb-versionOfMedikanren ardb)))
    (path-ver-from-st v)))

(define (dir-cat-temp dir-archive ardb)
  (if (implicit-uri-remote?)
    (format "~a/~a" dir-archive (ardb-filename ardb))
    (format "~a/~a/~a" dir-archive (path-ver ardb)
            (ardb-filename ardb))))

(define (dir-cat-temp-* dir-archive ardb)
  (format "~a*" (dir-cat-temp dir-archive ardb)))

;; *** commands for extracting, checking, and installing ***
(define (cmd-to-cat ardb dir-archive)
  (cond
    ((member "split" (ardb-format ardb))
     `("sh" "-c" ,(format "cat ~a" (dir-cat-temp-* dir-archive ardb))))
    (else `("cat" ,(dir-cat-temp dir-archive ardb)))))

(define (cmds-to-sha1 ardb dir-archive)
  `(((#:out) ()
             ,(cmd-to-cat ardb dir-archive)
             ("sha1sum")
             ("cut" "-c1-40")
             )))

(define (dirname afile)
  (path->string (simplify-path (build-path (string->path afile) 'up))))

(define (inst-subdir-from-ardb ardb)
  (if (new-format? ardb)
    (format "/~a/~a" (ardb-kgid ardb) (ardb-versionOfKg ardb))
    ""))

(define (cmds-to-extract sha1 ardb dir-archive)
  (let* ((adir-target (format "~a/~a~a" (adir-storage) sha1 (inst-subdir-from-ardb ardb)))
         (adir-target-parent (dirname adir-target))
         (adir-target-temp (format "~a/~a-temp" (adir-storage) sha1)))
    `((() ()
          ("rm" "-rf" ,adir-target-temp))
      (() ()
          ("mkdir" "-p" ,adir-target-temp))
      (() ()
          ,(cmd-to-cat ardb dir-archive)
          ("tar" "xzf" "-" "-C" ,adir-target-temp)
          )
      (() ()
          ("mkdir" "-p" ,adir-target-parent))
      (() ()
          ("mv" ,adir-target-temp ,adir-target)))))

(define (reldir-from-ardb ardb)
  (if (new-format? ardb)
    `(,(ardb-kgid ardb) ,(ardb-versionOfKg ardb))
    `(,(ardb-reldir ardb)))) ; This is now the only bona fide usage of ardb-reldir

(define (append-path path patels)
  (apply build-path (cons path patels)))

(define (do-symlink-impl path-src path-dest)
  (make-directory* path-dest)
  (make-directory* path-src)
  (symlink-tree path-src path-dest))

(define dr-do-symlink-impl (cmd:dry-runify do-symlink-impl 'do-symlink))

(define (do-symlink sha1 ardb)
  (let* (
         (path-dest (append-path (build-path (adir-install) (path-ver ardb) "data") (reldir-from-ardb ardb)))
         (path-src (append-path (build-path (adir-storage) sha1) (reldir-from-ardb ardb))))
    (dr-do-symlink-impl path-src path-dest)))

(define (ardb-already-installed? ardb)
  ;; cmds-to-extract owns adir-ardb-storage and writes atomically.  Manual
  ;; manipulation of adir-storage is not supported.  Therefore,
  ;; if we find that adir-ardb-storage already exists, it must have been
  ;; the byproduct of a previous successful sha1 verification and install.
  ;; Therefore, we may "short circuit" by not repeating the installation.
  ;; Instead, we run an echo command to signal to the user.
  (let* ((adir (path->string (build-path (adir-storage) (ardb-sha1sum ardb)))))
    (directory-exists? adir)))

(define (cmds-echo-already-installed ardb)
  (let* ((adir (path->string (build-path (adir-storage) (ardb-sha1sum ardb)))))
    `((() () ("echo" ,adir " appears to be already installed, skipping")))))

(define (run-check-extract-link dir-archive)
  (for ((ardb (config-ardbs)))
    (let* ((sha1-expected (ardb-sha1sum ardb)))
      (cond 
        ((ardb-already-installed? ardb)
         (begin
           (cmd:run-cmds (cmds-echo-already-installed ardb))
           (do-symlink sha1-expected ardb)))
        ((cmd:dry-run)
         (let ((sha1 (ardb-sha1sum ardb)))
           (cmd:run-cmds (cmds-to-extract sha1-expected ardb dir-archive))
           (do-symlink sha1-expected ardb)))
        (else
         (let ((sha1 (string-trim
                      #:left? #f
                      (cmd:run-cmds (cmds-to-sha1 ardb dir-archive)))))
           (if (equal? sha1 sha1-expected)
               (begin
                 (cmd:run-cmds (cmds-to-extract sha1 ardb dir-archive))
                 (do-symlink sha1 ardb))
               (error (format "sha1 ~a != expected ~a" sha1 sha1-expected)))))))))

;; *** commands for syncing from a remote source ***
(define (include-for-sync ardb)
  (if (ardb-already-installed? ardb)
      '()
      (if (implicit-uri-remote?)
        `("--include" ,(format "*~a*" (ardb-filename ardb)))
        `("--include" ,(format "*~a/~a*" (path-ver ardb) (ardb-filename ardb))))))

(define (includes-for-sync)
  (append-map (lambda (ardb) (include-for-sync ardb)) (config-ardbs)))

(define (prefix-for-aws-workaround)
  (if (omit-aws-workaround)
    '()
    '("env" "AWS_EC2_METADATA_DISABLED=true")
        ;; fix: "<botocore.awsrequest.AWSRequest object at 0x7f623ae5b040>"
        ;; https://github.com/aws/aws-cli/issues/5262
  ))

(define (cmds-to-sync)
  `((() () (,@(prefix-for-aws-workaround)
            "aws" "s3" "sync" "--quiet"
            "--exclude" "*"
            ,@(includes-for-sync)
            ,(uri-remote)
            ,(config-adir-temp)
            ))))

(define (path-remove-wildcards path)
  (let* ((path1 (string-replace path "*" "" #:all? #t)))
    (string-replace path1 "?" "" #:all? #t)))

(define (cmds-rm-r absd)
  `((() () ("rm" "-rf" ,(path-remove-wildcards absd)))))

;;; TEMPORARY: migrated-to-new-db-versioning
;;; BEGIN functions that do compatibility between the new kgid based config keys and the
;;; older configkey field.

(define (new-format? ardb)
  (cond
    ((equal? (ardb-kgid ardb) 'null) #f)
    ((equal? (ardb-versionOfKg ardb) 'null) #f)
    (else #t)))

(define (get-configkey ardb)
  (cond
    ((not (ardb-configkey ardb)) #f)
    ((eq? (ardb-configkey ardb) 'null) #f)
    ((new-format? ardb) (ardb-kgid ardb))
    (else (ardb-configkey ardb))))

(define (idver-from-ardb ardb)
  (if (new-format? ardb)
    (values (ardb-kgid ardb) (ardb-versionOfKg ardb))
    (values #f #f)))

;;; END compatibility

(define (gen-config-scm ver ardbs)
  (let* (
         (ardbs1
          (filter
           (lambda (ardb) (string-prefix? (ardb-versionOfMedikanren ardb) ver))
           ardbs)))
    `((databases ,@(append-map (lambda (ardb)
                                 (if (get-configkey ardb) (list (string->symbol (get-configkey ardb))) '()))
                               ardbs1)))))

;; *** commands to automatically populate config.scm ***
(define (write-config-scm ver)
  (define cfg (gen-config-scm ver (config-ardbs)))
  (pretty-write `((generated-config-scm . ,cfg)))
  (unless (cmd:dry-run)
    (let* (
          (absf (format "~a/~a/etc/config.scm" (adir-install) (path-ver-from-st ver)))
          (_ (make-parent-directory* absf))
          (fout1 (open-output-file absf #:exists 'replace)))
      (writeln cfg fout1)
      (close-output-port fout1))))

(define (write-configs-scm)
  (write-config-scm "v1.")
  (write-config-scm "v2."))

(define (update-vfd vfd ardbs)
  (if (null? ardbs)
    vfd
    (let-values (((id ver) (idver-from-ardb (car ardbs))))
      (update-vfd
        (if id
          (dict-set vfd (string->symbol id) (string->symbol ver))
          vfd)
        (cdr ardbs)))))

(define (rewrite-config-installer-scm)
  (let* ((ver "v2.")
         (absf (format "~a/~a/etc/config.installer.scm" (adir-install) (path-ver-from-st ver)))
         (_ (make-parent-directory* absf))
         (fin1 (if (file-exists? absf) (open-input-file absf) (open-input-string "()")))
         (h (read fin1))
         (vfd (dict-ref h 'version-for-database '()))
         (vfd2 (update-vfd vfd (config-ardbs)))
         (h3 (dict-set h 'version-for-database vfd2)))
    (if (cmd:dry-run)
      (begin
        (pretty-write `((rewrite-config-installer-scm . ,h)))
        (newline))
      (let ((fout1 (open-output-file absf #:exists 'replace)))
        (pretty-write `((rewrite-config-installer-scm . ,h3)))
        (pretty-write h3 fout1)
        (newline fout1)
        (close-output-port fout1)))))

;; *** main program ***
(define (validate-env )
  (define s3-id (getenv "ncats_s3_id"))
  (define s3-secret (getenv "ncats_s3_secret"))

  (unless s3-id (log-warning "environment variable ncats_s3_id is missing.  awscli may be confused."))
  (unless s3-secret (log-warning "environment variable ncats_s3_secret is missing.  awscli may be confused.")))

(define (setup-teardown-run-install)
  (validate-env)
  (cmd:run-cmds (cmds-to-sync))
  (run-check-extract-link (config-adir-temp)))

;; TODO: inline
(define (run-from-local-archive)
  (run-check-extract-link (adir-local-archive)))

(define (run-main)
  (cmd:with-adir-temp-root
    (lambda ()
      (run-main-impl))))

(define (run-main-impl)
  (cond
    ((not (or (afile-yaml) (uri-yaml))) (error "--dir-root required"))
    ((not (adir-install)) (error "--dir-install required"))
    ((not (adir-storage)) (error "--dir-storage required"))
    ((not (or (uri-remote-archive) (implicit-uri-remote?) (adir-local-archive)))
     (error "either --uri-remote-archive or --dir-local-archive is required"))
    (else
     (config-adir-temp (cmd:adir-temp)) ; parameter set!
     (config-ardbs (dataconfig)) ; parameter set!
     (cond
       ((implicit-uri-remote?) (setup-teardown-run-install))
       ((uri-remote-archive) (setup-teardown-run-install))
       ((adir-local-archive) (run-from-local-archive))
       (else (error "Nothing to do.  Pass --help for usage.")))
     (write-configs-scm)
     (rewrite-config-installer-scm)
     )))

;; *** CLI parsing ***
(define (parse-configuration)
  (cmd:dry-run #f)
  (command-line
   #:program "install-data-files.rkt"
   #:usage-help "install-data-files.rkt <options>"
   #:once-each
   [("--file-yaml") adir
                    "The absolute path to a data.yaml file"
                    (afile-yaml (string-trim #:left? #f adir))]
   [("--uri-yaml") adir
                    "An S3 URI to a data.yaml file with data accessible at a relative path"
                    (uri-yaml (string-trim #:left? #f adir))]
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
   [("--omit-aws-workaround")
    "Generate and overwrite config.scm files for extracted data"
    (omit-aws-workaround #t)]
   [("--dry-run")
    "Print commands to run.  Do not run."
    (cmd:dry-run #t)]
   #:args ()
   '())
   'dont-care-because-we-set-parameters
  )

(module+ main
  (parse-configuration)
  (run-main)
  )

(module+ test
  (define ardb-sample (ardb-new "rtx" "1.0-kge"
                                "rtx2-20210204" "rtx2"
                                "d56c1a0507b4e2c16f941214576af052f1279500" 
                                "v2.0"
                                "rtx2_2021_02_04.tar.gz"
                                '()))

  (define (with-config-sample-1 thunk)
    (uri-remote-archive "s3://bucket")
    (adir-storage "/path/to/storage")
    (adir-install "/path/to/install")
    (config-ardbs (list ardb-sample))
    (thunk))

  (chk
   (#:do (ardb->yaml ardb-sample))
   (#:t #t)
   )
  (chk
   #:=
   (styaml->ardbs (ardb->yaml ardb-sample))
   (list ardb-sample)
   )

  (chk
   (#:do
    (define ardb (car (styaml->ardbs
                       (string-join '(
                                      "!ardb"
                                      "sha1sum: d56c1a0507b4e2c16f941214576af052f1279500"
                                      "format: []"
                                      "versionOfMedikanren: v2.0"
                                      "filename: rtx2_2021_02_04.tar.gz"
                                      "kgid: rtx"
                                      "versionOfKg: 1.0-kge"
                                      "reldir: rtx2")
                                    "\n"
                                    )))))
   (#:= (ardb-configkey ardb) 'null)
   )

  (chk
   (#:do
    (define configscm
      (gen-config-scm "v2." (car (styaml->ardbs
                                  (string-join '(
                                                 "---"
                                                 "- !ardb"
                                                 "  versionOfMedikanren: v2."
                                                 "  configkey: yeast-sri-reference"
                                                 "  reldir: nonempty"
                                                 "  sha1sum: nonempty"
                                                 "  size: 0"
                                                 "  filename: nonempty"
                                                 "  kgid: rtx"
                                                 "  versionOfKg: 1.0-kge"
                                                 "  format:")
                                               "\n"
                                               ))))))
   (#:do (pretty-write configscm))
   (#:= (length (dict-ref configscm 'databases)) 1)
   )

  (chk
   (#:do
    (define configscm
      (gen-config-scm "v2." (car (styaml->ardbs
                                  (string-join '(
                                                 "---"
                                                 "- !ardb"
                                                 "  versionOfMedikanren: v2."
                                                 "  reldir: nonempty"
                                                 "  sha1sum: nonempty"
                                                 "  size: 0"
                                                 "  filename: nonempty"
                                                 "  kgid: rtx"
                                                 "  versionOfKg: 1.0-kge"
                                                 "  format:")
                                               "\n"
                                               ))))))
   (#:do (pretty-write configscm))
   (#:= (length (dict-ref configscm 'databases)) 0)
   )

  (chk
   (#:do
    (define configscm
      (gen-config-scm "v2." (car (styaml->ardbs
                                  (string-join '(
                                                 "---"
                                                 "- !ardb"
                                                 "  versionOfMedikanren: v2."
                                                 "  configkey:"
                                                 "  reldir: nonempty"
                                                 "  sha1sum: nonempty"
                                                 "  size: 0"
                                                 "  filename: nonempty"
                                                 "  kgid: rtx"
                                                 "  versionOfKg: 1.0-kge"
                                                 "  format:")
                                               "\n"
                                               ))))))
   (#:do (pretty-write configscm))
   (#:= (length (dict-ref configscm 'databases)) 0)
   )

  (chk
   #:=
   (with-config-sample-1
    (lambda ()
      (config-adir-temp "/tmp/dir")
      (cmds-to-sync)))
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
   #:do (with-config-sample-1
    (lambda ()
      (run-check-extract-link (config-adir-temp))))
   #:t #t)

  (chk
   #:= (path-remove-wildcards "/foo/*bar*") "/foo/bar"
   #:= (path-remove-wildcards "/foo/?bar?") "/foo/bar")

  (chk
    (#:= (dirname "/foo/bar/baz")
      "/foo/bar/"))

  (chk
    (#:= (dirname "/foo/bar/baz/")
      "/foo/bar/"))
  )
