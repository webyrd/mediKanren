#lang racket
(provide
 adir-temp
 dry-run
 has-dispatch?
 process-tbi)
(require chk)
(require aws/keys)
(require aws/s3)
(require "tagyaml.rkt")
(require "dispatch.rkt")
(require "../../stuff/run-shell-pipelines.rkt")
(require "metadata.rkt")
(require "current-source.rkt")
(require "cmd-helpers.rkt")
(require "dispatch-params.rkt")
(require "pipesig.rkt")
(require "task-checklist.rkt")
(require "data-import.rkt")
(require "notify-slack.rkt")

;task-build-index
;task-build-index-kgec

(define delimiter-s3 "/")


(define (update-vals-by-key keys vals k v)
  (let ((i (index-of k keys)))
    (if i
        (list-set vals i v)
        (error (printf "update-vals-by-key: key ~s not found in ~s" k keys)))))

(define dr-make-directory (dry-runify make-directory* 'make-directory))

(define (ensure-file-or-directory-link adir1 adir2)
  (when (link-exists? adir2)
    (delete-file adir2))
  (make-file-or-directory-link adir1 adir2))

(define dr-ensure-file-or-directory-link (dry-runify ensure-file-or-directory-link 'ensure-file-or-directory-link))

(define dr-delete-file (dry-runify delete-file 'delete-file))


(define (rfile-output tbi #:extension? (extension? #f))
  (define kgec (task-build-index-kgec tbi))
  (define kgid (kge-coord-kgid kgec))
  (define ver (kge-coord-ver kgec))
  (format "~a-~a~a" kgid ver (if extension? ".tgz" "")))

(define (expand-payload tbi)
  (define kgec (task-build-index-kgec tbi))
  (define kgid (kge-coord-kgid kgec))
  (define ver (kge-coord-ver kgec))
  (define afile-archive (path->string (build-path (adir-temp) (payload-from-kgec kgec))))
  (define sha1 (sha1sum afile-archive))
  ;; TODO: check sha1 once upstream sha1 is available
  ;;   https://github.com/NCATSTranslator/Knowledge_Graph_Exchange_Registry/issues/35

  ;; Q: Does the place for extracting the data need to be unique?
  ;; A: No, in fact it being unique would mean that a db/foo.rkt would have to change the
  ;; 'source-file-path in each of its define-relation/table statements, which would be burdensome.
  (define adir-data1 (path->string (build-path (adir-temp) "data")))
  (dr-make-directory adir-data1)
  (dr-make-directory (path->string (build-path (adir-temp) "data" "upstream")))
  (define adir-payload (path->string (build-path (adir-temp) "data" "upstream" kgid))) ; ver purposely omitted
  (dr-make-directory adir-payload)
  (define adir-data2 (path->string (build-path (adir-repo-ingest) "medikanren2" "data")))
  (define cmds-expand
    `((() () ("ls" "-lR" ,(adir-temp)))
      (() ()
          ("tar" "xzf" ,afile-archive "-C" ,adir-payload))
      (() () ("ls" "-lR" ,(adir-temp)))
      ))
  ; TODO capture ls -lR of what was in the archive
  ;(define cmds^ (run-pipeline-stdin-from-request request-inport cmds))
  (dr-ensure-file-or-directory-link adir-data1 adir-data2)
  (run-cmds cmds-expand)
  (dr-delete-file afile-archive))

(define (afile-archout tbi)
  (define rfile (rfile-output tbi #:extension? #t))
  (path->string (build-path (adir-temp) rfile)))

(define (path-split tbi)
  (define kgid (kge-coord-kgid (task-build-index-kgec tbi)))
  (define ver (kge-coord-ver (task-build-index-kgec tbi)))
  (build-path (adir-temp) "split" kgid ver))

(define (compress-out tbi)
  (printf "about to compress-out\n")
  (define kgec (task-build-index-kgec tbi))
  (let ((local-name (local-name-from-kg (kge-coord-kgid kgec) (kge-coord-ver kgec))))
    (begin
      (define kgid (kge-coord-kgid (task-build-index-kgec tbi)))
      (define ver (kge-coord-ver (task-build-index-kgec tbi)))
      (define adir-data1 (build-path (adir-temp) "data"))
      (define rfile (rfile-output tbi))
      (define afile-archout1 (afile-archout tbi))
      (define path-split1 (path-split tbi))
      (dr-make-directory path-split1)
      (define afile-split1 (path->string (build-path path-split1 (format "~a.tgz.split." rfile))))
      (run-cmds
       `(  (() () ("tar" "czf" ,afile-archout1 "-C" ,adir-data1 ,local-name))
           (() () ("split" "--bytes=1G" ,afile-archout1 ,afile-split1))
           (() () ("ls" "-l" ,(path->string path-split1)))
           ))
      ; TODO: now that tgz is generated, sha1sum it and generate yaml
      )))

(define (upload-archive-out s3dir tbi)
  (define path-split1 (path-split tbi))
  (define patels (directory-list path-split1 #:build? #f))
  (for ((patel patels))
    (let ((s3path (format "~a/~a" s3dir patel)))
      (multipart-put/file s3path (build-path path-split1 patel))
      ; TODO copy yaml
      )))

(define dr-upload-archive-out (dry-runify upload-archive-out 'upload-archive-out))

(define (check-for-payload tbi)
  (define afile (path->string (build-path (adir-temp) (payload-from-kgec (task-build-index-kgec tbi)))))
  (if (dry-run)
      (displayln `(check-for-payload ,afile))
      (unless (file-exists? afile)
        (error (format "Caller must supply a file at: ~a" afile)))))

(define (get-dbname tbi)
  (define kgec (task-build-index-kgec tbi))
  (define rfile-to-require (require-file-from-kg (kge-coord-kgid kgec) (kge-coord-ver kgec)))
  (define cmd (cmd-get-dbnames (adir-repo-ingest) rfile-to-require))
  (define stout (run-pipelines cmd))
  (define dbnames (read (open-input-string stout)))
  ;; We only build one db at a time, from a clean directory, so we should be the only
  (match dbnames
    (`(quote . ((,dbname))) (symbol->string dbname))
    (`(quote . ((,dbname . ,_)))
      (error "get-dbname: Detected multiple calls to database-extend-relations!.  Only one is allowed."))
    (`(quote . (())) ; TODO: Really this is a warning, but we don't currently have a way to report warnings
      (error "get-dbname: No db found.  Please call database-extend-relations! from your db wrapper."))
    (_ (error "get-dbname: internal error."))))

(define (yamlexpr-for-install-data-files tbi psig)
  (define kgec (task-build-index-kgec tbi))
  (define afile-archout1 (afile-archout tbi))
  (define size (file-size afile-archout1))
  (define sha1 (sha1sum afile-archout1))
  (define dbname (get-dbname tbi))
  `(#hash(
    ("versionOfMedikanren" . "v2")
    ("reldir" . ,(local-name-from-kg (kge-coord-kgid kgec) (kge-coord-ver kgec)))
    ("configkey" . ,dbname)
    ("sha1sum" . ,sha1)
    ("size" . ,size)
    ("filename" . ,(rfile-output tbi #:extension? #t))
    ("format" . (
      "tar.gz"
      "split"
    )))))

(define (s3path-yaml psig tsec-upload)
  (define s3adir (s3adir-for-psig psig tsec-upload))
  (format "~a/~a" s3adir "install.yaml"))

(define (upload-install-data-files-yaml tbi psig tsec-upload)
  (define yamlexpr (yamlexpr-for-install-data-files tbi psig))
  (define styaml (tagyaml->string "!ardb" yamlexpr))
  (put/bytes
    (s3path-yaml psig tsec-upload)
    (string->bytes/utf-8 (tagyaml->string "!ardb" yamlexpr))
    "application/yaml"))

(define dr-upload-install-data-files-yaml (dry-runify upload-install-data-files-yaml 'upload-install-data-files-yaml))

(define (report-to-slack state kgec psig tsec-upload)
    (match `(,state ,kgec ,psig ,tsec-upload)
      (`(ok ,kgec ,psig ,tsec-upload)
        (define s3path (s3path-yaml psig tsec-upload))
        (notify-slack (format "Sucessfully built ~a version ~a at s3://~a"
          (kge-coord-kgid kgec) (kge-coord-ver kgec) (s3path-yaml psig tsec-upload))))
      (_ #f)))

(define (process-tbi s3path-base psig tbi)
  (define hsig (psig-hash psig))
  (check-for-payload tbi)
  (expand-payload tbi)
  (dispatch-kgec (task-build-index-kgec tbi) hsig)
  (compress-out tbi)
  (define tsec-upload (floor (/ (current-milliseconds) 1000)))
  ; Use tsec-upload for both upload and yaml so that the relative path relationship
  ; for the yaml field "filename:" will be preserved
  (dr-upload-archive-out (s3adir-for-psig psig tsec-upload) tbi)
  (dr-upload-install-data-files-yaml tbi psig tsec-upload)
  (report-to-slack 'ok (task-build-index-kgec tbi) psig tsec-upload))




