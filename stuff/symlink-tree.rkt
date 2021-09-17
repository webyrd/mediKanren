#lang racket
(provide
  symlink-tree)
(require chk)

(define (redirect path path-src path-dst)
  (let* ((ep-dst (explode-path path-dst))
         (len-src (length (explode-path path-src)))
         (ep-src-tail (list-tail (explode-path path) len-src))
         (ep (append ep-dst ep-src-tail)))
    (apply build-path ep)))


(define (ensure-directory path)
  (when (file-exists? path)
    (delete-file path))
  (unless (directory-exists? path)
    (make-directory path)))

(define (symlink-tree path-src path-dst)
  (unless (directory-exists? path-src)
    (error "path-src must be a directory"))
  (unless (directory-exists? path-dst)
    (error "path-dst must be a directory"))
  (for ((path (find-files (lambda (x) #t) path-src)))
    (let ((path-new (redirect path path-src path-dst)))
      (cond
        ((directory-exists? path) (ensure-directory path-new))
        ((file-exists? path)
         (begin
           (when (file-exists? path-new)
             (delete-file path-new))
           (when (directory-exists? path-new)
             (error "not implemented: directory must be replaced by file"))
           (make-file-or-directory-link path path-new)))))))

(chk
 (#:=
  (path->string
   (redirect
    (build-path "/src/foo/bar")
    (build-path "/src")
    (build-path "/dst")))
  "/dst/foo/bar"))

(define (prepare-test path-0)
  (make-directory path-0)
  (make-directory (build-path path-0 "src"))
  (make-directory (build-path path-0 "src" "foo"))
  (display-to-file "" (build-path path-0 "src" "foo" "bar"))
  (make-directory (build-path path-0 "dst"))
  (list (build-path path-0 "src") (build-path path-0 "dst")))

(chk
 (#:do (define path-tmp1 (build-path (current-directory) "tmp1")))
 (#:do (define paths (prepare-test path-tmp1)))
 (#:do (define path-src (list-ref paths 0)))
 (#:do (define path-dst (list-ref paths 1)))
 (#:do (symlink-tree path-src path-dst))
 (#:t (link-exists? (build-path path-tmp1 "dst" "foo" "bar")))
 (#:=
  (resolve-path (build-path path-tmp1 "dst" "foo" "bar"))
  (build-path path-tmp1 "src" "foo" "bar"))
 (#:do (delete-directory/files path-tmp1)))
