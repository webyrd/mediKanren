#lang racket
(provide (all-defined-out))
(require xml)

#lang racket
(provide (all-defined-out))
(require xml)
(require net/http-client)
 
;; download xml for national clinical trials found here: https://clinicaltrials.gov/
(define all_nct_xml_dir
  "git/KG_creation/data/raw/AllPublicXML")

(define nct-https
  (http-conn-open
   "clinicaltrials.gov"
   #:ssl? #t))

;;"https://clinicaltrials.gov/"
;; bytes? http 200 = ok request, 400 = bad request, 500 = server error, 404 = page not found
;; lob? info of transaction 
;; ip? input-port we can read data from 
(define nct-data-files
  (let-values (((bytes? lob? ip?)
                (http-sendrecv
                 "clinicaltrials.gov"
                 "/ct2/show/NCT03478891?displayxml=true"
                 ;;"AllPublicXML.zip"
                 #:ssl? #t
                 )))
    (list bytes? lob? ip?)))

#|
xexpr conversion of xml
(root
  (parent-element                         
     (element (attribute) <"data">)))         
|#
(define nct-data
  (xml->xexpr
   (document-element
    (read-xml
     (list-ref nct-data-files 2)))))


(define test
  (xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths))))))


(xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths)))))


(ftp-establish-connection
 "https://clinicaltrials.gov/AllPublicXML.zip"	 	 	 	 
 21	 	 	 	 
 ;user	 	 	 	 
 ;passwd
 )



(define user-home-path/str
  (path->string (find-system-path 'home-dir)))

(define nct_dir
  (string-append user-home-path/str all_nct_xml_dir))

(define nct-xml-file-paths
  (filter
   (lambda (x)
     (and (path-has-extension? x ".xml")
          (regexp-match #rx"^NCT[0-9]+.xml$" (path->string (file-name-from-path x)))))
          (sequence->list (in-directory nct_dir))))

;; NCT03478891


(define test
  (xexpr->string
   (xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths)))))))


#;(define t
  (filter
   (lambda (x) (and (not (null? x))
                   (not (string? x))))
   (xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths)))))))


#|
;; list of all file paths 
(define paths-to-unichem-files
  (filter
   (lambda (x)
     (and (path-has-extension? x ".txt")
          (regexp-match #rx"^src[0-9]+src[0-9]+.txt$" (path->string (file-name-from-path x)))))
          (sequence->list (in-directory path-unichem))))


(define xml_raw_files
  (parse_xml_files nct_dir))



(define parse_xml_files
  (lambda (path)
    (for/fold
     ([ls '()])
     ([f (in-directory path)] #:when (path-has-extension? f ".xml"))     
     (define xml-file-paths  (sequence->list f))
     #;(define parsed-xml-file (read-xml (open-input-file f)))
     (pretty-print xml-file-paths))))
|#


(define all_nct_xml_dir
  "git/KG_creation/data/raw/AllPublicXML")

(define user-home-path/str
  (path->string (find-system-path 'home-dir)))

(define nct_dir
  (string-append user-home-path/str all_nct_xml_dir))

(define nct-xml-file-paths
  (filter
   (lambda (x)
     (and (path-has-extension? x ".xml")
          (regexp-match #rx"^NCT[0-9]+.xml$" (path->string (file-name-from-path x)))))
          (sequence->list (in-directory nct_dir))))

(define test
  (xexpr->string
   (xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths)))))))


(define t
  (filter
   (lambda (x) (and (not (null? x))
                   (not (string? x))))
   (xml->xexpr
    (document-element
     (read-xml
      (open-input-file
       (car nct-xml-file-paths)))))))


#|
;; list of all file paths 
(define paths-to-unichem-files
  (filter
   (lambda (x)
     (and (path-has-extension? x ".txt")
          (regexp-match #rx"^src[0-9]+src[0-9]+.txt$" (path->string (file-name-from-path x)))))
          (sequence->list (in-directory path-unichem))))


(define xml_raw_files
  (parse_xml_files nct_dir))



(define parse_xml_files
  (lambda (path)
    (for/fold
     ([ls '()])
     ([f (in-directory path)] #:when (path-has-extension? f ".xml"))     
     (define xml-file-paths  (sequence->list f))
     #;(define parsed-xml-file (read-xml (open-input-file f)))
     (pretty-print xml-file-paths))))
|#
