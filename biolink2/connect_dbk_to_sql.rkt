#lang racket/base
(require db
         racket/sequence
         racket/stream
         racket/list
         racket/string)

;; create sql connection object
(define chembl_sql_local
  (mysql-connect
   #:user "mjpatton"
   #:database "chembl_27"
   #:server "localhost"
   #:port 3306
   #:password "open_dbs"))

(define query-->stream
  (lambda (db-conn query)
    (sequence->stream
     (in-query
      db-conn
      query
      #:fetch +inf.0))))

(define print-table-row-to-tsv
  (lambda (ls port)
    (cond
      ((null? ls)
       (fprintf port "~c" #\newline)
       (void))
      ((sql-null? (car ls))
       (fprintf port "~a~c" "NULL" #\tab)
       (print-table-row-to-tsv (cdr ls) port))
      ((null? (cdr ls))
       (fprintf port "~a" (car ls))
       (print-table-row-to-tsv (cdr ls) port))
      (else
       (fprintf port "~a~c" (car ls) #\tab)
       (print-table-row-to-tsv (cdr ls) port)))))

(define get-table-col-names
  (lambda (db-conn tbl-name)
    (map (lambda (x) (vector-ref x 0))
       (query-rows db-conn (string-append "DESCRIBE " tbl-name)))))

(define export-query-result-to-tsv
  (lambda (db-conn tbl-name query)
    (let* ((tbl-col-names (get-table-col-names db-conn tbl-name))
           (output-file (open-output-file (format "~achembl_~a_table.tsv" (find-system-path 'home-dir) tbl-name) #:exists 'replace))        
           (stream (query-->stream db-conn query)))
      (begin
        (print-table-row-to-tsv tbl-col-names output-file)
        (process-stream-to-tsv stream output-file)
        (close-output-port output-file)))))

(define process-stream-to-tsv
  (lambda (stream port)
    (cond
      ((stream-empty? stream)
       (void))
      (else
       (begin
         (print-table-row-to-tsv (call-with-values (lambda () (stream-first stream)) list) port)
         (process-stream-to-tsv (stream-rest stream) port))))))


;; test export 1.1MB file
;(export-query-result-to-tsv chembl_sql_local "target_dictionary" "SELECT * FROM target_dictionary;")


#|samples queries|#
#|
(define test1
  (query-rows chembl_sql_local "SELECT * FROM activities limit 5;"))

(define sql-test
  (query-rows chembl_sql_local "SELECT COUNT(*), activities.*, actdocs.abstract, target_dictionary.*,
target_dictionary.chembl_id as target_chembl_id, assays.*, molecule_dictionary.chembl_id, chembl_id_lookup.*,
assaydocs.abstract FROM activities JOIN molecule_dictionary ON (activities.molregno=molecule_dictionary.molregno) JOIN chembl_id_lookup ON (molecule_dictionary.chembl_id=chembl_id_lookup.chembl_id) JOIN assays ON (activities.assay_id=assays.assay_id) JOIN docs AS actdocs ON (activities.doc_id=actdocs.doc_id) JOIN docs AS assaydocs ON (assays.doc_id=assaydocs.doc_id) JOIN target_dictionary ON (target_dictionary.tid=assays.tid) WHERE activities.activity_id;"))
|#

#|sample query->tsv export|#
#|
(export-query-result-to-tsv chembl_sql_local "activities" "SELECT * FROM activities;")
(export-query-result-to-tsv chembl_sql_local "assays" "SELECT * FROM assays;")
(export-query-result-to-tsv chembl_sql_local "chembl_id_lookup" "SELECT * FROM chembl_id_lookup;")
(export-query-result-to-tsv chembl_sql_local "target_dictionary" "SELECT * FROM target_dictionary;")
(export-query-result-to-tsv chembl_sql_local "docs" "SELECT * FROM docs;")

(export-query-result-to-tsv
 chembl_sql_local
 "KG_chembl_activities"
 "CREATE TABLE KG_chembl_activities SELECT activities.*, actdocs.abstract, target_dictionary.*,)
target_dictionary.chembl_id as target_chembl_id, assays.*, molecule_dictionary.chembl_id, chembl_id_lookup.*,
assaydocs.abstract FROM activities JOIN molecule_dictionary ON (activities.molregno=molecule_dictionary.molregno) JOIN chembl_id_lookup ON (molecule_dictionary.chembl_id=chembl_id_lookup.chembl_id) JOIN assays ON (activities.assay_id=assays.assay_id) JOIN docs AS actdocs ON (activities.doc_id=actdocs.doc_id) JOIN docs AS assaydocs ON (assays.doc_id=assaydocs.doc_id) JOIN target_dictionary ON (target_dictionary.tid=assays.tid);")
|#


#|useful mysql user code|#
#|  
CREATE USER 'newuser'@'localhost' IDENTIFIED BY 'password';

ALTER USER 'newuser'@'localhost' IDENTIFIED BY 'password';

GRANT ALL PRIVILEGES ON * . * TO 'newuser'@'localhost'; 

FLUSH PRIVILEGES;

USE <dbname>
|#




