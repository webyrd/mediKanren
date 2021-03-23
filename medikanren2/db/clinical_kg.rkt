#lang racket/base
;; provide will load the materialized relation defined below
(provide patient-clinical-data)
(require "../base.rkt" (except-in racket/match ==))

(define path-clinical-data )

(define path-demographic-data )

(define in (open-input-file path-clinical-data))


(define-relation/table patient-clinical-data
  'path               "uab_emr_covid/patient_clinical_data"
  'source-file-path   path-clinical-data
  'source-file-header '("patient_id" "date_time" "type_curie" "type_string" "type_value" "type_unit")
  'attribute-names    '(patient_id date_time type_curie type_string type_value type_unit)
  ;; #f in the attrib-type means "any" type
  'attribute-types    '(nat nat string string string string)
  'map (value/syntax
         (lambda (row)
           (match-define (list patient_id date_time type_curie type_string type_value type_unit) row)
           (list (string->number patient_id)
                 (string->number date_time)
                 type_curie
                 type_string
                 type_value
                 type_unit)))
  ;; if you know curie and value, you can get a pointer to all other data
  'indexes '((type_curie type_value)
             (patient_id type_curie type_value)
             ))
