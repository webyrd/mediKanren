#lang racket/base
(require "../common.rkt" "../db/clinical_kg.rkt" racket/pretty
	 "../dbk/dsv.rkt" "../dbk/stream.rkt")
(provide (all-from-out "../common.rkt") (all-defined-out))

(define-syntax-rule (test name e expected)
  (begin (printf "Testing ~s:\n" name)
         (let ((answer (time e)))
           (unless (equal? answer expected)
             (pretty-write 'e)
             (printf "FAILED ~s:\n" name)
             (printf "  ANSWER:\n")
             (pretty-write answer)
             (printf "  EXPECTED:\n")
             (pretty-write expected)))))

;; test query
(define c19-test
  (list "LOINC:94500-6" "LOINC:41458-1" "LOINC:94756-4" "LOINC:94533-7" "LOINC:94507-1" "LOINC:94309-2"))

(define c19-test-positive-result
  (list "Positive" "Detected"))

(define thrombotic-event-curies
  (list "ICD-10-CM:I21.A1" "ICD-10-CM:I21.A9" "ICD-10-CM:I21.0"
	"ICD-10-CM:I21.21" "ICD-10-CM:I21.1" "ICD-10-CM:I21.4"
	"ICD-10-CM:I21.9" "ICD-10-CM:I21.A" "ICD-10-CM:I21.3"
	)
  )

#;(define c19-cohort
  (run* (id date curie string value units)
     (membero curie c19-test)
     (membero value c19-test-positive-result)
     (patient-clinical-data
      id
      date
      curie
      string
      value
      units)
     ))

#;(pretty-print (length c19-cohort))

(define c19-cohort-with-thrombotic-event
  (run 50 (id date curie string value units thrombotic-events)
	(membero curie c19-test)
	(membero value c19-test-positive-result)
	(=/= thrombotic-events '())
 	(patient-clinical-data
	 id
	 date
	 curie
	 string
	 value
	 units)
	(:==  thrombotic-events (id date)
	      (run* (thrombotic-event-date curie string)
		    (fresh (value units)
			   (<=o date thrombotic-event-date)
			   (membero curie thrombotic-event-curies)
			   (patient-clinical-data
			    id
			    thrombotic-event-date
			    curie
			    string
			    value
			    units))))))

(pretty-print c19-cohort-with-thrombotic-event)
