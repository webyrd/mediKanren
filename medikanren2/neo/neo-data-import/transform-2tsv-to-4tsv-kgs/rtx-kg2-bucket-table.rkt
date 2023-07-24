#lang racket/base

(require "transform-utils.rkt")
(provide kg2-buckets)

(define zero-pub (lambda (s) 0))
(define bucket-top (lambda (s) (build-buckets-with-top 7 s)))
(define bucket-interval (lambda (interval)
                          (lambda (s)
                            (build-buckets-with-interval interval s))))

(define kg2-buckets
  (hash "biolink:directly_physically_interacts_with" bucket-top
        "biolink:has_increased_amount" zero-pub
        "biolink:has_metabolite" zero-pub
        "biolink:catalyzes" zero-pub
        "biolink:composed_primarily_of" zero-pub
        "biolink:precedes" (bucket-interval (list 0 1 2 (cons 3 6) (cons 7 57) (cons 58 133) (cons 134 221) 222))
        "biolink:coexists_with" (bucket-interval (list 0 1 (cons 2 10) (cons 11 22) (cons 23 70) (cons 71 115) (cons 116 178) 179))
        "biolink:disease_has_basis_in" zero-pub
        "biolink:gene_product_of" zero-pub
        "biolink:has_molecular_consequence" zero-pub
        "biolink:has_plasma_membrane_part" zero-pub
        "biolink:prevents" (bucket-interval (list 0 1 2 (cons 3 10) (cons 11 24) (cons 25 82) (cons 83 132) 133))
        "biolink:model_of" zero-pub
        "biolink:causes" (bucket-interval (list 0 1 (cons 2 3) (cons 4 16) (cons 17 41) (cons 42 99) (cons 100 161) 162))
        "biolink:exacerbates" (bucket-interval (list 0 1 2 3 (cons 4 9) (cons 10 14) (cons 15 21) 22))
        "biolink:expressed_in" zero-pub
        "biolink:gene_associated_with_condition" (bucket-interval (list 0 1 (cons 2 10) (cons 11 20) (cons 21 29) (cons 30 30) (cons 31 31) 32))
        "biolink:associated_with" zero-pub
        "biolink:derives_from" (bucket-interval (list 0 1 2 (cons 3 4) (cons 5 18) (cons 19 42) (cons 43 61) 62))
        "biolink:develops_from" zero-pub
        "biolink:translates_to"  zero-pub
        "biolink:affects" (bucket-interval (list 0 1 (cons 2 3) (cons 4 10) (cons 11 23) (cons 24 73) (cons 74 120) 121))
        "biolink:physically_interacts_with" bucket-top
        "biolink:superclass_of" zero-pub
        "biolink:produces" (bucket-interval (list 0 1 (cons 2 3) (cons 4 18) (cons 19 39) (cons 40 81) (cons 82 131) 132))
        "biolink:has_participant" zero-pub
        "biolink:in_taxon" zero-pub
        "biolink:temporally_related_to" zero-pub
        "biolink:close_match" bucket-top
        "biolink:regulates" bucket-top
        "biolink:enables" bucket-top
        "biolink:chemically_similar_to" zero-pub
        "biolink:colocalizes_with" bucket-top
        "biolink:same_as" zero-pub
        "biolink:diagnoses" (bucket-interval (list 0 1 (cons 2 5) (cons 6 15) (cons 16 49) (cons 50 129) (cons 130 208) 209))
        "biolink:has_phenotype" zero-pub
        "biolink:disease_has_location" zero-pub
        "biolink:has_output" zero-pub
        "biolink:overlaps" zero-pub
        "biolink:has_member" zero-pub
        "biolink:lacks_part" zero-pub
        "biolink:capable_of" zero-pub
        "biolink:occurs_in" (bucket-interval (list 0 1 (cons 2 28) (cons 29 90) (cons 91 360) (cons 361 1092) (cons 1093 1972) 1973))
        "biolink:subclass_of" (lambda (s) 1111)
        "biolink:is_sequence_variant_of" zero-pub
        "biolink:biomarker_for" zero-pub
        "biolink:has_input" (bucket-interval (list 0 1 (cons 2 5) (cons 6 16) (cons 17 39) (cons 40 512) (cons 513 913) 914))
        "biolink:mentions" zero-pub
        "biolink:contraindicated_for" zero-pub
        "biolink:has_decreased_amount" zero-pub
        "biolink:homologous_to" zero-pub
        "biolink:transcribed_from" zero-pub
        "biolink:has_not_completed" zero-pub
        "biolink:manifestation_of" (bucket-interval (list 0 1 (cons 2 3) (cons 4 6) (cons 7 13) (cons 14 31) (cons 32 46) 47))
        "biolink:treats" (bucket-interval (list 0 1 (cons 2 3) (cons 4 7) (cons 8 23) (cons 24 175) (cons 176 325) 326))
        "biolink:related_to" (bucket-interval (list 0 1 (cons 2 4) (cons 5 19) (cons 20 57) (cons 58 174) (cons 175 386) 387))
        "biolink:has_part" (bucket-interval (list 0 1 (cons 2 6) (cons 7 30) (cons 31 101) (cons 102 421) (cons 422 760) 761))
        "biolink:predisposes" (bucket-interval (list 0 1 (cons 2 5) (cons 6 13) (cons 14 36) (cons 37 64) (cons 65 100) 101))
        "biolink:contributes_to" bucket-top
        "biolink:has_completed" zero-pub
        "biolink:actively_involved_in" bucket-top
        "biolink:interacts_with" (bucket-interval (list 0 1 (cons 2 5) (cons 6 14) (cons 15 32) (cons 33 56) (cons 57 86) 87))
        "biolink:located_in" (bucket-interval (list 0 1 (cons 2 12) (cons 13 39) (cons 40 110) (cons 111 249) (cons 250 431) 432))
        "biolink:correlated_with" zero-pub
        "biolink:disrupts" (bucket-interval (list 0 1 2 (cons 3 6) (cons 7 14) (cons 15 42) (cons 43 64) 65))))
        
        





        
        





        



        


        