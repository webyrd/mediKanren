#lang racket/base

(provide
 )

(require
 "../../../../medikanren2/neo/neo-low-level/query-low-level-multi-db.rkt"
 "../../../../medikanren2/neo/neo-utils/neo-helpers-multi-db.rkt"
 "../../../../medikanren2/neo/neo-server/neo-server-utils.rkt"
 "../../../../medikanren2/neo/neo-reasoning/neo-biolink-reasoning-low-level.rkt"
 "../utils.rkt"
 json
 racket/format
 racket/list
 racket/match
 racket/set
 racket/pretty
 racket/string)

;; adverse affects for imatinib mesylate
(query:Known->X
 '("CHEBI:45783")
 '("biolink:causes"
   "biolink:exacerbates"
   "biolink:has_adverse_event"
   "biolink:contributes_to")
 #f)

;; contraindications for imatinib mesylate
(query:Known->X
 '("CHEBI:45783")
 '("biolink:contraindicated_in")
 #f)

;; the variable `all-predicates` is bound to a list of all supported
;; biolink predicates

;; other biolink predicates that might be useful:
#|
"biolink:contraindicated_in"
"biolink:has_side_effect"
"biolink:FDA_adverse_event_level"
"biolink:adverse_event_of"
"biolink:has_adverse_event"
"biolink:highest_FDA_approval_status"
"biolink:drug_regulatory_status_world_wide"
|#
