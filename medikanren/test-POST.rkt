#lang racket
(require net/http-client
         json)

(define (named-cuis->json named-cuis)
  (define (node cui name)
    (hash 'description     "something profound goes here"
          'id              cui
          'name            name
          'node_attributes 'null
          'symbol          'null
          'type            "chemical_substance"
          'uri             'null))
  (define (result cui name)
    (hash 'confidence  0.00000000000000001
          'description 'null
          'essence     name
          'id          'null
          'reasoner_id 'null
          'result_graph (hash 'edge_list '()
                              'node_list (list (node cui name)))
          'result_group                  'null
          'result_group_similarity_score 'null
          'result_type                   'null
          'row_data                      '()
          'score                         'null
          'score_direction               'null
          'score_name                    'null
          'text                          'null))
  (hash 'context                'null
        'datetime               'null
        'id                     'null
        'known_query_type_id    'null
        'message                'null
        'n_results              'null
        'original_question_text 'null
        'query_type_id          'null
        'reasoner_id            "mediKanren-0.00000000000000zerozero..."
        'response_code          "OK"
        'restated_question_text 'null
        'result_code            'null
        'result_list (map result (map car named-cuis) (map cdr named-cuis))
        'schema_version         "0.5"
        'table_column_names     '()
        'terms                  'null
        'tool_version           "Nope"
        'type                   "medical_translator_query_result"))

(define data (jsexpr->bytes
               (named-cuis->json
                 '(("OMIM:600001" .  "HEART DEFECTS, CONGENITAL, AND OTHER CONGENITAL ANOMALIES; HDCA")))))

;(define data
;"
;{
  ;\"context\": null,
  ;\"datetime\": null,
  ;\"id\": null,
  ;\"known_query_type_id\": null,
  ;\"message\": \"25 results found\",
  ;\"n_results\": null,
  ;\"original_question_text\": null,
  ;\"query_type_id\": null,
  ;\"reasoner_id\": null,
  ;\"response_code\": \"OK\",
  ;\"restated_question_text\": null,
  ;\"result_code\": null,
  ;\"result_list\": [
    ;{
      ;\"confidence\": 0.9993358516965531,
      ;\"description\": null,
      ;\"essence\": \"HEART DEFECTS, CONGENITAL, AND OTHER CONGENITAL ANOMALIES; HDCA\",
      ;\"id\": null,
      ;\"reasoner_id\": null,
      ;\"result_graph\": {
        ;\"edge_list\": [
          ;{
            ;\"attribute_list\": null,
            ;\"confidence\": null,
            ;\"evidence_type\": null,
            ;\"is_defined_by\": \"RTX\",
            ;\"negated\": null,
            ;\"provided_by\": \"BioLink\",
            ;\"publications\": null,
            ;\"qualifiers\": null,
            ;\"relation\": null,
            ;\"source_id\": \"DOID:9352\",
            ;\"target_id\": \"HP:0011466\",
            ;\"type\": \"has_phenotype\"
          ;},
          ;{
            ;\"attribute_list\": null,
            ;\"confidence\": null,
            ;\"evidence_type\": null,
            ;\"is_defined_by\": \"RTX\",
            ;\"negated\": null,
            ;\"provided_by\": \"BioLink\",
            ;\"publications\": null,
            ;\"qualifiers\": null,
            ;\"relation\": null,
            ;\"source_id\": \"OMIM:600001\",
            ;\"target_id\": \"HP:0011466\",
            ;\"type\": \"has_phenotype\"
          ;}
        ;],
        ;\"node_list\": [
          ;{
            ;\"description\": \"Absence or underdevelopment of the gallbladder.\",
            ;\"id\": \"HP:0011466\",
            ;\"name\": \"Aplasia/Hypoplasia of the gallbladder\",
            ;\"node_attributes\": null,
            ;\"symbol\": null,
            ;\"type\": \"phenotypic_feature\",
            ;\"uri\": \"http://purl.obolibrary.org/obo/HP_0011466\"
          ;},
          ;{
            ;\"description\": \"A diabetes mellitus that involves high blood glucose resulting from cells fail to use insulin properly.\",
            ;\"id\": \"DOID:9352\",
            ;\"name\": \"type 2 diabetes mellitus\",
            ;\"node_attributes\": null,
            ;\"symbol\": null,
            ;\"type\": \"disease\",
            ;\"uri\": \"http://purl.obolibrary.org/obo/DOID_9352\"
          ;},
          ;{
            ;\"description\": \"None\",
            ;\"id\": \"OMIM:600001\",
            ;\"name\": \"HEART DEFECTS, CONGENITAL, AND OTHER CONGENITAL ANOMALIES; HDCA\",
            ;\"node_attributes\": null,
            ;\"symbol\": null,
            ;\"type\": \"disease\",
            ;\"uri\": \"http://purl.obolibrary.org/obo/OMIM_600001\"
          ;}
        ;]
      ;},
      ;\"result_group\": null,
      ;\"result_group_similarity_score\": null,
      ;\"result_type\": null,
      ;\"row_data\": [
        ;\"type 2 diabetes mellitus\",
        ;\"DOID:9352\",
        ;\"HEART DEFECTS, CONGENITAL, AND OTHER CONGENITAL ANOMALIES; HDCA\",
        ;\"OMIM:600001\",
        ;\"0.000664\"
      ;],
      ;\"score\": null,
      ;\"score_direction\": null,
      ;\"score_name\": null,
      ;\"text\": \"The drug HEART DEFECTS, CONGENITAL, AND OTHER CONGENITAL ANOMALIES; HDCA is predicted to treat type 2 diabetes mellitus.\"
    ;},
  ;],
  ;\"schema_version\": \"0.5\",
  ;\"table_column_names\": [
    ;\"input disease name\",
    ;\"input disease ID\",
    ;\"output disease name\",
    ;\"output disease ID\",
    ;\"path weight\"
  ;],
  ;\"terms\": null,
  ;\"tool_version\": \"RTX 0.5.4\",
  ;\"type\": \"medical_translator_query_result\"
;}
;"
  ;)

(displayln `(sending: ,data))

(define-values (status headers in)
  (http-sendrecv "localhost"
                 "/query"
                 #:port 8000
                 #:ssl? #f
                 #:version "1.1"
                 #:method "POST"
                 #:data data))

(displayln status)
(displayln headers)
(displayln (port->string in))
(close-input-port in)
