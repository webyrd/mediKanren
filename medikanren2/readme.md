# mediKanren 2

## Prerequisites

## Loading databases

## Running queries

## Lightweight reasoning

Relations for lightweight reasoning are defined in `lw-reasoning.rkt`, and are currently under development and subject to change.

Here is an example of getting synonyms and subclasses:

```
(require "lw-reasoning.rkt")

(define meformin "CHEBI:6801")

;; get diseases/phenotypes treated by meformin and its synonyms
(define treated-diseases
  (run* disease-or-phen
    (fresh (drug)
      (kgx-synonym drug meformin)
      (triple drug "biolink:treats" disease-or-phen)
      (conde ((is-a disease-or-phen "biolink:PhenotypicFeature")) ; *
             ((is-a disease-or-phen "biolink:Disease"))))))

(get-names-ls treated-diseases)

;; get subclasses
(get-names-ls
 (apply append
        (map (lambda (disease)
               (run* d (subclass-of+ d disease)))
             treated-diseases)))

```

## TRAPI service

Running the [TRAPI](https://github.com/NCATSTranslator/ReasonerAPI)  service:

```
racket server.rkt
```

exposes the following endpoints on port `8384`:

**/pmi/v1/query**
**/query**

Main TRAPI endpoint, accepts [TRAPI 1.1](https://github.com/NCATSTranslator/ReasonerAPI) requests. Requests are run one at a time, on a first-received basis, and are limited to 10 minutes query time.

**/**

Simple Web UI for testing queries.

**/health**

Simple health check: runs a trivial query and reports on query time. If other queries are running and/or queued to be run, this query will return only after the others have terminated or timed out.

