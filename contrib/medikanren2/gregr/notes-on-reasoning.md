# Notes on reasoning for Translator

I'm interested in improving Translator's design to make sure it can support
real world use.  Specifically, I'm going to consider aspects of the design
that are related to reasoning.

## Retrieval parameters

Some retrieval parameters involving tradeoffs between precision and recall:
- Facts only vs. facts and inferences
  - Should we only consider ground-truth assertions coming directly from
    knowledge sources, or should we also consider assertions derived from
    reasoning about existing assertions?
- Exact ontology matching vs. plausible ontology matching
  - Should we only consider facts that are subtypes of what we are looking for?
    Or can we also admit supertypes, which may not be what we're looking for,
    but which we can't prove aren't what we're looking for?

Full recall seems like the right default, as long as results include some
measure of confidence.  Users and downstream systems can then make their own
choice of precision by filtering on confidence and other result structure.

Currently, Translator isn't designed for full recall:
- It presupposes "facts only" mode (ironically).
  - There is no formal way to express inference derivations in the Translator
    Reasoner API!
- It commits to exact ontology matching.
  - https://github.com/NCATSTranslator/TranslatorArchitecture/blob/master/README.md
    point 9 requires KPs to return identifiers and predicates that are a subtype of
    what was asked for.

## Ontology matching

We'll start with ontology matching since it's an easier issue to address.  For
tractable support of both full recall and the option for more precision, KPs
should return both subtypes and supertypes of what was asked for.

Consider the `regulates` predicate and its subtypes `negatively_regulates` and
`positively_regulates`:

- `A regulates B`
  - `A negatively_regulates B`
  - `A positively_regulates B`

If a user is interested in `A negatively_regulates B`, a KP is only allowed to
return edges with predicate `negatively_regulates`.  Yet it is possible that a
KP also contains less a specific edge claiming that `A regulates B`, possibly
because it was sourced from natural language processing (NLP).  While it wasn't
possible to determine the specific form of regulation involved in this claim,
the possibility that it is negative regulation can't be ruled out.  So this
claim could be one that the user is interested in!

If the user is interested in full recall, one workaround to consider is to have
the user ask for `A regulates B` instead of `A negatively_regulates B`.  In this
case a KP will return edges whose predicate is either `negatively_regulates` or
`regulates`.  The problem with this is the KP will also return edges whose
predicate is `positively_regulates`.  These edges are definitely not what the
user wants.

Sure, a user could filter these edges out, but consider what
happens with a taller ontology: given a complete binary-tree-shaped predicate
ontology of height `N`, asking for the most general predicate will return
results for `2^N` predicates.  If the user is only interested in one leaf
predicate and its plausible substitutes (supertypes), this can involve an
intractable amount of filtering.

Instead, if KPs are allowed to return supertypes as well as subtypes, the user
interested in full recall can ask for the specific predicate they are interested
in, and not have to do any filtering.  Alternatively, if the user is interested
in precision, results will only be returned for `N` predicates, which is a
tractable amount to filter.

## Inferred claims

The specification for the Translator Reasoner API,
https://github.com/NCATSTranslator/ReasonerAPI/blob/master/TranslatorReasonerAPI.yaml,
provides no formal mechanism for an ARA to describe a derived assertion that it
can infer from ground-truth assertions coming from KPs.  This description,
where a reasoning system would "show its work", is needed so that users and
downstream systems can validate the assumptions behind the inference.  Although
the API provides a way to describe support for ground-truth assertions, this
mechanism is not sufficient for describing inferences.  To properly describe an
inference, it is necessary to refer to supporting assertions. Since assertions
are graph edges, this means that the justification for an inferred edge needs to
refer to other edges.  Since inferences may be supported by other inferences,
this form of reference is recursive.

Instead of allowing inferred claims, we could insist that the user ask for the
exact graph structures they are interested in seeing results for.  This is what
the current design already insists.  But part of the value of Translator comes
from providing ARAs that are capable of synthesizing new information from
existing knowledge.  In order for an interested user to fully utilize this
synthesized information, they would have to know what forms of reasoning were
available and understand how to structure their queries to access results coming
from each form of reasoning.  This is a large burden to place on the user,
especially if they are non-technical.  It would be better to allow the user to
ask either naive or sophisticated questions, and automatically benefit from ARA
reasoning in both cases.

To remedy this situation, the Reasoner API needs general structure for
reasoning-based justification for inferred assertions, in addition to evidence
and provenance for KP assertions.  We could adopt a structure analogous to
proof-formation rules, where each step of inference would include an identifier
for the rule being invoked, and a list of sub-assertions that this rule depends
on.  These sub-assertions may be ground assertions, or may also be inferred,
giving justifications a tree-like structure.

### Example justification rule

For example, you can imagine using two 1-hop claims with particular predicates
`p0` and `p1` to justify belief in another 1-hop claim with predicate `p2`:

```
A-p0->B, B-p1->C
---------------- some-rule-name:p0+p1=p2
A-p2->C
```

Here are some instances of this 2 hops to 1 hop rule:
```
p0                   | p1                   | p2
------------------------------------------------------------------
positively_regulates | positively_regulates | positively_regulates
negatively_regulates | positively_regulates | negatively_regulates
positively_regulates | negatively_regulates | negatively_regulates
negatively_regulates | negatively_regulates | positively_regulates
```

### Normalization and conflation

With support for structured justification, inferences could even subsume
normalization.  If we treat normalization information as just another knowledge
graph containing equivalence edges, we can explicitly justify why we believe
that one concept identifier or predicate can stand in for another, and this
justification can be validated externally.  Nobody would need to implicitly
trust the normalization data coming from a centralized service.  This also
opens up the option for ARAs to perform selective conflation of concepts and
predicates while communicating what it's doing and why.
