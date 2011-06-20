#lang scribble/base
@(require scriblib/figure
          slideshow/pict
          redex/pict
          (except-in redex/reduction-semantics plug)
          "../2-models/models.rkt"
          "../sem-sem/reduction.rkt"
          "../sem-sem/patterns.rkt"
          "typeset-reduction.rkt")

@title[#:tag "sec:reduction"]{A Semantics for Reduction}

We now put the notion of matching from @secref{sec:match-rules} to work in a
formalization of the standard notation for context-sensitive reduction rules. 
As with patterns, we consider a core specification language that lacks
many of the conveniences of a language like Redex but nevertheless addresses the
principal ideas.

@figure["fig:reduction" "A semantics for reduction"]{
@(centered (render-reduction))
}

@Figure-ref{fig:reduction} shows our definition, in which a reduction rule 
@(hbl-append 2 (rt p) (arrow->pict '-->) (rt r))
consists of a pattern @rt[p] and a term template @rt[r]. The grammar in the
figure's top-right gives the syntax for term templates, which include atoms,
references to a variables bound by the left-hand side, applications of meta-level
functions (e.g., substitution), hole-filling operations, and pairing operations.
The rule in the top-left defines when a term @rt[t] reduces to @rt[t_^′] by such
a rule, namely when @rt[p] matches @rt[t] and instantiating @rt[r] with the 
resulting bindings produces @rt[t_^′].

The rest of the figure defines template instantiation. Atoms instantiate to 
themselves, variables instantiate to their values, and meta-applications
instantiate to the result of applying the meta-function to the instantiated
argument template. 

The instantiation of @rt[:in-hole] templates makes use of a generic @rt[plug]
function, defined on contexts @rt[C]. This function follows the path 
recorded for @rt[C] when it was constructed by decomposition. When plugging 
a context @rt[C_1] with a context @rt[C_2], @rt[plug] preserves the path 
superimposed on @rt[C_1], thereby connecting it with the path on @rt[C_2]. 
This extended path provides an  unambiguous interpretation of the second 
reduction rule in @figure-ref{fig:delim}, which plugs the result of plugging
a context with a context.

@(define-syntax-rule (Λdk-term t)
   (render-lw Λdk/red (to-lw t)))

The restriction of @rt[plug] to contexts creates a potential problem for rules
like this unusual definition for @Λdk-term[call/cc], which extends the captured 
continuation with a @Λdk-term[|+1|] frame:
@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation cont-plus-red)))
Although the rule does not explicitly define a path for the extended context,
one can be safely inferred, since the term paired with @Λdk-term[E] does not
contain any pluggable sub-terms. 

The instantiation rule for @rt[:cons] templates performs this inference via 
the function @rt[join]. When given a context and a term containing no contexts
(as defined by the auxiliary judgment form @rt[no-ctxts]), @rt[join] extends the
context's path through the extra layer. When both arguments contain contexts,
@rt[join] combines the terms with @rt[:cons], preventing possibly ambiguity
in a subsequent plugging operation.

@(define-syntax-rule (Λkp-term t)
   (render-lw Λdk/red (to-lw t)))

Note, however, that the embedded contexts themselves remain remain pluggable by
reduction rules and meta-functions that later pick apart the result term. For
example, consider another unusual definition for @Λkp-term[call/cc]:
@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation cont-pair-red)))
This rule calls @Λkp-term[v] with a pair of continuation values. The term 
denoting this pair is not itself pluggable, but the embedded contexts can
be plugged by subsequent reduction steps, after they are extracted by the
reduction rules for @Λkp-term[car] and @Λkp-term[cdr] (not shown).

In addition to these contrived reduction systems, the semantics in 
@figure-ref{fig:reduction} supports all of the systems in @secref{sec:examples}.