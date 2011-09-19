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

@figure["fig:reduction" "A semantics for reduction (cases apply in order)"]{
@(centered (render-reduction))
}

@Figure-ref{fig:reduction} shows our definition. A user of Redex specifies
a grammar and rules of the shape
@(hbl-append 2 (rt p) (arrow->pict '-->) (rt r)),
each consisting of a pattern @rt[p] and a term template @rt[r]. 
Redex then uses the judgment in the upper-left corner of the figure to
determine if a particular term @rt[t] rewrites to @rt[t_^′].
The grammar in the
figure's top-right gives the syntax for term templates, which include atoms,
the context @rt[:hole],
references to variables bound by the left-hand side, applications of meta-level
functions (e.g., substitution), hole-filling operations, and pairing operations.

The rest of the figure defines template instantiation. Atoms 
and @rt[:hole] instantiate to 
themselves, variables instantiate to their values, and meta-applications
instantiate to the result of applying the meta-function to the instantiated
argument template. 

The instantiation of @rt[:in-hole] templates makes use of a generic @rt[plug]
function that accepts a term representing a context and a term to plug, and
returns the result of plugging the context with the term.

When @rt[plug]'s first argument is a context @rt[C], then it
follows the path  recorded for @rt[C] when it was constructed by decomposition
as shown in the first three rules for @rt[plug].
When @rt[plug]'s first argument is a term constructed with @rt[:cons], it
allows plugging only when there is an unambiguous location in which to plug.
Specifically, if there is a @rt[:left], @rt[:right], or @rt[:hole] in both
sides of the @rt[:cons], then @rt[plug] is not well-defined (and thus neither is
@rt[inst]), but if there is a context on only one side, then @rt[plug], will 
fill that context.

When plugging 
a context @rt[C_1] with a context @rt[C_2], @rt[plug] preserves the path 
superimposed on @rt[C_1], thereby connecting it with the path on @rt[C_2]. 
This path extension is necessary, for example, to support the following rule
for an unusual control operator:
@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation cont-double-red)))

@(define-syntax-rule (Λk-term t)
   (render-lw Λdk/red (to-lw t)))

The rules that allow @rt[plug] to fill contexts embedded in
@rt[:cons] expressions helps with rules
that extend contexts, like this one for another unusual control operator:
@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation cont-plus-red)))
Although the rule does not explicitly define a path for the extended context
@Λk-term[(1+ E)],
one can be safely inferred, since the term paired with @Λk-term[E] has no
pluggable sub-terms. 

@(define-syntax-rule (Λkp-term t)
   (render-lw Λdk/red (to-lw t)))

Note, however, that the embedded contexts themselves remain pluggable by
reduction rules and meta-functions that later pick apart the result term. For
example, consider the rule for yet another unusual control operator:
@(centered
  (parameterize ([rule-pict-style 'horizontal])
    (render-reduction-relation cont-pair-red)))
This rule calls @Λkp-term[v] with a pair of continuation values. The term 
denoting this pair is not itself pluggable, but the embedded contexts can
be plugged by subsequent reduction steps, after they are extracted by the
reduction rules for projecting @Λkp-term[tuple] components.

In addition to these contrived reduction rules, the semantics in 
@figure-ref{fig:reduction} supports all of the systems 
in @secref{sec:examples}.
