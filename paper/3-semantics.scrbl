#lang scribble/base

@(require scriblib/figure
          scribble/manual
          scriblib/footnote
          redex/pict
          slideshow/pict
          "typeset-match-rules.rkt"
          "wfigure.rkt"
          "../sem-sem/patterns.rkt"
          "../sem-sem/non-syntax-directed-match-define-relation.rkt")

@title[#:tag "sec:match-rules"]{A Semantics for Matching}

@wfigure[#:size 1.7 "fig:pat-term" "Patterns and Terms"]{
@patterns-and-terms
}

This section formalizes the notion of matching used in the definitions of
the example reduction systems in @secref{sec:examples}. For ease of 
presentation, we stick to the core language of patterns and terms in
@figure-ref{fig:pat-term}. Redex supports a richer language of patterns
(notably including a notion of Kleene star), 
but this core captures an essence suitable for explaining the semantics
of matching.

Terms @pt[t] are binary trees that have atoms as leaves.
A contexts @pt[c] within a term carry the path to its hole in the form
of @pt[:left] and @pt[:right] annotations. For matching purposes, though,
the @pt[:left] and @pt[:right] constructors are equivalent to @pt[:cons]; 
the difference arises relevant only when plugging a context.
Atoms @pt[a] include literals such as numbers and symbols.

Patterns @pt[p] take one of six forms. 
Atomic patterns @pt[a] and the @pt[:hole] pattern match only themselves. A pattern 
@pt[(:name x p)] binds the pattern variable @pt[x] to the term matched by 
@pt[p]. Repeated pattern variables force the corresponding sub-terms to
be identical. A pattern @pt[(:nt n)] matches terms that match any of the 
productions of the non-terminal @pt[n] (defined outside the pattern). 
We write decomposition patterns @math{p_1[p_2]} using a separate keyword
for clarity: @pt[(:in-hole p_1 p_2)]. Finally, interior nodes are matched
by the pattern @pt[(:cons p_1 p_2)], with @pt[p_1] and @pt[p_2] matching the corresponding
sub-terms.

@(begin
   (require "../2-models/double.rkt"
            "../2-models/models.rkt")
   (define-syntax-rule (check-ok/pt e1 e2)
     (let ([converted (rp->p (lang-nts :arith) 'e1)]
           [qe2 'e2])
       (unless (equal? converted qe2)
         (error '3-semantics.scrbl 
                "example doesn't match:\n  ~s\n  ~s"
                converted
                qe2))
       @pt[e2])))

For example, the left-hand side of the reduction rule in 
@figure-ref{fig:arith} corresponds to the following pattern, 
where the literal @pt[empty] is used for the empty sequence and
the pattern @pt[:number] matches literal numbers:
@centered{@check-ok/pt[(in-hole C (+ number_1 number_2))
                       (:in-hole (:name C (:nt C))
                                 (:cons + 
                                        (:cons (:name number_1 :number)
                                               (:cons (:name number_2 :number)
                                                      empty))))]}

@figure["fig:matching" "Matching and Decomposition"]{
  @(centered combined-matching-rules)
}

@Figure-ref{fig:matching} gives a semantics for patterns via the judgment form
@|matches-schema/unframed|, which defines when the pattern @pt[p] matches the term
@pt[t]. The grammar @pt[G] is a finite map from non-terminals to sets of patterns. 
The bindings @pt[b] is a finite map from pattern variables to terms showing how the 
pattern variables of @pt[p] can be instantiated to yield @pt[t]. 
The @|matches-schema/unframed| judgment relies on an auxiliary
judgment @|decomposes-schema/unframed| that performs decompositions. Specifically,
it holds when @pt[t] can be decomposed into a context @pt[C] that matches @pt[p] 
and contains the sub-term @pt[t_^′] at its hole.

Many of the rules for these two judgment forms rely on the operator 
@pt[⊔]. It combines two mappings into a single one by unioning their 
bindings, as long as the domains do not overlap. If the domains
do overlap then the corresponding ranges must be the same; otherwise 
@pt[⊔] is not defined. Accordingly, rules that uses @pt[⊔] apply only when
@pt[⊔] is well-defined.

The atom rule produces an empty binding map because a pattern @pt[a] contains no
pattern variables. The @pt[:name] rule matches @pt[p] with @pt[t] and produces a
map extended with the binding @pt[(pair x t)]. The @pt[nt] rule applies 
if any of the non-terminal's productions match. The scope of a production's 
pattern variables is limited to that production, and consequently, the 
@pt[nt] rule produces an empty binding map. The @pt[:cons] rule matches the 
sub-terms and combines the resulting sets of bindings. The @pt[:in-hole] rule uses
the decomposition judgment form to find a decomposition and then checks to make 
sure that the term in the hole matches @pt[p_2].

The rules for the @|decomposes-schema/unframed| form are also organized
around the pattern in question.
The @pt[:hole] decomposition rule decomposes any term @pt[t] into the empty context
and @pt[t] itself. The first of two @pt[:cons] decomposition rules applies
when a decomposition's focus may be placed within a pair's left sub-term.
This decomposition highlights the same sub-term @pt[t_1^′] as the
decomposition of @pt[t_1] does, but places it within the larger context 
@pt[(:left C t_2)]. The second of the @pt[:cons] decomposition rules does the
same for the pair's right sub-term. The @pt[:nt] decomposition rule propagates
decompositions but, as in the corresponding matching rule, ignores binding maps.

The @pt[:in-hole] decomposition rule performs a nested decomposition. Nested 
decomposition occurs, for example, when decomposing according to 
call-by-need evaluation contexts (see the last production in @figure-ref{fig:cbn}).
The @pt[:in-hole] rule decomposes @pt[t] into
a composed context @pt[(append-contexts C_1 C_2)] and a sub-term @pt[t_^′], 
where @pt[p_1] and @pt[p_2] match @pt[C_1] and @pt[C_2] respectively. 
The definition of context composition (@figure-ref{fig:matching}, bottom-right) follows
the path in @pt[C_1]. The @pt[:name] decomposition rule is similar to the 
corresponding matching rule, but it introduces a binding to the context
that is matched, not the entire term.
