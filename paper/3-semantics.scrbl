#lang scribble/base

@(require scriblib/figure
          scribble/manual
          redex/pict
          slideshow/pict
          "typeset-match-rules.rkt"
          "wfigure.rkt"
          "../sem-sem/patterns.rkt"
          "../sem-sem/non-syntax-directed-match-define-relation.rkt")

@(define-syntax-rule (pt t) ; "pattern term"
   (with-rewriters (lw->pict patterns (to-lw t))))

@title{A Semantics for Matching}
This section formalizes the intuition that a term @math{t} decomposes into 
@math{C[e]} when @math{t} can be partitioned into two smaller terms @math{t'} 
and @math{t''} such that @math{t'} match the pattern @math{C} and @math{t''} 
match the pattern @math{e}. For ease of presentation, we stick to the core 
language of patterns and terms in @figure-ref{fig:pat-term}. Redex supports 
a richer language of patterns, but the present forms suffice to illustrate 
the essential concepts.

@figure["fig:pat-term" "Patterns and Terms"]{
@patterns-and-terms
}

Terms @pt[t] are binary trees with atoms as leaves. Atoms @pt[a] include 
literals such as numbers and symbols, as well as the distinguished atom 
@pt[:hole], used in decomposition. Patterns @pt[p] takes one of five forms. 
An atomic pattern @pt[a] matches the atom @pt[a]. Atoms A pattern 
@pt[(:name x p)] binds the pattern variable @pt[x] to the term matched by 
@pt[p], putting @pt[x] in scope of the right-hand side of the enclosing
reduction rule (@secref{sec:reduction}). If another @pt[:name] pattern also 
binds @pt[x], the two patterns are constrained to match identical terms. A 
pattern @pt[(:nt n)] matches terms that match any of the productions of the 
non-terminal @pt[n] (defined outside the pattern). A pattern 
@pt[(:in-hole p_1 p_2)], more conventionally written @math{p_1[p_2]}, 
performs a decomposition. Finally, a pattern @pt[(:cons p_1 p_2)] matches 
pairs. For example, the left-hand side of the @pt[[cont]] rule in 
@figure-ref{fig:cont} could be encoded as the following pattern: 
@centered{@pt[(:in-hole (:name E_1 (:nt E))
                        (:cons (:cons cont (:name E_2 (:nt E)))
                               (:nt v)))]}

@figure["matching" "Matching and Decomposition"]{
  @(centered combined-matching-rules)
}

@figure["binding-consistency" "Binding Consistency"]{
  @(centered binding-consistency)
}