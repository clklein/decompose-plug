#lang scribble/manual

@require[scribble/core]

@(element (style "showtitle" '()) "")
@(element (style "showabstract" '()) "")

@section{Problem Setup}
@itemlist[
@item{The semantics of context-sensitive reduction semantics is straightforward,
until you permit holes in terms themselves (e.g., terms denoting reified 
continuations).}
@item{Related work: Felleisen, Danvy, and that rewriting guy who thinks he
invented context-sensitive rewriting. Do their definitions cover terms with 
holes? Do they define plug and decompose on a language-by-language basis?}
@item{Current Redex solution: hide-hole, the-hole, and the-not-hole.}
]

@section{An Introduction to Context-Sensitive Reduction Semantics and Redex}
@itemlist[
@item{Similar to Redex tutorial in ICFP submission but written for readers who
don't know evaluation contexts.}
]

@section{The Semantics}
@itemlist[
@item{Non-syntax-directed matching rules}
@item{Examples: (cont E) ⊆ v? E ⊆ v?}
@item{Definition of reduction: t -> t’ by rule p -> p’ if t matches p with 
bindings b, t’ matches p’ with bindings b’, and b’ ⊆ b. (Assumes that bindings
do not distinguish active holes.)}
@item{In E ⊆ v language, ([] ((λ (a) a) (λ (b) b))) -> ((λ (b) b) []). If
you want to support this bad definition, need to distinguish the active
holes in bound pattern variables and plug accordingly.}
]

@section{Implementation}
@itemlist[
@item{Two forms of non-determinism in previous section. (1) Is the hole in the
head or tail of the cons? (2) How many times should we unroll non-terminal
definitions?}
@item{Syntax-directed matching rules}
@item{Jay's fixed-point idea.}
@item{@racketblock[(define-language one-loop
                     (L hole
                        (in-hole (L e) (λ x hole)))
                     (A hole
                        (in-hole L A)))]}
@item{@racketblock[(define-language another-loop
                     (code:comment "hole, (hole), ((hole)), etc. ∈ W")
                     (W hole
                        (in-hole W (hole))))]}]

@section{How Redex Should Change}