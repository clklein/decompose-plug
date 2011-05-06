#lang scribble/base

@(require scriblib/figure
          redex/pict
          "typesetting-setup.rkt"
          "../non-syntax-directed-match-define-relation.rkt")

@title{The Semantics}

@figure["matching" "Matching and Decomposition"]{
  @centered{
    @(with-rewriters (render-relation matches))
    @(with-rewriters (render-relation decomposes))
  }
}

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
