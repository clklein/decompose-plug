#lang scribble/base

@(require scriblib/figure
          redex/pict
          "typesetting-setup.rkt"
          "../non-syntax-directed-match-define-relation.rkt")

@title{A Semantics for Matching}

@figure["matching" "Matching and Decomposition"]{
  @centered{
    @(with-rewriters (render-relation matches))
    @(with-rewriters (render-relation decomposes))
  }
}
