#lang scribble/base

@(require scriblib/figure
          redex/pict
          redex/reduction-semantics
          "kont-model/model.rkt"
          "kont-model/util.rkt")

@(define-language unfortunate-loop
   (L hole
      (in-hole (L e) (λ x hole)))
   (A hole
      (in-hole L A)))

@title{Matching and Contexts}

@figure["fig:cont" "Continuation Language"]{
  @(render-language Λk #:nts (remove 'x (language-nts Λk)))
}

@Figure-ref["fig:cont"] shows the grammar of the language we
are considering. It contains application expressions,
variables, λ expressions, @rr[call/cc], the addition operator
and numbers. 

@figure["fig:contred" "Continuation Reduction"]{
  @(render-language Λk/red)
  @(render-reduction-relation red)
}

@(render-language unfortunate-loop)

