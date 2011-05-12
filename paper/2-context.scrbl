#lang scribble/base

@(require scriblib/figure
          redex/pict
          redex/reduction-semantics
          "kont-model/model.rkt"
          "kont-model/util.rkt")

@title{Matching and Contexts}

arithmetic:

@(render-language arith)

@(render-language arith/red)

@(render-reduction-relation arith-red)

cbv LC:

@(render-language Λ #:nts (remove* '(x y) (language-nts Λ)))

@(render-language Λ/red)

@(render-reduction-relation cbv-red)

cbn LC:

@(render-language Λneed/red #:nts '(E))

@(parameterize ([render-reduction-relation-rules '("deref")])
   (render-reduction-relation cbn-red))

continuations:

@(render-language Λk/red)

@(render-reduction-relation cont-red)

delimited control (coming):

bizarro thing:

@(define-language ex2
   (C (in-hole C (f hole)) hole))
@(render-language ex2)

