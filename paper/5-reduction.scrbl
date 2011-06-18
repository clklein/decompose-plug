#lang scribble/base
@(require scribble/manual
          scriblib/figure
          scriblib/footnote
          scribble/core 
          scribble/decode
          redex/pict
          redex/reduction-semantics
          (only-in "../2-models/util.rkt" rr)
          "../2-models/models.rkt"
          "../sem-sem/patterns.rkt"
          "typeset-match-rules.rkt"
          "typeset-reduction.rkt")

@(define-syntax-rule (big-footnote . args)
   (nested-flow (style "footnote" '(command never-indents)) 
                (decode-flow (list . args))))

@title[#:tag "sec:reduction"]{A Semantics for Reduction}

We now put the notion of matching from @secref{sec:match-rules} to work in a
formalization of the standard notation for context-sensitive reduction rules. 
As with patterns, we consider a core specification language that lacks
many of the conveniences of a language like Redex but nevertheless addresses the
principal ideas.

@figure["fig:reduction" "A semantics for reduction"]{@(render-reduction)}