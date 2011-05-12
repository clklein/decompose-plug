#lang scribble/base
@(require scribble/manual)

@title{A Semantics for Reduction}
@itemlist[
@item{One possibility: defined as the rewriting community would -- t → t’ by the
      rule p → p’ when t matches p with bindings b, t’ matches p’ with bindings 
      b’, and Dom(b) ⊆ Dom(b’).}
@item{Problem: how to implement @racket[unmatch : L p b -> (t ...)]?}
@item{Problem: when E ⊂ v, ([] ((λx.x) (λy.y))) → ((λy.y) []).}
@item{Alternative: @racket[reduce], @racket[subst], and @racket[plug].}
@item{Claim: seems to be equivalent for reduction systems people actually write.}]