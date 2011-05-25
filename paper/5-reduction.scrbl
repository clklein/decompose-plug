#lang scribble/base
@(require scribble/manual)

@title[#:tag "sec:reduction"]{A Semantics for Reduction}
@itemlist[
@item{One possibility: defined as the rewriting community would -- t → t’ by the
      rule p → p’ when t matches p with bindings b, t’ matches p’ with bindings 
      b’, and Dom(b) ⊆ Dom(b’).}
@item{Problem: how to implement @racket[unmatch : L p b -> (t ...)]?}
@item{Problem: when E ⊂ v, ([] ((λx.x) (λy.y))) → ((λy.y) []).}
@item{Problem: how to extend to meta-functions? Can't ignore them in matching and
      apply them in a second step due to right-hand sides like 
      (in-hole (f x_1) x_1), in which the meta-function's result is needed for
      matching.}
@item{Alternative: @racket[reduce], @racket[subst], and @racket[plug].}
@item{Doesn't handle some right-hand sides that the rejectes semantics one
      (e.g., (in-hole (cons E E) 7)), but the alternative seems sufficient
      for the rules people tend to write in practice.}]