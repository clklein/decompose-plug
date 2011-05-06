#lang scribble/base
@(require scribble/manual)


@title{Implementation}
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
