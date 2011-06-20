#lang scribble/lncs

@(require "citations.rkt"
          scribble/core)

@authors[@author[#:inst "1"]{Casey Klein}
         @author[#:inst "2"]{Jay McCarthy}
         @author[#:inst "1"]{Steven Jaconette}
         @author[#:inst "1"]{Robert Bruce Findler}]
@institutes[@institute{Northwestern University}
             @institute{Brigham Young University}]
@title{A Semantics for Context-Sensitive Reduction Semantics}

@abstract{
This paper explores the semantics of the meta-notation used in the
style of operational semantics introduced by Felleisen and Hieb.
Specifically, it defines a formal system that gives precise meanings
to the notions of contexts, decomposition, and plugging
(recomposition) left implicit in most expositions. This semantics is
not naturally algorithmic, so the paper also provides an algorithm and
proves a correspondence with the declarative definition.

The motivation for this investigation is PLT Redex, a domain-specific
programming language designed to support Felleisen-Hieb-style
semantics. This style of semantics is the de-facto standard in operational
semantics and, as such, is widely used.
Accordingly, our goal is that Redex programs should, as much as possible,
look and behave like those semantics. Over the five years
since Redex's first public release, the precise interpretation of
contexts has changed several times, as we repeatedly encountered
reduction systems that did not behave according to their authors'
intent. This work describes the result of that experience.
To the best of our knowledge, the semantics given here matches even
the most complex uses of Felleisen-Hieb-style semantics available.
}

@include-section["1-intro.scrbl"]
@include-section["2-context.scrbl"]
@include-section["3-semantics.scrbl"]
@include-section["4-algorithm.scrbl"]
@include-section["5-reduction.scrbl"]
@include-section["6-related.scrbl"]

@section[#:style (style #f '(hidden unnumbered))]{}
@(element (style "paragraph" '()) '("Acknowledgments"))
Thanks to Stephen Chang for his many 
interesting examples of contexts that challenged our
understanding of evaluation context semantics.
Thanks also to Matthias Felleisen and Matthew Flatt for
helpful discussions on the work.

@(generate-bibliography)

@section[#:tag "sec:proof"]{Appendix}
@element[(style "input" '(exact-chars)) '("core-algorithm-correctness-txt.tex")]