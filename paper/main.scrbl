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
This paper explores the semantics of the meta-notation used in the style of
operational semantics introduced by Felleisen and Hieb. Specifically, it defines
a formal system that gives precise meanings to the notions of contexts, 
decomposition, and plugging (or recomposition) left implicit in most 
expositions. This semantics is not naturally algorithmic, so the paper also 
provides an algorithm and proves a correspondence with the declarative 
definition.

The motivation for this investigation is PLT Redex, a domain-specific 
programming language designed to support Felleisen-Hieb-style definitions with
minimal changes from the conventional pencil-and-paper notation. Since Redex's
first release, its semantics for contexts as changed several times, as we 
repeatedly encountered reduction systems that did not behave according to their
authors' intent. The work describes the result of that experience.
}

@include-section["1-intro.scrbl"]
@include-section["2-context.scrbl"]
@include-section["3-semantics.scrbl"]
@include-section["4-algorithm.scrbl"]
@include-section["5-reduction.scrbl"]
@include-section["6-related.scrbl"]

@(generate-bibliography)

@section[#:style (style #f '(hidden unnumbered))]{}
@(element (style "paragraph" '()) '("Acknowledgments"))
Thanks to Stephen Chang for his many 
interesting examples of contexts that challenged our
understanding of evaluation context semantics.
Thanks also to Matthias Felleisen and Matthew Flatt for
helpful discussions on the work.

@section[#:tag "sec:proof"]{Appendix}
@element[(style "input" '(exact-chars)) '("core-algorithm-correctness-txt.tex")]