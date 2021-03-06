#lang scribble/lncs

@(require "citations.rkt"
          "extended.rkt"
          scriblib/footnote
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
look and behave like those semantics. Since Redex's first public release
more than seven years ago, its precise interpretation of
contexts has changed several times, as we repeatedly encountered
reduction systems that did not behave according to their authors'
intent. This paper describes the culimation of that experience.
To the best of our knowledge, the semantics given here accommodates even
the most complex uses of contexts available.
}

@include-section["1-intro.scrbl"]
@include-section["2-context.scrbl"]
@include-section["3-semantics.scrbl"]
@include-section["4-algorithm.scrbl"]
@include-section["5-reduction.scrbl"]
@include-section["6-related.scrbl"]

@section[#:tag "sec:acknowledgments" #:style (style #f '(hidden unnumbered))]{}
@(element (style "paragraph" '()) '("Acknowledgments"))
Thanks to Stephen Chang for his many 
interesting examples of contexts that challenged our
understanding of context-sensitive matching.
Thanks also to Matthias Felleisen and Matthew Flatt for
helpful discussions of the work.

@section[#:tag "sec:url" #:style (style #f '(hidden unnumbered))]{}
@(element (style "paragraph" '()) '(""))

A version of this paper can be found online at:
@centered{@url{http://www.eecs.northwestern.edu/~robby/plug/}}
That web page contains 
@(if extended-version?
     @list{a version of the paper with a complete proof of the theorem}
     @list{the final version of the paper as it appears in the proceedings})
and the Redex models for all of the figures in this paper.

@(generate-bibliography)

@(if extended-version?
     (list
      @section[#:tag "sec:pre-appendix" #:style (style #f '(hidden unnumbered))]{}
      @(element (style "newpage" '()) '())
      @section[#:tag "sec:proof"]{Appendix}
      @element[(style "input" '(exact-chars)) '("core-algorithm-correctness-txt.tex")])
     (void))