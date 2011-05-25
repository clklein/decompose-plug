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

@include-section["1-intro.scrbl"]
@include-section["2-context.scrbl"]
@include-section["3-semantics.scrbl"]
@include-section["4-algorithm.scrbl"]
@include-section["5-reduction.scrbl"]
@include-section["6-related.scrbl"]

@(generate-bibliography)

@(element (style "paragraph" '()) '("Acknowledgments"))
Thanks to Stephen Chang for his many 
interesting examples of contexts that challenged our
understanding of evaluation context semantics.

