#lang scribble/lncs

@(require scribble/core "citations.rkt")

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
@section{How Redex Should Change}

@section{Related Work}

@~cite[context-sensitive-rewriting-fundamentals]

@(generate-bibliography)
