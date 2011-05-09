#lang scribble/manual

@(require scribble/core "citations.rkt")

@(element (style "showtitle" '()) "")
@(element (style "showabstract" '()) "")

@include-section["1-intro.scrbl"]
@include-section["2-redex.scrbl"]
@include-section["3-semantics.scrbl"]
@include-section["4-algorithm.scrbl"]
@section{How Redex Should Change}

@section{Related Work}

@~cite[context-sensitive-rewriting-fundamentals]

@(generate-bibliography)
