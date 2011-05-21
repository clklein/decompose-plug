#lang scribble/base
@(require "citations.rkt")
@title{Related Work}

Contexts date back at least to @citet[berendrecht], who used them to define the compatible closure
of a reduction relation. @citet[felleisen-hieb] exploited the power of a selective
notion of context to give equational semantics for many aspects of programming languages,
notably continuations and state. 

Later, @citet[context-sensitive-rewriting-fundamentals]
explored contexts from a rewriting perspective, much like we do in the present work. 
Lucas's definitions, however, do not even accomodate the first context-sensitive
rewriting system in @secref["sec:examples"], because his formalism limits each
function symbol to a single place where rewriting can occur. The followup work in the
rewriting community seems to have focused on tools for proving termination of
such rewriting systems, something that is generally of limited interest when studying a rewriting
systems that are designed to model programming languages (as programming 
languages are typically known not to terminate).

(Does Maude@~cite[maude2] support context sensitive rewriting?)