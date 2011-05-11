#lang scribble/base
@(require scribble/manual
          redex/reduction-semantics
          redex/pict
          "citations.rkt")

@(define-language ex1
   (C (v C) (C e))
   (e v (e e))
   (v (λ (x) e) (cont C)))

@(define-language ex2
   (C (in-hole C (f hole)) hole))

@title{Introduction}

The dominant style of operational semantics technique in use today
has at its heart the notion of a context that controls 
where evaluation occurs. These contexts allow the designer
of a reduction semantics to factor the definition of 
a calculus into one part that specifies that atomic steps
of computation and a second part that controls where these
steps may occur. This factoring enables concise specification that, e.g, 
a language is call-by-value (or call-by-name or call-by-need@~cite[cbn-calculus]),
@racket[if] expressions must evaluate 
the test position before the branches, and even the behavior of 
exceptions, continuations, and state@~cite[felleisen-hieb],
all without cluttering the rules that describe the atomic steps
of computation.

Unfortunately, the precise meaning of context decomposition has not
been nailed down in the literature. Although an intuitive definition
is easy to understand from a few examples, this intuition does not
cover the full power of contexts. For example, what is the precise
language matched by this definition (note that values and contexts are mututally referential)?
@centered{@(render-language ex1)}
or this bizarre, small one?
@centered{@(render-language ex2)}
To remedy this lack, we have developed
a semantics for matching that supports contexts in their full glory
that is able to explain both of these examples, as well as match the
intuitive meaning and countless existing research papers.

Our motivation for studying context matching is its implementation
in the domain-specific
programming language Redex@~cite[rta2004-mfff redex]. Redex is designed to support
the semantics engineer with a lightweight toolset for
operational semantics and related formalisms. 
Specifically, Redex supports rapid
prototyping of context-sensitive operational semantics,
random testing, automatic typesetting, and, via its embedding
in Racket, access to a large palette of standard programming
tools. Redex is widely used, having supported several dozen research papers
as well as the latest Scheme standard@~cite[R6RS] and a number of larger
models, including a model of the Racket virtual machine.

The remainder of this paper builds up an intuitive understanding of 
what contexts are and how they are used via a series of examples, gives a semantics
for Redex's rewriting system, and discusses an algorithm to implement
the semantics.
