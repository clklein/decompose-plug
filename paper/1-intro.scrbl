#lang scribble/base
@(require scribble/manual
          redex/reduction-semantics
          redex/pict
          slideshow/pict
          "citations.rkt")

@(define-language ex1
   (C hole (v C) (C e))
   (e v (e e))
   (v (λ (x) e) (cont C)))
@(define ex1-pat
   (render-lw ex1 (to-lw (in-hole C e))))
@(define-language ex2
   (C (in-hole C (f hole)) hole))

@(define raw1 (render-language ex1))
@(define raw2 (render-language ex2))
@(define bkg (blank (max (pict-width raw1) (pict-width raw2)) 0))

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
been nailed down in a way that captures its diverse usage in the literature. Although an intuitive definition
is easy to understand from a few examples, this intuition does not
cover the full power of contexts. For example, which terms match the
pattern @|ex1-pat| from this language (in which values are contexts
are mutually referential)?
@centered{@(lc-superimpose bkg raw1)}
And which terms match this bizarre, small language?
@centered{@(lc-superimpose bkg raw2)}
To remedy this lack, we have developed
a semantics for matching and reduction that not only supports these exotic languages but
also captures the intuitive meanings of countless existing research papers. 
This semantics does not assume explicit language-specific definitions of 
plugging and decomposition, since most expositions leave these concepts 
implicit.

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
models, including a model of the Racket virtual machine@~cite[racket-VM].

In keeping with the spirit of Redex, we augment the standard
proof-based validation techniques with testing. More concretely, in
addition to proving a correspondence between a specification of
context-sensitive matching and an algorithm for that specification, 
we have conducted extensive testing of the semantics, using a Redex
model of Redex (there is little danger of meta-circularity causing
problems, as the embedding uses a relatively modest subset of Redex's
functionality). This model allows us to test that our semantics gives
the intended meanings to interesting calculi from the literature.

The remainder of this paper builds up an intuitive understanding of 
what contexts are and how they are used via a series of examples, gives a semantics
for Redex's rewriting system, and discusses an algorithm to implement
the semantics.
