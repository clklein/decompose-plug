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

Finally, for some perspective, without our system the careful researcher typically
supplies both a one-off plugging function and decomposition function, as exemplified
by @citet[example-of-using-contexts-with-explicit-deompose-and-plug]'s work.

Other notes:
@itemlist[
@item{Barendregt (1st ed. 1981, 2nd ed. 1984) uses contexts but not to
restrict where reduction occurs. Contexts can have multiple holes;
plug fills all of them (p. 29). “Multiple numbered contexts” (p. 375)
associate names with each hole to allow different holes to be plugged
with different terms. It's not clear from his definition what plug
does when multiple holes have the same name. Felleisen (Expressive
Power of PLs 1990) also defines multiple number contexts (Definition
3.4); it's clear from his definition that plug fills all
occurrences of each named hole.}
@item{Felleisen (dissertation 1987) defines contexts as grammars and uses
them to restrict reduction. Contexts are restricted to a single hole
(p. 27). The reduction system reifies continuations into lambdas (p.
100), avoiding the ambiguity of plug/decompose wrt multi-holed
contexts. Felleisen and Hieb (Revised Report 1992) and Sitaram and
Felleisen (Reasoning with Continuations II 1990) also reify
continuations into lambdas.}
@item{Lucas (Fundamentals of Context-Sensitive Rewriting 1995) defines a
more restrictive notion of context-sensitivity which notably cannot
express call-by-value or call-by-name evaluation contexts.}
@item{Danvy (Refocusing 2004) says that the meaning of evaluation contexts
is given by a plug function, defined separately. Decomposition is also
defined in terms of this plug function. In another paper (An
Operational Foundation for Delimited Continuations in the CPS
Hierarchy 2005), he gives an independent definition of decomposition
(but it still inverts plug).}
@item{Millikin (blog comment: http://tinyurl.com/44egua8) says that decomposition 
should be given explicitly.}
@item{Dubois (http://portal.acm.org/citation.cfm?id=695035) encodes a reduction 
semantics in Coq, with evaluation contexts as meta-level functions from terms to 
terms (constrained to coincide with a standard grammar for single-hole contexts), 
plug as meta-level application, and decompose as the inverse of plug. She claims 
her encoding to be the first formal treatment of evaluation contexts.}
@item{The PoplMark Challenge involves a context-sensitive reduction
semantics. Three of the submitted solutions use Dubois' encoding.
Another uses a first-order representation of contexts, defines plug
explicitly, and takes decompose to be plug's inverse. (The rest of the
solutions use SOS, don't tackle parts of the challenge involving
dynamic semantics, or are no longer online.)}]