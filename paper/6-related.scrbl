#lang scribble/base
@(require scribble/manual
          scriblib/footnote
          scriblib/figure
          "citations.rkt")
@title{Related Work}

@citet[berendregt] makes frequent use of a notion of contexts specialized to 
@math{λ}-terms. Like ours, these contexts may contain multiple holes, but
plug's behavior differs in that it fills all of the context's holes. 
@citet[felleisen-hieb] exploited the power of a selective notion of context 
to give equational semantics for many aspects of programming languages, 
notably continuations and state. The meaning of multi-holed contexts does
not arise in their work, since the grammar for contexts restricts them
to exactly one hole.

@citet[context-sensitive-rewriting-fundamentals] later explored an alternative
formulation of selective contexts. This formulation defines contexts not by
grammars but by specifying for each function symbol which sub-term positions
may be reduced. Because the specification depends only on the function symbol's
identity (i.e., and not on the sibling sub-terms), this formulation cannot express
common evaluation strategies, such as call-by-value and call-by-name. Follow-up
work on this form of context-sensitive rewriting focuses on tools for proving
termination, generally a topic of limited interest when studying reduction 
systems designed to model programming languages (which tend to allow 
non-termination).

As part of their work on SL, a meta-language similar to Redex, @citet[xiao-hosc01]
define a semantics for Felleisen-Hieb contexts by translating grammars to finite 
tree automata. This indirect approach allows SL to prove decomposition lemmas 
automatically using existing automata algorithms, but it is considerably more
complicated our approach and does not allow for multi-hole contexts.

@citet[dubois-tphols00] develops what may be the first formulation of a 
Felleisen-Hieb reduction semantics in a proof assistant, as part of a mechanized
proof of the soundness of ML's type system. Her formulation encodes single-hole 
contexts as meta-level term-to-term functions (restricted to coincide with the 
usual grammar defining call-by-value evaluation) and therefore models plug as 
meta-application. The formulation does not use an explicit notion of 
decomposition; instead, the contextual closure reduction rule applies to term 
@math{t} when plugging context @math{C} with term @math{t'} yields @math{t}. 
Three of the solutions submitted to the POPLmark Challenge@~cite[POPLmark] use 
this same encoding for the challenge's reduction semantics. A fourth uses a 
first-order encoding of contexts and therefore provides an explicit definition
of plugging.@note{The other submitted solutions use structural operational 
semantics, do not address dynamic semantics at all, or are no longer available 
online.}

In pursuit of an efficient implementation technique, @citet[refocusing] and 
@citet[refocusing-formalized] provide an axiomatization of the various 
components, such as a decomposition relation, that together define a reduction
semantics. For two reasons, this axiomatization is not an appropriate basis for
Redex. First, it requires users to specify plugging and decomposition 
explicitly. Common practice leaves these definitions implicit, and one of our 
design goals for Redex is to support conventional definitions with few changes. 
Second, the axiomatization requires decomposition to be a (single-valued) 
function, ruling out the semantics in @figure-ref{fig:arith}, and more 
problematically, reduction semantics for multi-threaded programs.