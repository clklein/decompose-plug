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
grammars but by specifying, for each function symbol, which sub-term positions
may be reduced. Because the specification depends only on the function symbol's
identity (i.e., and not on its sub-terms), this formulation cannot express
common evaluation strategies, such as left-to-right, call-by-value evaluation. Follow-up
work on this form of context-sensitive rewriting focuses on tools for proving
termination, generally a topic of limited interest when studying reduction 
systems designed to model a programming language (as programming languages are typically
known not to terminate).

As part of their work on SL, a meta-language similar to Redex, @citet[xiao-hosc01]
define a semantics for Felleisen-Hieb contexts by translating grammars to finite 
tree automata. This indirect approach allows SL to prove decomposition lemmas 
automatically using existing automata algorithms, but it is considerably more
complicated than our approach and does not allow for multi-hole contexts.

@citet[dubois-tphols00] develops the first formulation of a 
Felleisen-Hieb reduction semantics in a proof assistant, as part of a mechanized
proof of the soundness of ML's type system. Her formulation encodes single-hole 
contexts as meta-level term-to-term functions (restricted to coincide with the 
usual grammar defining call-by-value evaluation) and therefore models plug as 
meta-application. The formulation does not use an explicit notion of 
decomposition; instead, the contextual closure reduction rule applies to terms
that may be formed using the plug operation.

Berghofer's, Leroy's, and Xi's solutions to the POPLmark Challenge@~cite[POPLmark] use 
Dubois's encoding for the challenge's reduction semantics. Vouillon's solution uses a 
first-order encoding of contexts and therefore provides an explicit definition
of plugging. (The other submitted solutions use structural operational 
semantics, do not address dynamic semantics at all, or are no longer available 
online.)

@citet[refocusing] and 
@citet[refocusing-formalized] provide an axiomatization of the various 
components of a Felleisen-Hieb reduction semantics, 
such as a decomposition relation, that together define the
semantics. This axiomatization is not an appropriate basis for
Redex for two reasons. First, it requires users to specify plugging and decomposition 
explicitly. Common practice leaves these definitions implicit, and one of our 
design goals for Redex is to support conventional definitions. 
Second, the axiomatization requires decomposition to be a (single-valued) 
function, ruling out the semantics in @figure-ref{fig:arith} and, more 
problematically, reduction semantics for multi-threaded programs and programs in
languages like C and Scheme, which do not specify an order for application 
expressions.

More broadly speaking, there are 
hundreds@note{There are more than 400 citations to the original Felleisen-Hieb paper;
              while evaluation-context based semantics are still widely used, the paper
              is now rarely cited as it has become a standard part of the programming
              languages landscape.}
of papers that use evaluation context
semantics to model programming languages for just as many different
purposes. Although we have not implemented anywhere near all of them in Redex, we have sought out
interesting and non-standard ones over the years to try them out and to build our intuition
about how a semantics should behave.
