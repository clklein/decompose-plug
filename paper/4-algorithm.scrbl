#lang scribble/base
@(require scribble/manual
          scribble/core
          scriblib/figure
          redex/pict
          scriblib/footnote
          "../2-models/models.rkt"
          "../sem-sem/syntax-directed-match.rkt"
          "citations.rkt"
          "typeset-match-rules.rkt"
          "typeset-match-algo.rkt")

@title{An Algorithm for Matching}

The rules in @figure-ref{fig:matching} provide a declarative definition of
context-sensitive matching, but they do not lead directly to a tractable
matching algorithm. There are two problems. First, as reflected in the two 
@pt[:cons] decomposition rules, an algorithm cannot know a priori whether 
to match on the left and decompose on the right or to decompose on the left and
match on the right. An implementation that tries both possibilities scales 
exponentially in the number of nested @pt[:cons] patterns matched (counting indirect 
nesting through non-terminals). Second, the rules provide no answer to the 
question of whether to proceed in expanding a non-terminal if none of the input 
term has been consumed since last encountering that non-terminal. This question
arises, for example, when decomposing by the pattern @pt[(:nt E)] from the 
grammar in @figure-ref{fig:delim}, since @pt[E]'s second production causes the 
@pt[:in-hole] rule to decompose the same term by the pattern @pt[(:nt E)]. This
second problem is the manifestation of left recursion in the form of grammars
we consider.

@figure["fig:core-algo" "Core matching algorithm (cases apply in order)."]{
@(render-algorithm)
}

The first problem can be solved by matching and decomposing simultaneously.
Since these tasks differ only in their treatment of @pt[:hole] patterns,
much work can be saved by sharing intermediate results between the tasks.
@Figure-ref{fig:core-algo} demonstrates this approach with a function @mt[M]
that returns a set of pairs @mt[(pair d b)] representing possible ways to
match or decompose the input term. In a pair representing a match, @mt[d] is
the marker @mt[•]; in a pair representing a decomposition, @mt[d] is a pair
@mt[(pair C t)] such that the input term can be decomposed into a context @mt[C]
and a sub-term @mt[t] occurring in @mt[C]'s hole.

The first two @mt[M] cases handle the pattern @mt[:hole]. If the term in 
question is also @mt[:hole], then it may be considered either to match 
@mt[:hole] or to decompose into @mt[:hole] in the empty context. If the term is
not @mt[:hole], then only decomposition is possible. The third case
handles atomic patterns by producing a match result only if the given
term is identical to the atom.

The (meta) context in which a call to @mt[M] appears may eventually discard some 
or all of the results it receives. For example, consider the fourth clause,
which handles @mt[:cons] patterns. If the term is also a pair (constructed with
any of @pt[:cons], @pt[:left], or @pt[:right]), then this case makes
two recursive calls and examines the cross product of the results using the
@pt[select] helper function. For each result
pair, the case merges their bindings and checks that the results are not both 
decompositions. If neither is a decomposition, @pt[select] combines the pair into
a match result; if exactly one is a decomposition, it extends the decomposition
with the term matched by the non-decomposition. If both are decompositions, 
then the match fails.

The next case, for patterns @mt[(:in-hole p_c p_h)], recurs with @mt[p_c] and 
the input term, expecting to receive decompositions. For each one, it makes 
another recursive call, this time with @mt[p_h] and the sub-term in the 
decomposition's focus. Each of the latter call's results @mt[m] is combined with
the decomposition's context, yielding a match result if @mt[m] is a match and
a larger context if @mt[m] is a decomposition.

The remaining three cases are straightforward. The @mt[:name] case recurs on the
sub-pattern and extends the bindings of each of the results with either the 
matched term or the context carved out by the decomposition. The @mt[:nt] case
tries each production, discarding the binding component of each result. The
final case, a catch-all, applies when the pattern does not match or decompose
the input term.

Putting aside the problem of left recursion, the call @mt[(M G p t)]
computes the set of @mt[b] such that @matches-schema/unframed or 
@decomposes-schema/unframed for some @mt[C] and @mt[t_^′], and the top-level 
wrapper function @mt[matches] restricts this set to the bindings associated with
match derivations. More precisely, the following result holds (the complete proof
is given in @secref{sec:proof}).

@(element (style "leftrecurdef" '()) "")
@(element (style "correctnessthm" '()) "")

Parsing algorithms that support left recursive context-free grammars go back 
nearly fifty years@~cite[kuno-cacm65]. We refer the reader to
@citet[frost-iwpt07-sec3] for a summary. Some of these algorithms appear 
adaptable to our setting,@note{We realized the significance of this line of work to ours only recently; if
the PC knows this area, we would be grateful for any advice.} though we 
have implemented only one, an extension of the packrat parsing
algorithm@~cite[warth-pepm08]. This extension dynamically detects left recursion and 
treats the choice leading to it as a failure. If the other choices for the same 
portion of the input make any progress at all, the algorithm repeats the parse
attempt, in hopes that the entries added to the memo table during the failed
attempt will cause a second attempt to succeed. This process continues as long
as repeated attempts make additional progress.
Extending the algorithm in @figure-ref{fig:core-algo} with a similar iterative 
phase allows matching of terms from left recursive grammars, such 
the ones in @figure-ref{fig:delim} and @figure-ref{fig:wacky}. 

@;{|
@(define-syntax-rule (wt t) ; "wacky term"
   (render-lw wacky (to-lw t)))

The remainder of this section works through an example derivation, showing
how the algorithm can derive that @mt[C] matches the term @wt[(f (f hole))].
For readability, we use conventional notation for terms,
contexts, and patterns and omit @mt[M]'s unchanging first argument. 

The extended algorithm repeats the call @mt[(M (f (f hole)) C)] three times,
assuming it tries @wt[C]'s productions from left to right:

@itemlist[#:style 'ordered
@item{The @mt[:in-hole] production immediately leads to another call 
@mt[(M (f (f hole)) C)], which the extended algorithm considers a failure, since 
the same call is already in progress. Next, @mt[M] tries @wt[C]'s @wt[hole] 
production, which produces one result: the trivial decomposition 
@pt[(decomposes (f (f hole)) hole (f (f hole)) C)].
Since the first call made some progress---it computed one possible 
decomposition---the extended algorithm repeats the process.}
@item{The second call begins with the same curtailed left recursive call, now
taken to return the decomposition computed in the first attempt. The 
@mt[M] function's @mt[:in-hole] case continues with the call 
@mt[(M (f (f hole)) (f hole))], which computes the decomposition
@pt[(decomposes (f (f hole)) (f hole) (f hole) (f hole))].
The call to @mt[combine] composes this decomposition with the previous to
conclude that @pt[(decomposes (f (f hole)) (f hole) (f hole) C)]. Because
the second call discovered an additional @wt[C] decomposition, the extended 
algorithm repeats the process again.}
@item{In the third call, the result of the curtailed left recursive call 
includes the additional decomposition
@pt[(decomposes (f (f hole)) (f hole) (f hole) C)]. This result induces the call
@mt[(M (f hole) (f hole))], which computes the match 
@pt[(matches (f hole) (f hole))] (as well as the decomposition
@pt[(decomposes (f hole) (f hole) hole (f hole))]). This match leads @mt[M] to
conclude via @mt[combine] that @pt[(matches (f (f hole)) (in-hole C (f hole)))], 
and consequently that @pt[(matches (f (f hole)) C)]. The extended algorithm
repeats to process once more to conclude that no more matches or decompositions
by @wt[C] are possible.}
]
|}
