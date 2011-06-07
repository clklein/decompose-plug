#lang scribble/base
@(require scribble/manual
          scribble/core
          scriblib/figure
          redex/pict
          "../2-models/models.rkt"
          "typeset-match-rules.rkt"
          "typeset-match-algo.rkt")

@title{An Algorithm for Matching}

The rules in @figure-ref{fig:matching} provide a declarative definition of
context-sensitive matching, but they do not lead directly to an efficient
matching algorithm. There are two problems. First, as reflected in the two 
@pt[:cons] decomposition rules, an algorithm cannot know a priori whether 
to match on the left and decompose on the right or to decompose on the left and
match on the right. An implementation that tries both possibilities scales 
exponentially in the number of nested @pt[:cons] patterns (counting indirect 
nesting through non-terminals). Second, the rules provide no answer to the 
question of whether to proceed in expanding a non-terminal if none of the input 
term has been consumed since last encountering that non-terminal. This question
arises, for example, when decomposing by the pattern @pt[(:nt M)] from the 
grammar in @figure-ref{fig:delim}, since @pt[M]'s second production causes the 
@pt[:in-hole] rule to decompose the same term by the pattern @pt[(:nt M)]. This
second problem is the manifestation of left recursion in the form of grammars
we consider.

@figure["fig:core-algo" "Core matching algorithm. Cases apply in order."]{
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
not @mt[:hole], then only decomposition is possible. Similarly, the third case
handles all other atomic patterns by producing a match result only if the given
term is identical to the atom.

The (meta) context in which a call to @mt[M] appears may eventually discard some 
or all of the results it receives. For example, consider the fourth clause,
which handles @mt[:cons] patterns. If the term is also a pair, the case makes
two recursive and examines the cross product of the results. For each result
pair, the case merges their bindings and checks that the results are not both 
decompositions. If neither is a decomposition, the case combines the pair into
a match result; if exactly one is a decomposition, it extends the decomposition
with the term matched by the non-decomposition.

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

Putting aside the second problem described above, the call @mt[(M G p t)]
computes the set of @mt[b] such that @matches-schema/unframed or 
@decomposes-schema/unframed for some @mt[C] and @mt[t_^′], and the top-level 
wrapper function @mt[matches] restricts this set to the bindings associated with
match derivations. More precisely, the following result holds.

Definition (Left Recursion)

Theorem (Correctness for Grammars without Left Recursion)

The restriction on @mt[G] can be lifted by ...