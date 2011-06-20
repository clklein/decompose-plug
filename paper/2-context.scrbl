#lang scribble/base

@(require redex/pict
          redex/reduction-semantics
          (only-in scribble/core table paragraph style element)
          (only-in slideshow/pict vl-append)
          "../2-models/models.rkt"
          "../2-models/util.rkt"
          "wfigure.rkt"
          "citations.rkt")


@title[#:tag "sec:examples"]{Matching and Contexts}

@wfigure["fig:arith" "Arithmetic Expressions"]{
@(render-language arith)

@paragraph[(style "vspace" '()) '(".1in")]

@(render-reduction-relation arith-red)
}

This section introduces the notion of a context and explains, through a series of examples, how pattern matching for contexts works.
Each example model comes with a lesson that informs the design of our context-sensitive reduction semantics semantics.

In its essence, a pattern of the form @rr[(in-hole C e)] matches an expression when the expression can be split into two parts,
an outer part (the context) that matches @rr[C] and an inner part that matches @rr[e]. The outer part also marks where the inner
part appears with a hole, written @rr[hole]. In other words, if you think of an expression as a tree, matching against
@rr[(in-hole C e)] finds some subtree of the expression that matches @rr[e], and then replaces that subterm with the hole
to build a new expression in such a way that that new expression matches @rr[C].

@wfigure["fig:ex" "Example Decomposition"]{
@centered{
@table[(style #f '())
       (list (list @paragraph[(style #f '()) @list{@rr[C] = @rr[hole]}]
                   @paragraph[(style "hspace" '())]{.1in}
                   @paragraph[(style #f '()) @list{@rr[a] = @rr[(+ 1 2)]}])
             (list @paragraph[(style #f '()) @list{@rr[C] = @rr[(+ hole 2)]}]
                   @paragraph[(style "hspace" '())]{.1in}
                   @paragraph[(style #f '()) @list{@rr[a] = @rr[1]}])
             (list @paragraph[(style #f '()) @list{@rr[C] = @rr[(+ 1 hole)]}]
                   @paragraph[(style "hspace" '())]{.1in}
                   @paragraph[(style #f '()) @list{@rr[a] = @rr[2]}]))]}
}
To get warmed up, consider @figure-ref["fig:arith"]. In this language @rr[a] matches addition expressions and @rr[C] matches
contexts for addition expressions. More precisely, @rr[C] matches an addition expression that has exactly one hole.
For example, the expression @rr[(+ 1 2)] matches  @rr[(in-hole C a)] three ways, as shown in @figure-ref["fig:ex"].
Accordingly, the reduction relation given in @figure-ref["fig:arith"] reduces addition expressions wherever they appear
in an expression, reducing @rr[(+ (+ 1 2) (+ 3 4))] to two different expressions, @rr[(+ 3 (+ 3 4))] and @rr[(+ (+ 1 2) 7)].
This example tells us that our context matching semantics must support multiple decompositions for
any given term.

@wfigure["fig:lc" "λ-calculus"]{
@(render-language Λ/red #:nts (remove* '(x y) (language-nts Λ/red)))
}
A common use of contexts is to restrict the places where a reduction may occur in order to model 
a realistic programming language's order of evaluation. 
@Figure-ref["fig:lc"] 
gives a definition of @rr[E] that enforces left-to-right order of evaluation. 
For example, consider this nested set of function calls, 
@rr[((f x) (g y))],
where the result of @rr[(g y)] is passed to the result of @rr[(f x)].
It decomposes into the context
@rr[(hole (g y))]
allowing
evaluation in the first position of the
application,
but not this context
@rr[((f x) hole)].
The second context is not allowed
because the grammar for @rr[E]
allows the hole to appear
in the argument position of
an application expression only when the function
position is already a value. Accordingly,
the reduction system insists that the call
to @rr[f] happens before the call to @rr[g].
This example tells us that our semantics for decomposition
must be able to support multiple different ways to 
decompose each expression form, depending on the subexpressions of
that form (application expressions
in this case).

Contexts can also be used in clever ways to model the call-by-need λ-calculus.
Like call-by-name, call-by-need evaluates the argument to a function only if
the value is actually needed by the body. 
Unlike call-by-name, however, each 
function argument is evaluated at most once. 
A typical implementation of
a language with call-by-need uses state to track if an argument has been evaluated,
but it is also possible to give a direct explanation, expoiting contexts to control where evaluation occurs.

@wfigure[#:size 2 "fig:cbn" "Call-by-need Contexts"]{
@(render-language Λneed/red #:nts '(E))
}
@Figure-ref["fig:cbn"] shows the contexts from @citet[cbn-calculus]'s model of call-by-need.
The first three productions of @rr[E] are standard, allowing evaluation wherever @rr[E]
may be, in the argument of the @rr[|1+|] primitive, 
as well as in the function position of an application, regardless of what
appears in the argument position. The fourth case allows evaluation in the body of
a lambda expression that is in the function position of an application. Intuitively,
this case says that once we have determined the function to be applied, then
we can begin to evaluate its body. Of course, the function is eventually going to
need its argument and this is where the final production comes in. This production
is the most interesting. It says that when a function in the function position
of some application needs its argument, then you may evaluate its argument.

As an example, this expression
@rr[((λ (x) (|+1| 1)) (|+1| 2))]
reduces by simplifying the body of the
@rr[λ] expression to @rr[2],
without reducing the argument because 
it decomposes into this context
@rr[((λ (x) hole) (|+1| 2))]
using the third production of @rr[E].
In contrast,
@rr[((λ (x) (|+1| x)) (|+1| 2))]
reduces to
@rr[((λ (x) (|+1| x)) 3)]
because the body of of the @rr[λ] expression decomposes into
the context @rr[(|+1| hole)] with its parameter in
the hole, and thus the entire expression decomposes
into the context
@rr[((λ (x) (|+1| x)) hole)].
This use of contexts tell us that our semantics must be
able to support a sophisticated form of nesting, namely that
a decomposition must occur in one part of a term in order
for a decomposition to occur in another.

@wfigure[#:size 2.5 "fig:cont" "Continuations"]{
@(render-language Λk/red)

@(parameterize ([rule-pict-style 'horizontal])
   (render-reduction-relation cont-red))
}
When building a model for continuations, there is an easy connection to make, namely that
an evaluation context is itself a natural representation for a continuation. That is,
at the point that when a continuation is grabbed, the context in which it is grabbed
is the continuation. @Figure-ref["fig:cont"] contains an extension of the 
left-to-right call-by-value model in @figure-ref["fig:lc"], that adds in support
for continuations.
It adds @rr[call/cc], the operator that grabs a continuation, and the new value form
@rr[(cont E)] that represents a continuation. For example, the next reduction step
for this expression
@rr[(|+1| (call/cc (λ (k) (k 2))))] 
is to grab a continuation. In this model that continuation is represented as
@rr[(cont (|+1| hole))], 
which is then applied to @rr[call/cc]'s argument
in the original context, yielding the expression 
@rr[(|+1| ((λ (k) (k 2)) (cont (|+1| hole))))].
The next step is to substitute for @rr[k], 
which yields this expression
@rr[(|+1| ((cont (|+1| hole)) 2))].
This expression has a continuation value in the function
position of an application, and the next step is to
invoke the continuation. So, we can simply replace the context
of the continuation invocation with the context inside the continuation,
plugging the argument passed to the continuation in the hole:
@rr[(|+1| 2)].
This reduction system tells us that our context decomposition
semantics must be able to support contexts that appear in a
term that play no part in any decomposition (and yet must still
match).

@(require (only-in slideshow/pict blank))

@wfigure["fig:delim" "Delimited Continuations"]{
@(render-language Λdk/red)

@paragraph[(style "vspace" '()) '(".1in")]

@(vl-append
  (parameterize ([render-reduction-relation-rules '(0)])
    (render-reduction-relation delim-red))
  (blank 0 2)
  (parameterize ([render-reduction-relation-rules '(1)]
                 [rule-pict-style 'horizontal])
    (render-reduction-relation delim-red))
  (parameterize ([render-reduction-relation-rules '(2)]
                 [rule-pict-style 'horizontal])
    (render-reduction-relation delim-red)))
}
Generalizing from ordinary continuations to delimited 
continuations is simply a matter of factoring the contexts
into two parts, those that contain a prompt and those that
do not. @Figure-ref["fig:lc"] shows one way to do this, as
an extension of @figure-ref["fig:cont"]. The non-terminal @rr[E]
matches an arbitrary context and @rr[M] matches a context
that does not contain any prompt expressions. Accordingly,
the rule for grabbing a continuation exploits this factoring
to record only the portion of the context between
@rr[call/comp] and the nearest enclosing prompt in a continuation.

@wfigure["fig:wacky" "Wacky Context"]{
@(vl-append 
  4
  (render-language wacky)
  (render-language wacky-inside-out))
}
The interesting aspect of this system is how @rr[E] refers to @rr[M] 
and how that makes it difficult to support an algorithm
that matches @rr[E]. For all of the example systems so far in
this section, a matching algorithm can match a pattern of the
form @rr[(in-hole C e)] by attempting to match @rr[C] against
the entire term and, once a match has been found, attempting to 
match what appeared at the hole against @rr[e]. With @rr[E], however,
this leads to an infinite loop because @rr[E] expands to a
decomposition that includes @rr[E] in the first position.

A simple fix that works for the delimited continuations 
example is to treat such
cycles as a failure to match, but that fix does not work
for the first definition of @rr[C] given in @figure-ref["fig:wacky"]. 
Specifically, @rr[C] would match nothing with an algorithm
that treated those cycles as a failure to match, but the
context @rr[(f hole)] should match @rr[C] and, more generally,
the two definitions of @rr[C] in @figure-ref["fig:wacky"]
should be equivalent.
(A more complex version of this context came up when one of our Redex
users was developing an extension to the call-by-need model. 
We do not yet have a useful model that includes this peculiarity,
but do expect that it represents the essence of a useful model.)
