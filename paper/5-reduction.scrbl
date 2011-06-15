#lang scribble/base
@(require scribble/manual
          scriblib/figure
          scriblib/footnote
          scribble/core 
          scribble/decode
          redex/pict
          redex/reduction-semantics
          (only-in "../2-models/util.rkt" rr)
          "../2-models/models.rkt"
          "../sem-sem/patterns.rkt"
          "typeset-match-rules.rkt"
          "typeset-reduction.rkt")

@(define-syntax-rule (big-footnote . args)
   (nested-flow (style "footnote" '(command never-indents)) 
                (decode-flow (list . args))))

@title[#:tag "sec:reduction"]{A Semantics for Reduction}

We now put the notion of matching from @secref{sec:match-rules} to work in a
formalization of the standard notation for context-sensitive reduction rules. 
As with patterns, we consider a core specification language that lacks
many of the conveniences of a language like Redex but nevertheless addresses the
principal ideas.



@figure["fig:reduction" "Reduction"]{
@verbatim{
      r ::= a
          | (var x)            G ⊢ t : p | b   inst(G, r, b) = t′
          | (in-hole r r p)    ----------------------------------
          | (cons r r)                G ⊢ t / p → t′ / r
          | (app f r)
      f ∈ t → t

         -----------------        --------------------------
         inst(G, a, b) = a        inst(G, (var x), b) = b(x)

inst(G, r_1, b) = t_1   inst(G, r_2, b) = t_2   plug^G_p(t_1, t_2) = t
----------------------------------------------------------------------
            inst(G, (in-hole r_1 r_2 p), b) = t
            
          inst(G, r_1, b) = t_1   inst(G, r_2, b) = t_2
          ---------------------------------------------
          inst(G, (cons r_1 r_2), b) = (cons t_1 t_2)

                      inst(G, r, b) = t
               --------------------------------
               inst(G, (app f r), b) = f[| t |]

          G ⊢ t = C[t_2] : p | b   t_1 = uncontext[| C |]               
          -----------------------------------------------
                    plug^G_p(t_1, t_2) = t
}
}

@Figure-ref{fig:reduction} shows our definition@big-footnote{
The definition of reduction popular in the term rewriting
community provides some guidance here. In first-order term rewriting, a
reduction rule @math{ρ} is a pair @math{(l, r)} of usually non-ground terms.
A term @math{t_1} reduces to @math{t_2} by the rule @math{ρ} if there is a
substitution @math{σ} such that @math{σ(l) = t_1} and @math{σ(r) = t_2}.
The same idea can be expressed in our setting by replacing @math{l} and @math{r}
with patterns @pt[p_1] and @pt[p_2] and defining @pt[t_1] to reduce to @pt[t_2]
when @(with-rewriters (render-lw patterns (to-lw (matches G t_1 p_1 b_1)))),
@(with-rewriters (render-lw patterns (to-lw (matches G t_2 p_2 b_2)))), and
@pt[b_1] @math{⊓} @pt[b_2] @math{≠} @math{⊤}.

This definition is appealingly simple. It supports the language defined in
@figure-ref{fig:cont} without the need for an explicit definition of plugging.
For example, consider reduction of the following term:
@(centered (rr ((cont hole) (|+1| (call/cc (λ (k) k))))))
Matching with the left-hand side produces the bindings 
@rr[E] @math{=} @rr[((cont hole) (|+1| hole))] 
and 
@rr[v] @math{=} @rr[(λ (k) k)].
The binding for @rr[E] does not explicitly indicate whether @rr[(v (cont E))]
should fill the hole in @rr[cont] or the one in @rr[|+1|], but the requirement 
that the result matches the right-hand side rules out the former hole.

Unfortunately, this definition appears difficult to extend to reduction rules
whose right-hand sides appeal to meta-level functions, at least without 
sacrificing simplicity. Reduction cannot, for example, treat meta-level 
applications as inert syntax to be matched now and evaluated later since
application results can affect matching (and vice versa), as in the following
rule:
@(let ()
   (define-language dummy
     (E E)
     (r r))
   (define-metafunction dummy
     [(f) (f)])
   (define-metafunction dummy
     [(g) (g)])
   (centered
    (parameterize ([rule-pict-style 'horizontal])
      (render-reduction-relation
       (reduction-relation
        dummy
        (--> (in-hole E r) (f (in-hole (g r) E))))))))}
in which a reduction rule's left-hand side is a pattern but its right-hand
side is a term template @math{r} drawn from the grammar in the top-left.
A template is either an atom, a reference to a variable bound by the rule's 
left-hand side, a plug operation, a pair construction, or a meta-level 
application. Plug operations carry a pattern explained below.

The rule in the top-right defines when the term @math{t} reduces to @math{t'} by
the rule that takes @math{p} to @math{r}, namely when @math{p} matches @math{t}
and @math{r} instantiates to @math{t'} using the match's bindings. The
instantiation relation in the figure's center defines this latter condition.
All of the rules except the one for @pt[:in-hole] are straightforward. That rule
instantiates the context and sub-term templates then combines the results using
a generic formulation of plugging, parameterized by a pattern that distinguishes
pluggable holes by explaining the context's intended interpretation. This 
@math{plug} relation (bottom of figure) defines the result of plugging 
@math{t_1} with @math{t_2} to be a term that decomposes by @math{p} into 
@math{t_2} and a context @math{C} whose unencoded representation is @math{t_1}.

With the @rr[call/cc] rule's @pt[:in-hole] template annotated with the pattern
@rr[E],@note{For simplicity, the definition in @figure-ref{fig:reduction} 
requires the annotation on @pt[:in-hole] templates to be written explicitly, but
it can be inferred under reasonable conditions. These conditions require that
meta-functions carry signatures that describe their input and output as patterns
and that left-hand sides do not associate the same @pt[:var]-bound variable with
more than one pattern.} this definition, like the symmmetric one, properly 
handles the example reduction above; i.e., it plugs only the the @rr[|+1|] hole
in the term @rr[E] since the result of plugging the @rr[cont] hole does not 
decompose by the following pattern:
@(centered (rr (in-hole E ((λ (k) k) (cont ((cont hole) (|+1| hole)))))))

In general, though, @math{plug_G^p} is not a function. For example, consider a
variant of the language in @figure-ref{fig:cont} in which continuation values
are not wrapped by the @rr[cont] keyword. In the original language, the term
@rr[((cont hole) (|+1| 0))] reduces only to @rr[((cont hole) 1)], but in the
modified language, the same term, now written @rr[(hole (|+1| 0))], reduces to 
@rr[(hole 1)] @emph{and} @rr[(1 hole)]. The additional result arises because it
too decomposes by the pattern @rr[(in-hole E 1)], as required by @math{plug}.

This spurious reduction does not fit our intuition, but we consider the
discrepancy a problem not with the semantics for reduction but with the language
itself. In particular, the language's syntax is hopelessly ambiguous. Consider
the term @rr[(hole hole)]. In the original syntax, this term could correspond to
either @rr[((cont hole) (cont hole))], @rr[(cont (hole (cont hole)))], or
@rr[(cont ((cont hole) hole))].

We have not encountered a realistic language that triggers @math{plug}'s 
multi-valued behavior. It appears that this particular language would be
supported by a semantics in which plugging is defined on encoded contexts
instead of terms, since the former indicates the target hole. For example, 
matching @rr[(hole (|+1| 0))] against the left-hand side 
@rr[(in-hole E (|+1| number))] would produce bindings in which @rr[E] is the
context  @pt[(:right hole :no-ctxt)], which unambiguously indicates which hole
to fill. Other ambiguous languages, however, remain ambiguous. For example,
which hole should be filled by the following rule?
@(let ()
   (define-language dummy
     (E E)
     (r r))
   (centered
    (parameterize ([rule-pict-style 'horizontal])
      (render-reduction-relation
       (reduction-relation
        dummy
        (--> (in-hole E r) (in-hole (E E) r)))))))