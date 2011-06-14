#lang scribble/base
@(require scribble/manual
          scriblib/figure
          redex/pict
          redex/reduction-semantics
          (only-in "../2-models/util.rkt" rr)
          "../2-models/models.rkt"
          "../sem-sem/patterns.rkt"
          "typeset-match-rules.rkt"
          "typeset-reduction.rkt")

@title[#:tag "sec:reduction"]{A Semantics for Reduction}

We now put the notion of matching from @secref{sec:match-rules} to work in a
formalization of the standard notation for context-sensitive reduction rules. 
As with patterns, we consider a core specification language that lacks
many of the conveniences of a language like Redex but nevertheless addresses the
principal ideas.

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
        (--> (in-hole E r) (f (in-hole (g r) E))))))))

We therefore prefer the asymmetric definition in @figure-ref{fig:reduction},
in which a reduction rule's left-hand side is a pattern but its right-hand
side is a term template @math{r} drawn from the grammar in the top-left.
A template is either an atom, a reference to a variable bound by the rule's 
left-hand side, a plug operation, a pair construction, or a meta-level 
application. Plug operations carry a pattern that identifies pluggable holes,
a point we we revisit shortly.

The rule in the top-right defines when @math{t} reduces to @math{t'} by the
rule that takes @math{p} to @math{r}, namely when @math{p} matches @math{t}
and @math{r} instantiates to @math{t'} using the match's bindings. The
instantiation relation in the figure's center defines this latter condition.
All of the rules except the @pt[:in-hole] are straightforward. This rule
instantiates the 

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

@;{
Notes:
   
Revisit previous example.   
   
But note that in general the plug_G^p relation, and consequently also inst,
is not a function. Consider a modification to the syntax of the language in
figure X.
...

(Mention that rewriting-inspired defition behaves the same.)

This example, however, is not realistic. Eliminating the cont wrapper makes the
grammar unambiguous. For example, consider the term t. With the wrappers inserted,
does it become t', t'', t''', or yet another term?

We have not encountered a realistic example that triggers plug's multi-valued behavior.
If this were a concern, we would pursue a semantics in which plug operates on encoded
contexts. In addition to require template instantiation to receive encoded contexts, 
this arrangement forces terms to contain contexts. For example, if plugging is defined
only on terms with a distinguished hole, the call/cc rule must construct a term 
containing an embedded context, to be plugged in a later reduction step.

}