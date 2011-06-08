#lang racket
(require slideshow
         "context.rkt"
         "title.rkt"
         "examples.rkt"
         "util.rkt"
         "timeline.rkt"
         "simple-pattern.rkt"
         "desiderata.rkt"
         "redex-code-example.rkt"
         "../2-models/models.rkt"
         racket/runtime-path
         redex)

(title)

(desiderata)

(a-redex-example)

(timeline)

(simple-pattern-overview)

(slide 
 (table 2
        (list (t "the hole:") (scale (pat hole) 1.5)
              (t "decomposition:") (scale (pat (in-hole pat pat)) 1.5))
        (cons rc-superimpose lc-superimpose)
        cbl-superimpose
        40 40))

(context-picture)

(example 
 arith :arith ()
 (in-hole C a)
 (+ 1 2))
(flush-examples)

(lesson "Contexts are a source of ambiguity")

(example
 Λ/red :Λ/red (x y)
 (in-hole E (e_1 e_2))
 ((f x) (g y)))
(flush-examples)

(lesson "Contexts must support multiple ways to decompose each expression form (app in this case)")

(example
 Λneed/red :Λneed/red (A v)
 (in-hole E (|+1| number))
 ((λ (x) (|+1| 2)) (|+1| 3)))

(example
 Λneed/red :Λneed/red (A v)
 (in-hole E (|+1| number))
 ((λ (x) (|+1| x)) (|+1| 3)))

(flush-examples)
(lesson "Finding a decomposition may require other, unrelated decompositions")

(example
 Λk/red/no-hide-hole :Λk/red ()
 (in-hole E (call/cc v))
 (|+1| (call/cc (λ (k) (|+1| (k (|+1| 2)))))))

(example
 Λk/red/no-hide-hole :Λk/red ()
 (in-hole E (|+1| number))
 (|+1| ((cont (|+1| hole)) (|+1| 2))))
(flush-examples)

(lesson "Contexts may be involved in no decompositions during a match")

(example
 Λdk/red :Λdk/red ()
 (in-hole M (|#| (in-hole E (call/comp v))))
 (|+1| (|#| (|+1| (call/comp (λ (k) (k 2))))))
 #:out-of-memory? #t)

(flush-examples)

(example
 wacky :wacky ()
 C
 (f (f hole))
 #:out-of-memory? #t)

(flush-examples)

(lesson "An algorithm must deal with cycles well")

(define-runtime-path sem-sem/patterns.rkt "../sem-sem/patterns.rkt")
(define-runtime-path paper/typeset-match-rules.rkt "../paper/typeset-match-rules.rkt")
(define-runtime-path paper/common.rkt "../paper/common.rkt")

(define-from patterns sem-sem/patterns.rkt)
(define-from matches-schema paper/typeset-match-rules.rkt)
(define-from matches-rules paper/typeset-match-rules.rkt)
(define-from decomposes-schema paper/typeset-match-rules.rkt)
(define-from decomposes-rules paper/typeset-match-rules.rkt)
(define-from with-keyword-rewriters paper/common.rkt)

(slide
 (scale-up
  (inset 
   (vc-append 20
              (hc-append 40
                         matches-schema
                         decomposes-schema)
              (over-there
               (λ () (parameterize ([render-language-nts '(p a t C)])
                       (with-keyword-rewriters 
                        (λ () 
                          (render-language patterns)))))))
   20)))

(define rules1
  (vr-append matches-schema
             matches-rules))

(define rules2
  (vr-append decomposes-schema
             decomposes-rules))

(slide (scale-up (cc-superimpose (ghost rules2) rules1)))

(slide (scale-up (cc-superimpose (ghost rules1) rules2)))
