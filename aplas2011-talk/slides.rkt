#lang racket
(require slideshow
         "context.rkt"
         "title.rkt"
         "examples.rkt"
         "util.rkt"
         "simple-pattern.rkt"
         "desiderata.rkt"
         "redex-code-example.rkt"
         "../aplas2011/2-models/models.rkt"
         racket/runtime-path
         redex)

(title)

(slide
 (scale/improve-new-text 
  (pat (in-hole E | |))
  4))

(desiderata)

(a-redex-example)

(simple-pattern-overview)

(slide 
 (table 2
        (list (scale (pat hole) 1.5) (t "the hole") 
              (scale (pat (in-hole pat pat)) 1.5) (t "decomposition") )
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
 left-rec-eval-ctxt :left-rec-eval-ctxt (e v x)
 (in-hole E (|+1| number))
 ((λ (x) x) (|+1| 2))
 #:out-of-memory? #t)

(flush-examples
 (λ (i p)
   (if (= i 1)
       (list p
             (rt-superimpose p (scale (contexts-above) 2/3)))
       (list p))))
   

(lesson "An algorithm must deal with cycles well")

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

#|
(example
 Λk/red :Λk/red ()
 (in-hole E (call/cc v))
 (|+1| (call/cc (λ (k) (|+1| (k (|+1| 2)))))))

(example
 Λk/red :Λk/red ()
 (in-hole E (|+1| number))
 (|+1| ((cont (|+1| hole)) (|+1| 2))))
(flush-examples)

(lesson "Contexts may be involved in no decompositions during a match")
|#

(define-runtime-path semantics/patterns.rkt "../semantics/patterns.rkt")
(define-runtime-path typeset-match-rules.rkt "../aplas2011/typeset-match-rules.rkt")
(define-runtime-path typeset-reduction.rkt "../aplas2011/typeset-reduction.rkt")
(define-runtime-path common.rkt "../aplas2011/common.rkt")

(define-from patterns semantics/patterns.rkt)
(define-from matches-schema typeset-match-rules.rkt)
(define-from matches-rules typeset-match-rules.rkt)
(define-from decomposes-schema typeset-match-rules.rkt)
(define-from decomposes-rules typeset-match-rules.rkt)
(define-from with-keyword-rewriters common.rkt)
(define-from render-r-grammar typeset-reduction.rkt)
(define-from render-reduces typeset-reduction.rkt)
(define-from with-reduction-rewriters typeset-reduction.rkt)

(let ([t-nt
       (λ (nt)
         (over-there
          (λ () (parameterize ([render-language-nts (list nt)])
                  (with-keyword-rewriters 
                   (λ () 
                     (render-language patterns)))))))])
  (slide
   (scale-up
    (inset 
     (hc-append 40
                matches-schema
                decomposes-schema)
     20))))

(define rules1
  (vr-append matches-schema
             matches-rules))

(define rules2
  (vr-append decomposes-schema
             decomposes-rules))

(slide (scale-up (cc-superimpose (ghost rules2) rules1)))

(slide (scale-up (cc-superimpose (ghost rules1) rules2)))

(slide (scale-up (over-there
                  (λ ()
                    (with-reduction-rewriters
                     (λ ()
                       (vc-append 
                        40 
                        (blank)
                        (with-compound-rewriter
                         'tuple  ;; hack to avoid showing 'inst' returning 2 values
                         (λ (lws) (list (list-ref lws 2)))
                         (render-reduces))
                        (render-r-grammar)
                        (blank)
                        (blank))))))))

(slide (scale/improve-new-text (t "Thank you.") 2))