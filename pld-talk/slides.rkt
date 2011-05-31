#lang racket
(require slideshow
         redex
         "context.rkt"
         "title.rkt"
         "examples.rkt"
         "util.rkt"
         "../2-models/models.rkt")

(title)

(define (combine . args)
  (table 2 
         (apply
          append
          (map (λ (arg)
                 (list (scale (car arg) 1.5)
                       (apply para #:width 300 (cdr arg))))
               args))
         rc-superimpose ctl-superimpose 
         20 10))

(slide
 (para "To match:")
 (combine
  (list (pat literal) "match literal occurrence e.g.,"
        (pat λ) "," (pat 17) "," (pat +))
  (list (pat (pat ...)) "match an interior node in a tree; one child per"
        (pat pat) "in the sequence")
  (list (pat nt) "try all of the productions of" (pat nt))))

(slide
 (vl-append
  40
  (para #:fill? #f "Example:")
  (ht-append
   (blank 40 0)
   (vl-append
    60
    (language->pict arith #:nts '(a))
    (para #:fill? #f
          (pat (+ a_1 a_2))
          "matches"
          (pat (+ (+ 1 2) 3)))
    (para #:fill? #f
          "with" (pat a_1) "=" (pat (+ 1 2))
          "and"
          (pat a_2) "=" (pat 3))))))

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

 ;wacky
 ;wacky-inside-out
(flush-examples)
(lesson "The algorithm must deal with cycles well")
