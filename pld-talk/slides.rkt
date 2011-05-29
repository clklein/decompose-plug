#lang racket
(require slideshow
         redex
         "context.rkt"
         "title.rkt"
         "util.rkt"
         "../2-models/model.rkt")

(title)

(define (combine . args)
  (table 2 
         (apply
          append
          (map (λ (arg)
                 (list (scale (car arg) 2)
                       (apply para #:width 300 (cdr arg))))
               args))
         rc-superimpose ctl-superimpose 
         20 10))

(slide
 (para "To match:")
 (combine
  (list (pat atom) "match literal occurrence of" (pat atom) "e.g.,"
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
    10
    (scale (language->pict arith #:nts '(a)) 2)
    (blank 0 2)
    (para #:fill? #f
          (pat (+ a_1 a_2))
          "matches"
          (pat (+ (+ 1 2) 3)))
    (para #:fill? #f
          "with" (pat a_1) "=" (pat (+ 1 2))
          "and"
          (pat a_2) "=" (pat 3))))))

(context-picture)

(lesson "Contexts introduce ambiguity")
(lesson "Contexts must support multiple ways to decompose each expression form (app in this case)")
(lesson "Finding a decomposition may require other, unrelated decompositions")
(lesson "Finding a decomposition may require other, unrelated decompositions")
(lesson "Contexts may be involved in no decompositions during a match")
(lesson "The algorithm must deal with cycles well")