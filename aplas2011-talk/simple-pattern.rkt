#lang racket
(require slideshow
         redex
         "util.rkt"
         "../aplas2011/2-models/models.rkt")

(provide simple-pattern-overview)

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

(define (simple-pattern-overview)
  (slide
   (let ([m (t "Match  :")]) 
     (scale (vl-append 
             (hbl-append m (t "  Grammar  Pat  Term"))
             (hbl-append (rbl-superimpose (ghost m) (t "→"))
                         (t "  Substitution*  or  Failure")))
            1.5))
   
   (blank)
   
   (vl-append
    10
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
  
  (slide
   (para "To match:")
   (combine
    (list (pat literal) "match literal occurrence e.g.,"
          (pat λ) "," (pat 17) "," (pat +))
    (list (pat (pat ...)) "match an interior node in a tree; one child per"
          (pat pat) "in the sequence")
    (list (pat nt) "try all of the alternatives for" (pat nt)))))
