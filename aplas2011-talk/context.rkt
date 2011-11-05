#lang racket
(require (prefix-in 2: 2htdp/image)
         slideshow
         (only-in mrlib/image-core render-image)
         "util.rkt")
(provide context-picture)

(define (i->p i)
  (dc (λ (dc dx dy)
        (render-image i dc dx dy))
      (2:image-width i)
      (2:image-height i)))

(define C (pat pat_1))
(define e (pat pat_2))

(define (label p1 p2)
  (refocus (vc-append 10  p2 p1)
           p1))

(define (triangle-context e?)
  (cb-superimpose
   (label (i->p (2:triangle 300 'outline (2:pen "black" 20 "solid" "round" "round")))
          C)
   ((if e? values ghost)
    (label (i->p (2:triangle 100 'outline (2:pen "black" 20 "solid" "round" "round")))
           e))))

(define decomp
  (scale (pat (in-hole pat_1 pat_2)) 1.5))

(define (stage-context-picture tri2)
  (slide 
   (vl-append 120 
              (hbl-append (t "to match ") 
                          decomp
                          (t ","))
              (ht-append
               60
               (vc-append 10
                          (triangle-context #f) 
                          (vc-append
                           (hbl-append (t "first match ")
                                       C
                                       (t ", treating"))
                           (t "the hole as a wildcard,")))
               (tri2 (vc-append 10
                                (triangle-context #t) 
                                (vc-append
                                 (hbl-append (t "then match ")
                                             e
                                             (t " against"))
                                 (t "the spot where the hole went"))))))))
(define (context-picture)
  (stage-context-picture ghost)
  (stage-context-picture values))
