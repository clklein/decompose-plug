#lang racket
(require (prefix-in 2: 2htdp/image)
         slideshow
         (only-in mrlib/image-core render-image)
         redex/pict
         "util.rkt")
(provide context-picture
         contexts-above)

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

(define (triangle-context e? upper-label?)
  (cb-superimpose
   (label (i->p (2:triangle 300 'outline (2:pen "black" 20 "solid" "round" "round")))
          (if upper-label? C (ghost C)))
   ((if e? values ghost)
    (colorize
     (label (i->p (2:triangle 100 'outline (2:pen "red" 20 "solid" "round" "round")))
            e)
     "red"))))

(define (decomp red1? red2?)
  (with-atomic-rewriter
   'specialpat1 
   (λ () (if red1?
             (colorize (pat pat_1) "red")
             (pat pat_1)))
   (with-atomic-rewriter
    'specialpat2 
    (λ () (if red2?
              (colorize (pat pat_2) "red")
              (pat pat_2)))
    (scale (pat (in-hole specialpat1 specialpat2)) 1.5))))

(define (stage-context-picture tri2 body red1? red2?)
  (slide 
   (vl-append 120 
              (hbl-append (t "to match ")
                          (decomp red1? red2?)
                          (body (t ",")))
              (body
               (ht-append
                60
                (vc-append 10
                           (triangle-context #f #t) 
                           (vc-append
                            (hbl-append (t "first match ")
                                        C
                                        (t ", treating"))
                            (t "the hole as a wildcard,")))
                (tri2 (vc-append 10
                                 (triangle-context #t #t) 
                                 (vc-append
                                  (hbl-append (t "then match ")
                                              (colorize e "red")
                                              (t " against"))
                                  (t "the spot where the hole went")))))))))

(define (context-picture)
  (stage-context-picture ghost ghost #f #f)
  (stage-context-picture ghost values #t #f)
  (stage-context-picture values values #f #t))

(define (contexts-above)
  (vc-append
   40
   (triangle-context #f #t)
   (triangle-context #t #f)))
