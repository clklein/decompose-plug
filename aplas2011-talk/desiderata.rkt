#lang racket
(require slideshow)

(provide desiderata)

(define bubbles
  `(("Programs"
     "look and act like"
     "semantic defns,"
     ("but they " ,(it "run")))
    ("Support the"
     "entire semantics"
     "engineering"
     "lifecycle")
    ("Focus on"
     "lightweight"
     "validation"
     "techniques")))

(define (raw-bubble-pict strs)
  (define (str->pict s)
    (cond
      [(list? s)
       (apply hbl-append (map str->pict s))]
      [(pict? s)
       s]
      [else
       (t s)]))
  (apply vc-append (map str->pict strs)))

(define ghost-bubble
  (ghost (launder (apply cc-superimpose (map raw-bubble-pict bubbles)))))

(define (bubble index)
  (define p (cc-superimpose ghost-bubble (raw-bubble-pict (list-ref bubbles index))))
  (define size (* (max (pict-width p) (pict-height p)) 1.2))
  (cc-superimpose 
   (colorize (filled-ellipse size size)
             "black")
   (colorize p "white")))

(define (desiderata)
  (slide
   (t "Redex is a Programming Language")
   (blank)
   (hc-append 100 
              (bubble 0)
              (bubble 1))
   (bubble 2)))