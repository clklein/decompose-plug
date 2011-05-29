#lang racket/base
(require slideshow
         redex
         "../2-models/util.rkt")
(provide pat lesson)

(literal-style "Inconsolata")
(non-terminal-style (literal-style))
(default-style (literal-style))
(non-terminal-subscript-style '(subscript . "Inconsolata"))

(define-syntax-rule (pat arg)
  (scale (rr arg) 2))

(define (lesson . args)
  (define p 
    (colorize (inset (apply para #:fill? #f (scale/improve-new-text (bt "Lesson:") 2) args) 20 10) "white"))
  (slide 
   (cc-superimpose
    (colorize (filled-rectangle (pict-width p) (pict-height p))
              "black")
    p)))