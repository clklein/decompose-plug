#lang racket/base
(require (for-syntax racket/base)
         scribble/core
         scribble/latex-properties
         scribble/decode)
(provide wfigure)

(define-syntax (wfigure stx)
   (syntax-case stx ()
     [(_ #:size s . args)
      #'(wfigure/proc s (list . args))]
     [(_ . args)
      #'(wfigure #:size 2 . args)]))
                                        
(define (wfigure/proc size args)
  (define rendered-size
    (cond
      [(equal? size 2) "T"]
      [(equal? size 2.5) "TF"]
      [(equal? size 2.2) "TT"]
      [else (error 'wfigure "unknown size: ~s" size)]))
  (define f (decode-flow args))
  (nested-flow
   (style (format "Wfigure~a" rendered-size)
          (list (make-tex-addition "wfigure.tex")))
   f))

