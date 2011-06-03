#lang racket/base
(require (for-syntax racket/base)
         scribble/core
         scribble/latex-properties
         scribble/decode
         scriblib/figure
         setup/main-collects)
(provide wfigure figure-ref Figure-ref)

(define-syntax (wfigure stx)
   (syntax-case stx ()
     [(_ #:size s tag caption . args)
      #'(wfigure/proc tag caption s (list . args))]
     [(_ tag caption . args)
      #'(wfigure #:size 2 tag caption . args)]))

;; abstraction breaking ...
(define figure-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list (make-tex-addition (abs "figure.tex")))))

(define (wfigure/proc tag caption size args)
  (define rendered-size
    (cond
      [(equal? size 2) "T"]
      [(equal? size 2.5) "TF"]
      [(equal? size 2.2) "TT"]
      [(equal? size 3) "TH"]
      [else (error 'wfigure "unknown size: ~s" size)]))
  (define f (decode-flow args))
  (nested-flow
   (style (format "Wfigure~a" rendered-size)
          (list (make-tex-addition "wfigure.tex")))
   (append 
    f
    (list
     (make-paragraph
      plain
      (list
       (make-element (make-style "Legend" figure-style-extras)
                     (list (Figure-target tag) ": " 
                           caption))))))))

