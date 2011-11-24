#lang racket/base
(require slideshow
         redex
         racket/runtime-path
         racket/draw
         "../aplas2011/2-models/util.rkt")
(provide pat lesson render-sexp scale-up define-from over-there
         clip-to assembler-transformation b-1925953)

(define to-orig-param (make-channel))

(define orig-param-thread
  (thread
   (λ ()
     (let loop ()
       (define l (channel-get to-orig-param))
       (channel-put (list-ref l 0) ((list-ref l 1)))
       (loop)))))

(define-syntax-rule
  (define-from id file)
  (define id (over-there (λ () (dynamic-require file 'id)))))

(define (over-there thunk) 
  (define c (make-channel))
  (channel-put to-orig-param (list c thunk))
  (channel-get c))

(literal-style "Inconsolata")
(non-terminal-style (literal-style))
(default-style (literal-style))
(non-terminal-subscript-style '(subscript . "Inconsolata"))

(default-font-size 55)
(label-font-size (default-font-size))
(metafunction-font-size (default-font-size))

(define (clip-to w h p)
  (inset/clip p 
              0 0
              (- w (pict-width p))
              (- h (pict-height p))))

(define extra-margin 16)

(define-runtime-path pattern.png "1925953.png")
(define b-1925953 (read-bitmap pattern.png))
(define tile (bitmap b-1925953))

(define bkg-pict
  (dc
   (λ (dc dx dy)
     (define brush (send dc get-brush))
     (define pen (send dc get-pen))
     (send dc set-brush (new brush%
                             [stipple b-1925953]
                             [transformation (assembler-transformation)]))
     (send dc set-pen "black" 1 'transparent)
     (send dc draw-rectangle dx dy 1024 768)
     (send dc set-brush brush)
     (send dc set-pen pen))
   1024 768))

(define bkg
  (cc-superimpose
   bkg-pict
   #;
   (clip-to
    1024 768
    (apply hc-append
           (make-list 
                      (apply vc-append
                             (make-list (ceiling (/ 768 (pict-height tile))) 
                                        tile)))))
   (cellophane (colorize (filled-rounded-rectangle
                          (- 1024 margin extra-margin)
                          (- 768 margin extra-margin)
                          40
                          #:draw-border? #f)
                         "white")
               .95)))

(current-slide-assembler
 (let ([c-a-s (current-slide-assembler)])
   (λ (a b c)
     (define pict
       (ct-superimpose (inset bkg (- margin))
                       (c-a-s a b c)))
     (define drawer (make-pict-drawer pict))
     (dc
      (λ (dc dx dy)
        (parameterize ([assembler-transformation (send dc get-transformation)])
          (drawer dc dx dy)))
      (pict-width pict)
      (pict-height pict)))))
(define assembler-transformation (make-parameter #f))

(define-syntax-rule (pat arg)
  (rr arg))

(define (lesson . args)
  (define p 
    (inset (colorize (ht-append (scale/improve-new-text (bt "Lesson: ") 2)
                                (apply para #:width 400 #:fill? #f args))
                     "white")
           20 10))
  (slide 
   (cc-superimpose
    (colorize (filled-rectangle (pict-width p) (pict-height p))
              "black")
    p)))

(define (render-sexp sexp)
  (define fixed-hole
    (let loop ([sexp sexp])
      (cond
        [(equal? sexp (term hole)) 'hole]
        [(pair? sexp) (cons (loop (car sexp)) (loop (cdr sexp)))]
        [else sexp])))
  (define p (open-input-string (format "~s" fixed-hole)))
  (port-count-lines! p)
  (lw->pict typesetting-lang (to-lw/stx (read-syntax #f p))))

(define (scale-up p)
  (scale p (min (/ (- client-w 12) (pict-width p))
                (/ (- client-h 12) (pict-height p)))))