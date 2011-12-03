#lang racket/base

(require (except-in slideshow #;printable<%>)
         racket/gui/base
         racket/class
         racket/math
         "util.rkt")

(provide title)

(define white (make-object color% 255 255 255))
(define black (make-object color% 0 0 0))
(define gray (make-object color% 50 50 50))
(define deep-purple (make-object color% 51 0 102))
(define shallow-purple (make-object color% 204 153 255))

(define (make-brush-bitmap bmp)
  (define tile (bitmap bmp))
  (define pict 
    (clip-to
     1024 768
     (apply hc-append
            (make-list (ceiling (/ 1024 (pict-width tile)))
                       (apply vc-append
                              (make-list (ceiling (/ 768 (pict-height tile))) 
                                         tile))))))
  (define bm (make-bitmap 1024 768))
  (define bdc (make-object bitmap-dc% bm))
  (draw-pict pict bdc 0 0)
  (send bdc set-bitmap #f)
  bm)

(define (adjust-bitmap bmp f)
  (define w (send bmp get-width))
  (define h (send bmp get-height))
  (define new-bmp (make-bitmap w h))
  (define pixels (make-bytes (* w h 4)))
  (send bmp get-argb-pixels 0 0 w h pixels)
  (for ([i (in-range 0 (* 4 w h) 4)])
    (define a (bytes-ref pixels i))
    (define r (bytes-ref pixels (+ i 1)))
    (define g (bytes-ref pixels (+ i 2)))
    (define b (bytes-ref pixels (+ i 3)))
    (define-values (a2 r2 g2 b2) (f a r g b))
    (bytes-set! pixels i a2)
    (bytes-set! pixels (+ i 1) r2)
    (bytes-set! pixels (+ i 2) g2)
    (bytes-set! pixels (+ i 3) b2))
  (send new-bmp set-argb-pixels 0 0 w h pixels)
  new-bmp)

(define violety (make-brush-bitmap b-1925953))
(define redy (make-brush-bitmap (adjust-bitmap b-1925953 (λ (a r g b) (values a r g g)))))
(define bluey (make-brush-bitmap (adjust-bitmap b-1925953 (λ (a r g b) (values a g g b)))))
(define deep-violety 
  (make-brush-bitmap (adjust-bitmap b-1925953 (λ (a r g b) 
                                                (define (adj n) (round (* n 1/2)))
                                                (values a 
                                                        (adj r)
                                                        (adj g)
                                                        (adj b))))))

(define (with-dc-settings dc thunk)
  (let ([alpha (send dc get-alpha)]
        [smoothing (send dc get-smoothing)]
        [pen (send dc get-pen)]
        [brush (send dc get-brush)])
    (thunk)
    (send dc set-alpha alpha)
    (send dc set-smoothing smoothing)
    (send dc set-pen pen)
    (send dc set-brush brush)))

(define (make-plt-title-background plt-red-color plt-blue-color plt-background-color plt-lambda-color
                                   plt-pen-color plt-pen-style
                                   #:clip? [clip? #t]
                                   #:edge-cleanup-pen [edge-cleanup-pen #f]
                                   #:pen-size [pen-size 0])
  (let ()
    (define left-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 153 44)
        (send p line-to 161.5 60)
        (send p curve-to 202.5 49 230 42 245 61)
        (send p curve-to 280.06 105.41 287.5 141 296.5 186)
        (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
        (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
        (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
        (send p line-to 135 572)
        p))
    
    (define left-logo-path
      (let ([p (new dc-path%)])
        (send p append left-lambda-path)
        (send p arc 0 0 630 630 (* 235/360 2 pi) (* 121/360 2 pi) #f)
        p))
    
    (define bottom-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 135 572)
        (send p line-to 188.5 564)
        (send p curve-to 208.5 517 230.91 465.21 251 420)
        (send p curve-to 267 384 278.5 348 296.5 312)
        (send p curve-to 301.01 302.98 318 258 329 274)
        (send p curve-to 338.89 288.39 351 314 358 332)
        (send p curve-to 377.28 381.58 395.57 429.61 414 477)
        (send p curve-to 428 513 436.5 540 449.5 573)
        (send p line-to 465 580)
        (send p line-to 529 545)
        p))
    
    (define bottom-logo-path
      (let ([p (new dc-path%)])
        (send p append bottom-lambda-path)
        (send p arc 0 0 630 630 (* 314/360 2 pi) (* 235/360 2 pi) #f)
        p))
    
    (define right-lambda-path
      (let ([p (new dc-path%)])
        (send p move-to 153 44)
        (send p curve-to 192.21 30.69 233.21 14.23 275 20)
        (send p curve-to 328.6 27.4 350.23 103.08 364 151)
        (send p curve-to 378.75 202.32 400.5 244 418 294)
        (send p curve-to 446.56 375.6 494.5 456 530.5 537)
        (send p line-to 529 545)
        p))
    
    (define right-logo-path
      (let ([p (new dc-path%)])
        (send p append right-lambda-path)
        (send p arc 0 0 630 630 (* 314/360 2 pi) (* 121/360 2 pi) #t)    
        p))
    
    (define lambda-path ;; the lambda by itself (no circle)
      (let ([p (new dc-path%)])
        (send p append left-lambda-path)
        (send p append bottom-lambda-path)
        (let ([t (make-object dc-path%)])
          (send t append right-lambda-path)
          (send t reverse)
          (send p append t))
        (send p close)
        p))
    
    #;
    (define lambda-path
      (let ([p (new dc-path%)])
        (send p append left-lambda-path)
        (send p append bottom-lambda-path)
        (send p append right-lambda-path)
        p))
    
    ;; This function draws the paths with suitable colors:
    (define (paint-plt dc dx dy)
      (send dc set-smoothing 'aligned)
      (let ([old-pen (send dc get-pen)]
            [old-brush (send dc get-brush)]
            [old-clip (send dc get-clipping-region)])
        
        (send dc set-pen plt-pen-color pen-size plt-pen-style)
        
        (cond
          [(procedure? plt-lambda-color)
           (with-dc-settings 
            dc
            (λ ()
              (plt-lambda-color dc) 
              (send dc draw-path lambda-path dx dy)))]
          [plt-lambda-color
           (send dc set-brush plt-lambda-color 'solid)
           (send dc draw-path lambda-path dx dy)]
          [else 
           (void)])
        
        ;; Draw red regions
        (cond
          [(is-a? plt-red-color bitmap%)
           (send dc set-brush (new brush% 
                                   [stipple plt-red-color]
                                   [transformation (assembler-transformation)]))
           (send dc draw-path left-logo-path dx dy)
           (send dc draw-path bottom-logo-path dx dy)]
          [(procedure? plt-red-color)
           (with-dc-settings 
            dc
            (λ ()
              (plt-red-color dc)
              (send dc draw-path left-logo-path dx dy)
              (send dc draw-path bottom-logo-path dx dy)))]
          [else
           (send dc set-brush plt-red-color 'solid)
           (send dc draw-path left-logo-path dx dy)
           (send dc draw-path bottom-logo-path dx dy)])
        
        ;; Draw blue region
        (cond
          [(is-a? plt-blue-color bitmap%)
           (send dc set-brush (new brush% 
                                   [stipple plt-blue-color]
                                   [transformation (assembler-transformation)]))
           (send dc draw-path right-logo-path dx dy)]
          [(procedure? plt-blue-color)
           (with-dc-settings 
            dc
            (λ ()
              (plt-blue-color dc) 
              (send dc draw-path right-logo-path dx dy)))]
          [else
           (send dc set-brush plt-blue-color 'solid)
           (send dc draw-path right-logo-path dx dy)])
        
        (send dc set-pen old-pen)
        (send dc set-brush old-brush)
        (send dc set-clipping-region old-clip)))
    
    (define (cleanup-edges path dc dx dy)
      (when edge-cleanup-pen
        (let ([pen (send dc get-pen)]
              [brush (send dc get-brush)]
              [alpha (send dc get-alpha)])
          (send dc set-pen edge-cleanup-pen)
          (send dc set-brush "black" 'transparent)
          (send dc set-alpha .8)
          (send dc draw-path path dx dy)
          (send dc set-pen pen)
          (send dc set-brush brush)
          (send dc set-alpha alpha))))
    
    (define bkg
      (cond
        [(is-a? plt-background-color bitmap%)
         (dc
          (λ (dc dx dy)
            (with-dc-settings
             dc
             (λ ()
               (send dc set-brush (new brush% 
                                       [stipple plt-red-color]
                                       [transformation (assembler-transformation)]))
               (send dc set-pen "black" 1 'transparent)
               (send dc draw-rectangle dx dy client-w client-h))))
          client-w client-h)]
        [plt-background-color
         (colorize (filled-rectangle client-w client-h)
                    plt-background-color)]
        [else (blank client-w client-h)]))
    
    ((if clip? clip values)
     (pin-over
      bkg
      320
      50
      (scale (dc paint-plt 630 630 0 0) 12/10)))))

(define bw-title-background 
  (make-plt-title-background redy ; light-gray
                             bluey ; light-gray
                             #f
                             white ; gray ; red
                             black 
                             'transparent))

(define wb-title-background 
  (make-plt-title-background deep-violety ; deep-purple
                             deep-violety
                             deep-violety
                             black
                             black
                             'transparent))

(define (title)
  (define names 
    (hbl-append
     30
     (t "Casey Klein")
     (t "Jay McCarthy")
     (t "Steven Jaconette")
     (t "Robby Findler")))
  (define title 
    (vc-append (bt "A Semantics for Context-Sensitive")
               (bt "Reduction Semantics")))
  (define title-info
    (colorize (inset 
               (vc-append 20 
                          (scale title
                                 (/ (pict-width names)
                                    (pict-width title)))
                          names)
               40
               40)
              "white"))
  (define p
    (cc-superimpose
     wb-title-background
     title-info))
  
  (slide 
   (cc-superimpose 
    bw-title-background
    (clip (refocus p title-info)))))
