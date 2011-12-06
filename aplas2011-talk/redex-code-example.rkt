#lang racket
(require redex
         slideshow
         slideshow/code
         "util.rkt")
(provide a-redex-example)

(current-keyword-color "black")
(current-id-color "black")
(current-literal-color "black")
(current-base-color "black")
(current-const-color "black")

(define-syntax-rule
  (with-pict p defs ...)
  (begin
    (define p
      (code defs ...))
    defs ...))

(with-pict
 lang-pict
 (define-language Λ
   (e (e e)
      (λ (x) e)
      (+ e e)
      x)
   (x variable-not-otherwise-mentioned)))

(define language-inside
  (code (e (e e)
           (λ (x) e)
           (+ e e)
           x)))
(define language-outside
  (code (define-language Λ
          #,language-inside
          (x variable-not-otherwise-mentioned))))
  
  
(with-pict
 extended-lang-pict
 (define-extended-language Λ/red Λ
   (E (E e) 
      (v E) 
      (+ E e)
      (+ v E)
      hole)
   (v (λ (x) e)
      number)))

(define-metafunction Λ
  [(Σ number ...) ,(apply + (term (number ...)))])

(with-pict
 red-pict
 (define red
   (reduction-relation
    Λ/red
    (--> (in-hole E ((λ (x) e) v))
         (in-hole E (subst e x v)))
    (--> (in-hole E (+ number_1 number_2))
         (in-hole E (Σ number_1 number_2))))))

(define (with-subst-rewrite thunk)
  (with-compound-rewriter 'subst
                          (λ (lws)
                            (define inner-lws (lw-e (list-ref lws 3)))
                            (define e (list-ref lws 2))
                            (define x (list-ref lws 3))
                            (define v (list-ref lws 4))
                            (list "" e "{" x ":=" v "}"))
                          (thunk)))

(define (a-redex-example)
  (define lang-code-total
    (scale (vl-append 40
                      (vl-append (tt "#lang racket")
                                 (tt "(require redex)"))
                      lang-pict
                      extended-lang-pict)
           .8))
  
  (define math-lang (vl-append (scale (ghost (code abc)) .8)
                               (scale (render-language Λ #:nts (remove 'x (language-nts Λ)))
                                      .6)))
  (define math-extended-lang (scale (render-language Λ/red)
                                    .6))
  
  (define beside-offset (+ (max (pict-width math-lang)
                                (pict-width math-extended-lang))
                           20))
  (define (add-beside in-pict
                      new-pict
                      main)
    (define-values (dx dy) (lt-find main in-pict))
    (pin-over main
              (- beside-offset)
              dy
              new-pict))
                      
  
  (slide
   (panorama
    (add-beside 
     extended-lang-pict
     math-extended-lang
     (add-beside 
      lang-pict
      math-lang
      lang-code-total))))
               
  
  (slide
   (parameterize ([white-bracket-sizing 
                   (λ (str size)
                     (let ([inset-amt 18])
                       (cond
                         [(equal? str "[")
                          (values inset-amt
                                  0
                                  0
                                  (/ inset-amt 2))]
                         [else
                          (values 0
                                  inset-amt
                                  (/ inset-amt 2)
                                  0)])))])
     (scale/improve-new-text
      (with-subst-rewrite
       (λ ()
         (render-reduction-relation red #:style 'horizontal)))
      2/3))
   
   (blank)
   
   red-pict))
