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
  (slide
   (hc-append
    40 
    (scale (vl-append (render-language Λ #:nts (remove 'x (language-nts Λ)))
                      (render-language Λ/red))
           2/3)
    (scale (vl-append 40
                      (vl-append (tt "#lang racket")
                                 (tt "(require redex)"))
                      lang-pict
                      extended-lang-pict)
           .8)))
  
  (slide
   (scale (with-subst-rewrite
           (λ ()
             (render-reduction-relation red #:style 'horizontal)))
          2/3)
   
   (blank)
   
   red-pict))
