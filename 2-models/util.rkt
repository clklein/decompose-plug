#lang racket/base
(require redex/pict
         redex/reduction-semantics
         "models.rkt")

(provide rr with-rewriters)

;; use this dummy language to get the right
;; fonts for the various non-terminals used 
;; in section 2.
(define-extended-language typesetting-lang
  Λk/red
  ((M C) E)
  (a e)
  ((f g) x))
  
(define-syntax-rule 
  (rr exp) 
  (lw->pict typesetting-lang (to-lw exp)))

(define-syntax-rule 
  (with-rewriters exp)
  (with-subst-rewrite (λ () exp)))

(define (with-subst-rewrite thunk)
  (with-compound-rewriter 'subst
                          (λ (lws)
                            (define inner-lws (lw-e (list-ref lws 3)))
                            (define x (list-ref inner-lws 1))
                            (define v (list-ref inner-lws 2))
                            (define e (list-ref lws 2))
                            (list "" e "{" x ":=" v ", ...}"))
                          (thunk)))

