#lang racket/base
(require redex/pict
         redex/reduction-semantics
         "model.rkt")

(provide rr)

;; use this dummy language to get the right
;; fonts for the various non-terminals used 
;; in section 2.
(define-extended-language typesetting-lang
  Λk/red
  (C λ)
  (a λ))
  
(define-syntax-rule 
  (rr exp) 
  (lw->pict typesetting-lang (to-lw exp)))
