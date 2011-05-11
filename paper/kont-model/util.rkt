#lang racket/base
(require redex/pict
         "model.rkt")

(provide rr)

(define-syntax-rule 
  (rr exp) 
  (lw->pict Λk/red (to-lw exp)))
