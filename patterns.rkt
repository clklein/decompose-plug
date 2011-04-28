#lang racket

(require redex)
(provide (all-defined-out))

(define-language patterns
  (p a
     (:name x p)
     (:nt x)
     (:in-hole p p)
     (:cons p p))
  (a :hole
     variable-not-otherwise-mentioned
     number)
  (x variable-not-otherwise-mentioned)
  
  ((t u) a
         (:cons t t))
  
  (b ([x v] ...))
  (v t C)
  
  (L (n ...))
  (n [x (p ...)])
  
  (F (left t)
     (right t))
  (C no-frame
     (F C)))