#lang racket/base

(require redex/reduction-semantics 
         unstable/dict
         racket/match)
(provide (all-defined-out))

(define-language patterns
  ((t u) (:cons t t)
         a)
  (a :hole
     literal)
  (p a
     (:name x p)
     (:nt n)
     (:in-hole p p)
     (:cons p p))
  (literal variable-not-otherwise-mentioned
           number)
  (x variable-not-otherwise-mentioned)
  (n variable-not-otherwise-mentioned)
  
  
  (b (set (pair x v) ...))
  (v t C)
  
  (G (D ...))
  (D [x (p ...)])
  
  (C :no-ctxt
     (:left t C)
     (:right t C))
  
  (otherwise-mentioned ⊤))

(define pattern? (redex-match patterns p))
(define value? (redex-match patterns v))
(define term? (redex-match patterns t))
(define bindings? (redex-match patterns b))
(define grammar? (redex-match patterns G))