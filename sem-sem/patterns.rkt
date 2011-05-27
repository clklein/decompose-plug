#lang racket/base

(require redex/reduction-semantics 
         unstable/dict
         racket/match)
(provide (all-defined-out))

(define-language patterns
  (p a
     (:name x p)
     (:nt n)
     (:in-hole p p)
     (:cons p p))
  (a :hole
     literal)
  (literal variable-not-otherwise-mentioned
           number)
  (x variable-not-otherwise-mentioned)
  (n variable-not-otherwise-mentioned)
  
  ((t u) a
         (:cons t t))
  
  (b ((pair x v) ...))
  (v t C)
  
  (L (D ...))
  (D [x (p ...)])
  
  (F (:left t)
     (:right t))
  (C :no-context
     (F C)))

(define pattern? (redex-match patterns p))
(define value? (redex-match patterns v))
(define term? (redex-match patterns t))
(define bindings? (redex-match patterns b))
(define language? (redex-match patterns L))