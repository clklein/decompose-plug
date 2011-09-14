#lang racket/base

(require redex/reduction-semantics 
         unstable/dict
         racket/match)
(provide (all-defined-out))

(define-language patterns
  (t (:cons t t)
     (:left t t)
     (:right t t)
     a :hole)
  (C :hole
     (:left C t)
     (:right t C))
  
  (p a :hole
     (:name x p)
     (:nt n)
     (:in-hole p p)
     (:cons p p))
  (a variable-not-otherwise-mentioned
     number)
  (x variable-not-otherwise-mentioned)
  (n variable-not-otherwise-mentioned)
  
  (b (set (pair x t) ...))
  
  (G (D ...))
  (D [x (p ...)])
  
  (k :cons :left :right) ; for convenience in matching rules
  
  (otherwise-mentioned ⊤))

(define pattern? (redex-match patterns p))
(define atom? (redex-match patterns a))
(define constructor? (redex-match patterns k))
(define term? (redex-match patterns t))
(define bindings? (redex-match patterns b))
(define grammar? (redex-match patterns G))
