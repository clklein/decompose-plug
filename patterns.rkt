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

(define-metafunction patterns
  uncontext : C -> t
  [(uncontext no-frame)
   :hole]
  [(uncontext ((left t) C))
   (:cons (uncontext C) t)]
  [(uncontext ((right t) C))
   (:cons t (uncontext C))])

(define-metafunction patterns
  append-contexts : C C -> C
  [(append-contexts no-frame C)
   C]
  [(append-contexts (F C_1) C_2)
   (F (append-contexts C_1 C_2))])

(define encode-term
  (match-lambda
    ['() 'mt]
    [(cons t u)
     `(:cons ,(encode-term t)
             ,(encode-term u))]
    [t t]))

(define decode-term
  (term-match/single
   patterns
   [no-frame (term :hole)]
   [((left t) C)
    (cons (decode-term (term C))
          (decode-term (term t)))]
   [((right t) C)
    (cons (decode-term (term t))
          (decode-term (term C)))]
   [mt '()]
   [(:cons t_1 t_2)
    (cons (decode-term (term t_1))
          (decode-term (term t_2)))]
   [a (term a)]))