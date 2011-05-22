#lang racket

(require redex/reduction-semantics unstable/dict)
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
  
  (F (:left t)
     (:right t))
  (C :no-context
     (F C)))

(define-metafunction patterns
  uncontext : C -> t
  [(uncontext :no-context)
   :hole]
  [(uncontext ((:left t) C))
   (:cons (uncontext C) t)]
  [(uncontext ((:right t) C))
   (:cons t (uncontext C))])

(define-metafunction patterns
  append-contexts : C C -> C
  [(append-contexts :no-context C)
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
   [no-context (term :hole)]
   [((:left t) C)
    (cons (decode-term (term C))
          (decode-term (term t)))]
   [((:right t) C)
    (cons (decode-term (term t))
          (decode-term (term C)))]
   [mt '()]
   [(:cons t_1 t_2)
    (cons (decode-term (term t_1))
          (decode-term (term t_2)))]
   [a (term a)]))

(define no-contexts-bindings
  (let ([context? (redex-match patterns C)])
    (λ (bs)
      (for/list ([b bs])
        (for/list ([m b])
          (match m
            [(list x v)
             (list x (term (non-context ,v)))]))))))

(define-metafunction patterns
  non-context : v -> t
  [(non-context t) t]
  [(non-context C) (uncontext C)])

(define (merge-bindings b1 b2)
  (let/ec return
    (define (c v w)
      (or (term (merge-value ,v ,w))
          (return false)))
    (dict-union b1 b2 #:combine c)))

(define-metafunction patterns
  merge-value : v v -> v or #f
  [(merge-value v v) v]
  [(merge-value C t)
   C
   (where t (uncontext C))]
  [(merge-value t C)
   C
   (where t (uncontext C))]
  [(merge-value v_1 v_2) ; else
   #f])