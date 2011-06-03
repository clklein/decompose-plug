#lang racket/base

(require redex/reduction-semantics
         racket/match
         "patterns.rkt")
(provide (all-defined-out))

(define-metafunction patterns
  uncontext : C -> t
  [(uncontext :no-ctxt)
   :hole]
  [(uncontext (:left t C))
   (:cons (uncontext C) t)]
  [(uncontext (:right t C))
   (:cons t (uncontext C))])

(define-metafunction patterns
  append-contexts : C C -> C
  [(append-contexts :no-ctxt C)
   C]
  [(append-contexts (:left t C_1) C_2)
   (:left t (append-contexts C_1 C_2))]
  [(append-contexts (:right t C_1) C_2)
   (:right t (append-contexts C_1 C_2))])

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

(define-metafunction patterns
  non-context : v -> t
  [(non-context t) t]
  [(non-context C) (uncontext C)])

(define (⊔/proc b1 b2)
  (term (⊔ ,b1 ,b2)))

(define-metafunction patterns
  ⊔ : b b -> b or ⊤
  [(⊔ (set) b)
   b]
  [(⊔ (set (pair x_0 v_0) (pair x_1 v_1) ...) b)
   (⊔ (set (pair x_1 v_1) ...) b_1)
   (where b_1 (merge-binding x_0 v_0 b))]
  [(⊔ b_1 b_2) ; else
   ⊤])

(define-metafunction patterns
  merge-binding : x v b -> b or ⊤
  [(merge-binding x v (set))
   (set (pair x v))]
  [(merge-binding x v (set (pair x v_0) (pair x_1 v_1) ...))
   (set (pair x v_m) (pair x_1 v_1) ...)
   (where v_m (merge-value v v_0))]
  [(merge-binding x v (set (pair x_0 v_0) (pair x_1 v_1) ...))
   (set-adjoin (pair x_0 v_0) b)
   (side-condition (term (neq x x_0)))
   (where b (merge-binding x v (set (pair x_1 v_1) ...)))]
  [(merge-binding x v b) ; else
   ⊤])

(define-metafunction patterns
  merge-value : v v -> v or ⊤
  [(merge-value v v) v]
  [(merge-value C t)
   C
   (where t (uncontext C))]
  [(merge-value t C)
   C
   (where t (uncontext C))]
  [(merge-value v_1 v_2) ; else
   ⊤])

(define raw-bindings
  (match-lambda
    [`(set (pair ,xs ,vs) ...)
     (map list xs vs)]))

;; metafunctions to facilitate typesetting
(define-metafunction patterns
  [(neq any_1 any_1) #f]
  [(neq any_!_1 any_!_1) #t])
(define-metafunction patterns
  [(no-bindings) (set)])
(define-metafunction patterns
  [(productions (D_0 ... [n_i (p ...)] D_i+1 ...) n_i)
   (p ...)])
(define-metafunction patterns
  [(: F C) (F C)])
(define-metafunction patterns
  [(set-adjoin any_0 (set any_1 ...))
   (set any_0 any_1 ...)])