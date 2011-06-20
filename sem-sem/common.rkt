#lang racket/base

(require redex/reduction-semantics
         racket/match
         "patterns.rkt")
(provide (all-defined-out))

(define-metafunction patterns
  append-contexts : C C -> C
  [(append-contexts :hole C)
   C]
  [(append-contexts (:left C_1 t) C_2)
   (:left (group/id (append-contexts C_1 C_2)) t)]
  [(append-contexts (:right t C_1) C_2)
   (:right t (group/id (append-contexts C_1 C_2)))])

(define encode-term
  (match-lambda
    ['() 'mt]
    [(cons t u)
     `(:cons ,(encode-term t)
             ,(encode-term u))]
    [':hole ':hole]
    [(? atom? a) a]))

(define (⊔/proc b1 b2)
  (term (⊔ ,b1 ,b2)))

(define-metafunction patterns
  ⊔ : b b -> b or ⊤
  [(⊔ (set) b)
   b]
  [(⊔ (set (pair x_0 t_0) (pair x_1 t_1) ...) b)
   (⊔ (set (pair x_1 t_1) ...) b_1)
   (where b_1 (merge-binding x_0 t_0 b))]
  [(⊔ b_1 b_2) ; else
   ⊤])

(define-metafunction patterns
  merge-binding : x t b -> b or ⊤
  [(merge-binding x t (set))
   (set (pair x t))]
  [(merge-binding x_0 t_0 (set (pair x_0 t_0) (pair x_1 t_1) ...))
   (set (pair x_0 t_0) (pair x_1 t_1) ...)]
  [(merge-binding x t (set (pair x_0 t_0) (pair x_1 t_1) ...))
   (set-adjoin (pair x_0 t_0) b)
   (side-condition (term (neq x x_0)))
   (where b (merge-binding x t (set (pair x_1 t_1) ...)))]
  [(merge-binding x t b) ; else
   ⊤])

(define raw-bindings
  (match-lambda
    [`(set (pair ,xs ,ts) ...)
     (map list xs ts)]))

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
(define-metafunction patterns
  [(group/id any) any])