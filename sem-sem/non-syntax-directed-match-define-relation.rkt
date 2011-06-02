#lang racket

(require redex/reduction-semantics
         "common.rkt"
         "patterns.rkt")
(provide matches decomposes)

(define-relation patterns
  matches ⊆ L × t × p × b
  [(matches L a a (no-bindings))]
  [(matches L t (:name x p) b_^′)
   (matches L t p b)
   (lub-not-top (set (pair x t)) b b_^′)]
  [(matches L t (:nt n) (no-bindings))
   (nt-has-prod p L n)
   (matches L t p b)]
  [(matches L (:cons t_1 t_2) (:cons p_1 p_2) b)
   (matches L t_1 p_1 b_1)
   (matches L t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(matches L t_1 (:in-hole p_1 p_2) b)
   (decomposes L t_1 C t_2 p_1 b_1)
   (matches L t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)])

(define-relation patterns
  decomposes ⊆ L × t × C × t × p × b
  [(decomposes L t :no-ctxt t :hole (no-bindings))]
  [(decomposes L t_1 C t_2 (:name x p) b_^′)
   (decomposes L t_1 C t_2 p b)
   (lub-not-top (set (pair x C)) b b_^′)]
  [(decomposes L (:cons t_1 t_2) (:left t_2 C) t_1^′ (:cons p_1 p_2) b)
   (decomposes L t_1 C t_1^′ p_1 b_1)
   (matches L t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(decomposes L (:cons t_1 t_2) (:right t_1 C) t_1^′ (:cons p_1 p_2) b)
   (matches L t_1 p_1 b_1)
   (decomposes L t_2 C t_2^′ p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(decomposes L t C t_2 (:in-hole p_1 p_2) b)
   (decomposes L t C_1 t_1 p_1 b_1)
   (decomposes L t_1 C_2 t_2 p_2 b_2)
   (concatenation-of C_1 C_2 C)
   (lub-not-top b_1 b_2 b)]
  [(decomposes L t_1 C t_2 (:nt n) (no-bindings))
   (nt-has-prod p L n)
   (decomposes L t_1 C t_2 p b)])