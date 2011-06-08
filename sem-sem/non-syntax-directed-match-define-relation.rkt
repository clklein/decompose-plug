#lang racket

(require redex/reduction-semantics
         "common.rkt"
         "patterns.rkt")
(provide matches decomposes)

(define-relation patterns
  matches ⊆ G × t × p × b
  [(matches G a a (no-bindings))]
  [(matches G t (:name x p) b_^′)
   (matches G t p b)
   (lub-not-top (set (pair x t)) b b_^′)]
  [(matches G t (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (matches G t p b)]
  [(matches G (:cons t_1 t_2) (:cons p_1 p_2) b)
   (matches G t_1 p_1 b_1)
   (matches G t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(matches G t_1 (:in-hole p_1 p_2) b)
   (decomposes G t_1 C t_2 p_1 b_1)
   (matches G t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)])

(define-relation patterns
  decomposes ⊆ G × t × C × t × p × b
  [(decomposes G t :no-ctxt t :hole (no-bindings))]
  [(decomposes G (:cons t_1 t_2) (:left t_2 C) t_1^′ (:cons p_1 p_2) b)
   (decomposes G t_1 C t_1^′ p_1 b_1)
   (matches G t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(decomposes G (:cons t_1 t_2) (:right t_1 C) t_1^′ (:cons p_1 p_2) b)
   (matches G t_1 p_1 b_1)
   (decomposes G t_2 C t_2^′ p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(decomposes G t_1 C t_2 (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (decomposes G t_1 C t_2 p b)]
  [(decomposes G t ((append-contexts C_1 C_2)) t_2 (:in-hole p_1 p_2) b)
   (decomposes G t C_1 t_1 p_1 b_1)
   (decomposes G t_1 C_2 t_2 p_2 b_2)
   (lub-not-top b_1 b_2 b)]
  [(decomposes G t_1 C t_2 (:name x p) b_^′)
   (decomposes G t_1 C t_2 p b)
   (lub-not-top (set (pair x C)) b b_^′)])