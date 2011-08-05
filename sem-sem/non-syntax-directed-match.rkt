#lang racket/base

(require redex/reduction-semantics
         (except-in "common.rkt" :)
         "patterns.rkt")
(provide matches decomposes)

;; Visible changes (check paper text and proof)
;; 1. TODO: rewriter for lub -> ⊔ change
;; 2. TODO: are premises in OK order?

(define-judgment-form patterns
  mode : I I I O
  matches ⊆ G × t × p × b
  [(matches G a a (no-bindings))]
  [(matches G :hole :hole (no-bindings))]
  [(matches G t (:name x p) (⊔ (set (pair x t)) b))
   (matches G t p b)
   (where/hidden b_lub (⊔ (set (pair x t)) b))]
  [(matches G t (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (matches G t p b)]
  [(matches G (k t_1 t_2) (:cons p_1 p_2) (⊔ b_1 b_2))
   (matches G t_1 p_1 b_1)
   (matches G t_2 p_2 b_2)
   (k-ok k)
   (where/hidden b (⊔ b_1 b_2))]
  [(matches G t_1 (:in-hole p_1 p_2) (⊔ b_1 b_2))
   (decomposes G t_1 C t_2 p_1 b_1)
   (matches G t_2 p_2 b_2)
   (where/hidden b (⊔ b_1 b_2))])

(define-judgment-form patterns
  mode : I I O O I O
  decomposes ⊆ G × t × C × t × p × b
  [(decomposes G t :hole t :hole (no-bindings))]
  [(decomposes G (k t_1 t_2) (:left C t_2) t_1^′ (:cons p_1 p_2) (⊔ b_1 b_2))
   (decomposes G t_1 C t_1^′ p_1 b_1)
   (matches G t_2 p_2 b_2)
   (k-ok k)
   (where/hidden b (⊔ b_1 b_2))]
  [(decomposes G (k t_1 t_2) (:right t_1 C) t_2^′ (:cons p_1 p_2) (⊔ b_1 b_2))
   (matches G t_1 p_1 b_1)
   (decomposes G t_2 C t_2^′ p_2 b_2)
   (k-ok k)
   (where/hidden b (⊔ b_1 b_2))]
  [(decomposes G t_1 C t_2 (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (decomposes G t_1 C t_2 p b)]
  [(decomposes G t (group/id (append-contexts C_1 C_2)) t_2 (:in-hole p_1 p_2) (⊔ b_1 b_2))
   (decomposes G t C_1 t_1 p_1 b_1)
   (decomposes G t_1 C_2 t_2 p_2 b_2)
   (where/hidden b (⊔ b_1 b_2))]
  [(decomposes G t_1 C t_2 (:name x p) (⊔ (set (pair x C)) b))
   (decomposes G t_1 C t_2 p b)
   (where/hidden b_lub (⊔ (set (pair x C)) b))])

;; this is here for a typesetting hook, so
;; we don't have to add the k non-terminal
(define-judgment-form patterns
  mode : I
  [(k-ok k)])