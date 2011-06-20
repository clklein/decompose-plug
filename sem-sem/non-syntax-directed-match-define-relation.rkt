#lang racket

(require redex/reduction-semantics
         "common.rkt"
         "patterns.rkt")
(provide matches decomposes)

(define-relation patterns
  matches ⊆ G × t × p × b
  [(matches G a a (no-bindings))]
  [(matches G :hole :hole (no-bindings))]
  [(matches G t (:name x p) (lub (set (pair x t)) b))
   (matches G t p b)]
  [(matches G t (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (matches G t p b)]
  [(matches G (k t_1 t_2) (:cons p_1 p_2) (lub b_1 b_2))
   (matches G t_1 p_1 b_1)
   (matches G t_2 p_2 b_2)
   (k-ok k)]
  [(matches G t_1 (:in-hole p_1 p_2) (lub b_1 b_2))
   (decomposes G t_1 C t_2 p_1 b_1)
   (matches G t_2 p_2 b_2)])

(define-relation patterns
  decomposes ⊆ G × t × C × t × p × b
  [(decomposes G t :hole t :hole (no-bindings))]
  [(decomposes G (k t_1 t_2) (:left C t_2) t_1^′ (:cons p_1 p_2) (lub b_1 b_2))
   (decomposes G t_1 C t_1^′ p_1 b_1)
   (matches G t_2 p_2 b_2)
   (k-ok k)]
  [(decomposes G (k t_1 t_2) (:right t_1 C) t_2^′ (:cons p_1 p_2) (lub b_1 b_2))
   (matches G t_1 p_1 b_1)
   (decomposes G t_2 C t_2^′ p_2 b_2)
   (k-ok k)]
  [(decomposes G t_1 C t_2 (:nt n) (no-bindings))
   (nt-has-prod p G n)
   (decomposes G t_1 C t_2 p b)]
  [(decomposes G t ((append-contexts C_1 C_2)) t_2 (:in-hole p_1 p_2) (lub b_1 b_2))
   (decomposes G t C_1 t_1 p_1 b_1)
   (decomposes G t_1 C_2 t_2 p_2 b_2)]
  [(decomposes G t_1 C t_2 (:name x p) (lub (set (pair x C)) b))
   (decomposes G t_1 C t_2 p b)])

;; this is here for a typesetting hook, so
;; we don't have to add the k non-terminal
(define-metafunction patterns
  [(k-ok cons) #t]
  [(k-ok left) #t]
  [(k-ok right) #t]
  [(k-ok any) #f])
