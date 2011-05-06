#lang racket

(require redex/reduction-semantics
         "patterns.rkt")
(provide matches decomposes)

(define-extended-language non-directed-matching patterns
  (b {}
     {[x v]}
     (∪ b b)))

(define-relation non-directed-matching
  matches ⊆ L × t × p × b
  [(matches L a a {})]
  [(matches L t (:name x p) (∪ {[x t]} b))
   (matches L t p b)]
  [(matches (name L (n_0 ... [x_i (p_0 ... p_i p_i+1 ...)] n_i+1 ...)) t (:nt x_i) {})
   (matches L t p_i b)]
  [(matches L (:cons t_1 t_2) (:cons p_1 p_2) (∪ b_1 b_2))
   (matches L t_1 p_1 b_1)
   (matches L t_2 p_2 b_2)
   (~ b_1 b_2)]
  [(matches L t_1 (:in-hole p_1 p_2) (∪ b_1 b_2))
   (decomposes L t_1 C t_2 p_1 b_1)
   (matches L t_2 p_2 b_2)
   (~ b_1 b_2)])

(define-relation non-directed-matching
  decomposes ⊆ L × t × C × t × p × b
  [(decomposes L t no-frame t :hole {})]
  [(decomposes L t_1 C t_2 (:name x p) (∪ {[x C]} b))
   (decomposes L t_1 C t_2 p b)]
  [(decomposes L (:cons t_1 t_2) ((left t_2) C) t_1’ (:cons p_1 p_2) (∪ b_1 b_2))
   (decomposes L t_1 C t_1’ p_1 b_1)
   (matches L t_2 p_2 b_2)
   (~ b_1 b_2)]
  [(decomposes L (:cons t_1 t_2) ((right t_1) C) t_1’ (:cons p_1 p_2) (∪ b_1 b_2))
   (matches L t_1 p_1 b_1)
   (decomposes L t_2 C t_2’ p_2 b_2)
   (~ b_1 b_2)]
  [(decomposes L t C t_2 (:in-hole p_1 p_2) (∪ b_1 b_2))
   (decomposes L t C_1 t_1 p_1 b_1)
   (decomposes L t_1 C_2 t_2 p_2 b_2)
   (append-contexts C_1 C_2 C)
   (~ b_1 b_2)]
  [(decomposes (name L (n_0 ... [x_i (p_0 ... p_i p_i+1 ...)] n_i+1 ...)) t_1 C t_2 (:nt x_i) {})
   (decomposes L t_1 C t_2 p_i b)])

(define-relation non-directed-matching
  ~ ⊆ b × b
  [(~ {} b)]
  [(~ {[x v]} b)
   (consistent x v b)]
  [(~ (∪ b_1 b_2) b)
   (~ b_1 b)
   (~ b_2 b)])

(define-relation non-directed-matching
  consistent ⊆ x × v × b
  [(consistent x v {})]
  [(consistent x v_1 {[x v_2]})
   ,(equal? (term (uncontext v_1)) (term (uncontext v_2)))]
  [(consistent x_1 v_1 {[x_2 v_2]})
   ,(not (equal? (term x_1) (term x_2)))]
  [(consistent x v (∪ b_1 b_2))
   (consistent x v b_1)
   (consistent x v b_2)])