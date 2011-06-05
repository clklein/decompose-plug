#lang racket/base

(require (except-in redex/reduction-semantics plug)
         racket/list
         racket/match
         "patterns.rkt"
         "common.rkt"
         "syntax-directed-match-total.rkt")

(provide reduction
         reductions/func reduces
         inst plug)

(define-extended-language reduction patterns
  (r a
     (:cons r r)
     (:in-hole r r)
     (:var x)
     (:app f r))
  (f (side-condition any_1 (procedure? (term any_1)))))

(define-metafunction reduction
  inst : r b -> v
  [(inst a b) a]
  [(inst (:cons r_1 r_2) b)
   (:left t C)
   (where C (inst r_1 b))
   (where t (inst r_2 b))]
  [(inst (:cons r_1 r_2) b)
   (:right t C)
   (where t (inst r_1 b))
   (where C (inst r_2 b))]
  [(inst (:cons r_1 r_2) b)
   (:cons t_1 t_2)
   (where t_1 (inst r_1 b))
   (where t_2 (inst r_2 b))]
  [(inst (:in-hole r_1 r_2) b)
   (plug C_1 (inst r_2 b))
   (where C_1 (inst r_1 b))]
  [(inst (:var x_i) (set (pair x_0 v_0) ... (pair x_i v_i) (pair x_i+1 v_i+1) ...))
   v_i]
  [(inst (:app f r) b)
   (meta-app f (non-context (inst r b)))])

(define-metafunction reduction
  meta-app : f t -> t
  [(meta-app f t)
   ,((term f) (term t))])

(define-metafunction reduction
  plug : v v -> v
  [(plug :no-ctxt v) v]
  [(plug (:left t C_1) C_2)
   (:left t (plug C_1 C_2))]
  [(plug (:left t_1 C) t_2)
   (:cons (plug C t_2) t_1)]
  [(plug (:right t C_1) C_2)
   (:right t (plug C_1 C_2))]
  [(plug (:right t_1 C) t_2)
   (:cons t_1 (plug C t_2))])

(define (reductions/func language rules to-reduce)
  (remove-duplicates
   (append-map 
    (match-lambda
      [(list p r)
       (map (λ (b) (term (non-context (inst ,r ,b))))
            (matches language p to-reduce))])
    rules)))

(define-relation reduction
  [(reduces G t p t_^′ r)
   (matches G t p b)
   (eq (non-context (inst r b)) t_^’)])