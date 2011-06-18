#lang racket/base

(require (except-in redex/reduction-semantics plug)
         racket/list
         racket/set
         racket/match
         "patterns.rkt"
         "common.rkt"
         "syntax-directed-match-total.rkt")

(provide reduction
         reductions reductions* reduces
         inst plug plug-ctxt no-ctxts)

(define-extended-language reduction patterns
  (r a
     (:cons r r)
     (:in-hole r r)
     (:var x)
     (:app f r))
  (f (side-condition any_1 (procedure? (term any_1)))))

(define-metafunction reduction
  inst : r b -> t
  [(inst a b) a]
  [(inst (:cons r_1 r_2) b)
   (join (inst r_1 b) (inst r_2 b))]
  [(inst (:in-hole r_1 r_2) b)
   (plug (inst r_1 b) (inst r_2 b))]
  [(inst (:var x) b)
   (lookup b x)]
  [(inst (:app f r) b)
   (meta-app f (inst r b))])

(define-metafunction reduction
  join : t t -> t
  [(join C t)
   (:left C t)
   (side-condition (term (no-ctxts t)))]
  [(join t C)
   (:right t C)
   (side-condition (term (no-ctxts t)))]
  [(join t_1 t_2)
   (:cons t_1 t_2)])

(define-metafunction reduction
  plug : t t -> t
  [(plug C t)
   (plug-ctxt C t)]
  [(plug (:cons t_1 t_2) t)
   (:cons (plug t_1 t) t_2)
   (side-condition (term (no-ctxts t_2)))]
  [(plug (:cons t_1 t_2) t)
   (:cons t_1 (plug t_2 t))
   (side-condition (term (no-ctxts t_1)))])

(define-metafunction reduction
  plug-ctxt : C t -> t
  [(plug-ctxt :hole t) t]
  [(plug-ctxt (:left C_1 t) C_2)
   (:left (plug-ctxt C_1 C_2) t)]
  [(plug-ctxt (:left C t_1) t_2)
   (:cons (plug-ctxt C t_2) t_1)]
  [(plug-ctxt (:right t C_1) C_2)
   (:right t (plug-ctxt C_1 C_2))]
  [(plug-ctxt (:right t_1 C) t_2)
   (:cons t_1 (plug-ctxt C t_2))])

(define-relation reduction
  no-ctxts ⊆ t
  [(no-ctxts a)]
  [(no-ctxts (:cons t_1 t_2))
   (no-ctxts t_1)
   (no-ctxts t_2)])

(define-metafunction reduction
  lookup : b x -> t
  [(lookup (set (pair x_0 t_0) ... (pair x_i t_i) (pair x_i+1 t_i+1) ...) x_i)
   t_i])

(define-metafunction reduction
  meta-app : f t -> t
  [(meta-app f t)
   ,((term f) (term t))])

(define (reductions language rules to-reduce)
  (remove-duplicates
   (append-map 
    (match-lambda
      [(list p r)
       (map (λ (b) (term (inst ,r ,b)))
            (matches language p to-reduce))])
    rules)))

(define (reductions* language rules to-reduce)
  (define seen (set))
  (define norms (set))
  (let loop ([t to-reduce])
    (unless (set-member? seen t)
      (set! seen (set-add seen t))
      (define ts (reductions language rules t))
      (if (empty? ts)
          (set! norms (set-add norms t))
          (for-each loop ts))))
  (set-map norms values))

(define-relation reduction
  [(reduces G t p t_^′ r)
   (matches G t p b)
   (eq (inst r b) t_^’)])
