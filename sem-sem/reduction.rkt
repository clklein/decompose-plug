#lang racket/base

(require (except-in redex/reduction-semantics plug)
         racket/list
         racket/set
         racket/match
         "patterns.rkt"
         "common.rkt"
         (only-in "syntax-directed-match-total.rkt"
                  [matches total-matches/proc]))

(provide reduction
         reduces reductions/multi reductions*/multi
         inst join plug no-ctxts)

(define-extended-language reduction patterns
  (r a
     (:var x)
     (:app f r)
     (:in-hole r r)
     (:cons r r))
  (s (r (x ...)) ; optional freshness declarations
     r)
  (f (side-condition any_1 (procedure? (term any_1)))))

(define-metafunction reduction
  inst : r b -> t
  [(inst a b) a]
  [(inst (:var x) b)
   (lookup b x)]
  [(inst (:app f r) b)
   (meta-app f (inst r b))]
  [(inst (:in-hole r_1 r_2) b)
   (plug (inst r_1 b) (inst r_2 b))]
  [(inst (:cons r_1 r_2) b)
   (join (inst r_1 b) (inst r_2 b))])

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
  plug : C t -> t
  [(plug :hole t) t]
  [(plug (:left C_1 t) C_2)
   (:left (plug C_1 C_2) t)]
  [(plug (:left C t_1) t_2)
   (:cons (plug C t_2) t_1)]
  [(plug (:right t C_1) C_2)
   (:right t (plug C_1 C_2))]
  [(plug (:right t_1 C) t_2)
   (:cons t_1 (plug C t_2))])

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

(define-judgment-form reduction
  #:mode (matches I I I O)
  #:contract (matches G t p b)
  [(matches G t p b_i)
   (where (b_0 ... b_i b_i+1 ...)
          (total-matches G t p))])
(define-metafunction reduction
  [(total-matches G t p)
   ,(total-matches/proc (term G) (term p) (term t))])

(define-judgment-form reduction
  #:mode (reduces I I I O I)
  #:contract (reduces G t p t s)
  [(reduces G t p t_^′ r)
   (matches G t p b)
   (where t_^′ (inst r b))]
  ; Rules with freshness declarations (not shown in paper)
  [(reduces G t p t_^′ (r (x ...)))
   (matches G t p b)
   (where t_^′ (inst r (add-fresh b t (x ...))))])
(define-metafunction reduction
  [(add-fresh b t (x ...))
   ,(add-fresh/proc (term b) (term t) (term (x ...)))])

(define (reductions/multi language rules to-reduce)
  (define apply-rule
    (match-lambda
      [(list p r)
       (judgment-holds (reduces ,language ,to-reduce ,p t ,r) t)]
      [(list p r xs)
       (judgment-holds (reduces ,language ,to-reduce ,p t (,r ,xs)) t)]))
  (remove-duplicates (append-map apply-rule rules)))

(define (reductions*/multi language rules to-reduce)
  (define seen (set))
  (define irred (set))
  (let loop ([t to-reduce])
    (unless (set-member? seen t)
      (set! seen (set-add seen t))
      (define ts (reductions/multi language rules t))
      (if (empty? ts)
          (set! irred (set-add irred t))
          (for-each loop ts))))
  (set-map irred values))

(define (add-fresh/proc bindings wrt vars)
  (for/fold ([b bindings]) 
            ([x vars]
             [s (variables-not-in wrt vars)])
            (match b
              [`(set . ,others)
               `(set (pair ,x ,s) ,@others)])))