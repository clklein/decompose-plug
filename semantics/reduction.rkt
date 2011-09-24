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
         inst plug join no-ctxts)

(define-extended-language reduction patterns
  (r a
     :hole
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
  [(inst :hole b) :hole]
  [(inst (:var x) b)
   (lookup b x)]
  [(inst (:in-hole r_1 r_2) b) 
   ;; WARNING: result of 'inst' not necc. a C
   (plug (inst r_1 b) (inst r_2 b))]
  [(inst (:cons r_1 r_2) b)
   (join (inst r_1 b) (inst r_2 b))]
  [(inst (:app f r) b)
   (meta-app f (inst r b))])

(define-metafunction reduction
  join : t t -> t
  [(join C t) (:left C t) (judgment-holds (no-ctxts t))]
  [(join t C) (:right t C) (judgment-holds (no-ctxts t))]
  [(join t_1 t_2) (:cons t_1 t_2)])

(define-judgment-form reduction
  #:mode (no-ctxts I)
  #:contract (no-ctxts t)
  [(no-ctxts a)]
  [(no-ctxts (:cons t_1 t_2)) (no-ctxts t_1) (no-ctxts t_2)])

(define-metafunction reduction
  plug : C t -> t
  [(plug :hole t) t]
  
  [(plug (:left C_l t_r) C) (:left (plug C_l C) t_r)]
  [(plug (:left C_l t_r) t) (:cons (plug C_l t) t_r)]
  
  [(plug (:right t_l C_r) C) (:right t_l (plug C_r C))]
  [(plug (:right t_l C_r) t) (:cons t_l (plug C_r t))])

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