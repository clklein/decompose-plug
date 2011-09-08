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
         inst plug no-ctxts non-ctxt)

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
  [(inst (:app f r) b)
   (meta-app f (inst r b))]
  [(inst (:in-hole r_1 r_2) b)
   t
   (judgment-holds (plug (inst r_1 b) (inst r_2 b) t))]
  [(inst (:cons r_1 r_2) b)
   (:cons (inst r_1 b) (inst r_2 b))])

(define-judgment-form reduction
  #:mode (plug I I O)
  #:contract (plug t t t)
  
  [(plug :hole t t)]
  
  [(plug (:left C_l t_r) C (:left t_l t_r))
   (plug C_l C t_l)]
  [(plug (:left C_l t_r) t (:cons t_l t_r))
   (plug C_l t t_l)
   (non-ctxt t)]
  
  [(plug (:right t_l C_r) C (:right t_l t_r))
   (plug C_r C t_r)]
  [(plug (:right t_l C_r) t (:cons t_l t_r))
   (plug C_r t t_r)
   (non-ctxt t)]
  
  [(plug (:cons t_l t_r) t (:cons t_l^′ t_r))
   (plug t_l t t_l^′)
   (no-ctxts t_r)]
  [(plug (:cons t_l t_r) t (:cons t_l t_r^′))
   (plug t_r t t_r^′)
   (no-ctxts t_l)])

(define-judgment-form reduction
  #:mode (no-ctxts I)
  #:contract (no-ctxts t)
  [(no-ctxts a)]
  [(no-ctxts (:cons t_1 t_2))
   (no-ctxts t_1)
   (no-ctxts t_2)])

(define-judgment-form reduction
  #:mode (non-ctxt I)
  #:contract (non-ctxt t)
  [(non-ctxt a)]
  [(non-ctxt (:cons t_1 t_2))])

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