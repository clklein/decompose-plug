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
         inst plug join has-context)

(define-extended-language reduction patterns
  (r a
     :hole
     (:var x)
     (:app f r)
     (:in-hole r r)
     (:cons r r)
     (:hide-hole r))
  (s (r (x ...)) ; optional freshness declarations
     r)
  (f (side-condition any_1 (procedure? (term any_1)))))

(define-metafunction reduction
  inst : r b -> (tuple t bool)
  [(inst a b) (tuple a false)]
  [(inst :hole b) (tuple :hole true)]
  [(inst (:var x) b) (tuple (lookup b x) (has-context (lookup b x)))]
  [(inst (:in-hole r_1 r_2) b) 
   (plug C (inst r_2 b))
   ;; WARNING: result of 'inst' not necc. a C
   (where (tuple C true) (inst r_1 b))]
  [(inst (:cons r_1 r_2) b)
   (join (inst r_1 b) (inst r_2 b))]
  [(inst (:app f r) b)
   (tuple (meta-app f (inst r b)) (has-context (meta-app f (inst r b))))]
  [(inst (:hide-hole p) b)
   (tuple t false)
   (where (tuple t bool) (inst t b))])

(define-metafunction reduction
  join : (tuple t bool) (tuple t bool) -> (tuple t bool)
  [(join (tuple C true) (tuple t false)) (tuple (:left C t) true)]
  [(join (tuple t false) (tuple C true)) (tuple (:right t C) true)]
  [(join (tuple t_1 bool_1) (tuple t_2 bool_2)) (tuple (:cons t_1 t_2) (∨ bool_1 bool_2))])

(define-metafunction reduction
  ∨ : bool bool -> bool
  [(∨ false false) false]
  [(∨ bool bool) true])

(define-metafunction reduction
  plug : C (tuple t bool) -> (tuple t bool)
  [(plug :hole (tuple t bool)) (tuple t bool)]
  
  [(plug (:left C_l t_r) (tuple C_c true)) 
   (tuple (:left t_l t_r) true)
   (where (tuple C_c true) (plug C_c C))]
  [(plug (:left C_l t_r) (tuple t bool_1)) 
   (tuple (:cons t_l t_r) (has-context t_r))
   (where (tuple t_l bool_2) (plug C_l (tuple t bool_1)))]
  ;[(plug (:left C_l t_r) C) (:left (plug C_l C) t_r)]
  ;[(plug (:left C_l t_r) t) (:cons (plug C_l t) t_r)]
  
  [(plug (:right t_l C_r) (tuple C bool)) (tuple (:right t_r (plug C_r C)) bool)]
  [(plug (:right t_l C_r) (tuple t bool)) (tuple (:cons t_l (plug C_r t)) bool)]
  ;[(plug (:right t_l C_r) C) (:right t_l (plug C_r C))]
  ;[(plug (:right t_l C_r) t) (:cons t_l (plug C_r t))]
  )

(define-metafunction reduction
  has-context : t -> bool
  [(has-context a) false]
  [(has-context (:cons t_1 t_2)) (∨ (has-context t_1) (has-context t_2))]
  [(has-context t) true])

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