#lang racket

(require (except-in redex plug)
         "patterns.rkt"
         "syntax-directed-match-total.rkt")

(provide reductions)

(define-extended-language reduction patterns
  (s a
     (:cons s s)
     (:in-hole s s)
     (:var x)
     (:app f s))
  (f (side-condition any_1 (procedure? (term any_1)))))

(define-metafunction reduction
  inst : s b -> v
  [(inst a b) a]
  [(inst (:cons s_1 s_2) b)
   ((left t) C)
   (where C (inst s_1 b))
   (where t (inst s_2 b))]
  [(inst (:cons s_1 s_2) b)
   ((right t) C)
   (where t (inst s_1 b))
   (where C (inst s_2 b))]
  [(inst (:cons s_1 s_2) b)
   (:cons t_1 t_2)
   (where t_1 (inst s_1 b))
   (where t_2 (inst s_2 b))]
  [(inst (:in-hole s_1 s_2) b)
   (plug C_1 (inst s_2 b))
   (where C_1 (inst s_1 b))]
  [(inst (:var x_i) ([x_0 v_0] ... [x_i v_i] [x_i+1 v_i+1] ...))
   v_i]
  [(inst (:app f s) b)
   ,((term f) (term (non-context (inst s b))))])

(define-metafunction reduction
  plug : v v -> v
  [(plug no-context v) v]
  [(plug ((left t) C_1) C_2)
   ((left t) (plug C_1 C_2))]
  [(plug ((left t_1) C) t_2)
   (:cons (plug C t_2) t_1)]
  [(plug ((right t) C_1) C_2)
   ((right t) (plug C_1 C_2))]
  [(plug ((right t_1) C) t_2)
   (:cons t_1 (plug C t_2))])

(define (reductions language rules to-reduce)
  (remove-duplicates
   (append-map 
    (match-lambda
      [(list p s)
       (map (λ (b) (term (non-context (inst ,s ,b))))
            (matches language p to-reduce))])
    rules)))