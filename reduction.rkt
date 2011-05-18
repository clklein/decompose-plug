#lang racket

(require redex
         "patterns.rkt"
         "syntax-directed-match.rkt")

(provide reduce
         reduction)

(define-extended-language reduction patterns
  (s a
     (:cons s s)
     (:in-hole s s)
     (:var x))
  (rr ((p s) ...)))

(define-metafunction reduction
  lookup : x b -> v
  [(lookup x ((x_1 v_1) ... (x v) (x_2 v_2) ...))
   v]
  [(lookup x ((x_1 v_1) ...)) ; binding not found
   x])

(define-metafunction reduction
  eval : s b -> v
  [(eval a b) a]
  [(eval (:cons s_1 s_2) b)
   ((left t) C)
   (where C (eval s_1 b))
   (where t (eval s_2 b))]
  [(eval (:cons s_1 s_2) b)
   ((right t) C)
   (where t (eval s_1 b))
   (where C (eval s_2 b))]
  [(eval (:cons s_1 s_2) b)
   (:cons t_1 t_2)
   (where t_1 (eval s_1 b))
   (where t_2 (eval s_2 b))]
  [(eval (:in-hole s_1 s_2) b)
   (plug-ctxt C_1 (eval s_2 b))
   (where C_1 (eval s_1 b))]
  [(eval (:var x) b)
   (lookup x b)])

(define-metafunction reduction
  plug-ctxt : v v -> v
  [(plug-ctxt no-context v) v]
  [(plug-ctxt ((left t) C) v)
   ((left t) C_*)
   (where C_* (plug-ctxt C v))]
  [(plug-ctxt ((left t) C) v)
   (:cons t_* t)
   (where t_* (plug-ctxt C v))]
  [(plug-ctxt ((right t) C) v)
   ((right t) C_*)
   (where C_* (plug-ctxt C v))]
  [(plug-ctxt ((right t) C) v)
   (:cons t t_*)
   (where t_* (plug-ctxt C v))])

(define-metafunction reduction
  reduce : L rr t -> (v ...)
  [(reduce L () t)
   ()]
  [(reduce L ((p s)) t)
   ((eval s b_*) ...)
   (where (b_* ...) (matches L p t))]
  [(reduce L ((p_1 s_1) (p_2 s_2) ...) t)
   ,(remove-duplicates (term (v_* ... v_** ...)))
   (where (v_* ...) (reduce L ((p_1 s_1)) t))
   (where (v_** ...) (reduce L ((p_2 s_2) ...) t))])