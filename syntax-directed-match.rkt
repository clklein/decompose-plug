#lang racket

(require redex
         "patterns.rkt"
         (rename-in "set-comp.rkt" [set-comp :set-comp]))
(provide matches)

(define-extended-language directed-matching patterns
  (d (C t)
     •)
  (m (d b)))

(define-metafunction directed-matching
  matches : L p t -> (b ...)
  [(matches L p t)
   ,(set-comp b (in (• b) (M L p t)))])

(define-metafunction directed-matching
  M : L p t -> (m ...)
  [(M L :hole :hole)
   (set (((no-context) :hole) (no-bindings)) (• (no-bindings)))]
  [(M L :hole t) ; t ≠ :hole
   (set (((no-context) t) (no-bindings)))]
  [(M L a a) ; a ≠ :hole
   (set (• (no-bindings)))]
  [(M L (:name x p) t)
   ,(set-comp (d b_^’) (guard (neq b_^’ #f)) (eq b_^’ (⊓ (set (x (named d t))) b))
                       (in (d b) (M L p t)))]
  [(M L (:nt x) t)
   ,(set-comp (d (no-bindings)) (in (d b) (M L p t)) (in p (productions L x)))]
  [(M L (:in-hole p_c p_h) t)
   ,(set-comp (d b) (eq d (combine C d_h))
                    (guard (neq b #f))
                    (eq b (⊓ b_c b_h))
                    (in (d_h b_h) (M L p_h t_c))
                    (in ((C t_c) b_c) (M L p_c t)))]
  [(M L (:cons p_l p_r) (:cons t_l t_r))
   ,(set-comp (d b) (in d (select t_l d_l t_r d_r))
                    (guard (neq b #f))
                    (eq b (⊓ b_l b_r))
                    (in (d_r b_r) (M L p_r t_r))
                    (in (d_l b_l) (M L p_l t_l)))]
  [(M L p t) ; else 
   (set)])

(define-metafunction directed-matching
  named : d t -> v
  [(named • t) t]
  [(named (C t) u) C])

(define-metafunction directed-matching
  select : t d t d -> (d) or ()
  [(select t • u •)
   (set •)]
  [(select t (C t_^’) u •)
   (set ((: (left u) C) t_^’))]
  [(select t • u (C u_^’))
   (set ((: (right t) C) u_^’))]
  [(select t (C t_^’) u (C_^’ u_^’))
   (set)])

(define-metafunction directed-matching
  combine : C d -> d
  [(combine C •) •]
  [(combine C_1 (C_2 t))
   ((append-contexts C_1 C_2) t)])

(define-metafunction directed-matching
  ⊓ : b b -> b or #f
  [(⊓ () b)
   b]
  [(⊓ ([x_0 v_0] [x_1 v_1] ...) b)
   (⊓ ([x_1 v_1] ...) b_1)
   (where b_1 (merge-binding x_0 v_0 b))]
  [(⊓ b_1 b_2) ; else
   #f])

(define-metafunction directed-matching
  merge-binding : x v b -> b or #f
  [(merge-binding x v ())
   ([x v])]
  [(merge-binding x v ([x v_0] [x_1 v_1] ...))
   ([x v_m] [x_1 v_1] ...)
   (where v_m (merge-value v v_0))]
  [(merge-binding x v ([x_0 v_0] [x_1 v_1] ...))
   ([x_0 v_0] [x_1’ v_1’] ...)
   (side-condition (not (equal? (term x) (term x_0))))
   (where ([x_1’ v_1’] ...) (merge-binding x v ([x_1 v_1] ...)))]
  [(merge-binding x v b) ; else
   #f])

(define-metafunction directed-matching
  merge-value : v v -> v or #f
  [(merge-value v v) v]
  [(merge-value C t)
   C
   (where t (uncontext C))]
  [(merge-value t C)
   C
   (where t (uncontext C))]
  [(merge-value v_1 v_2) ; else
   #f])

;; metafunctions to facilitate typesetting
(define-metafunction directed-matching
  [(set any ...) (any ...)])
(define-metafunction directed-matching
  [(neq any_1 any_1) #f]
  [(neq any_!_1 any_!_1) #t])
(define-metafunction directed-matching
  [(no-context) ,'no-context])
(define-metafunction directed-matching
  [(no-bindings) ()])
(define-metafunction directed-matching
  [(productions (n_0 ... [x_i (p ...)] n_i+1 ...) x_i)
   (p ...)])
(define-metafunction directed-matching
  [(left t) (,'left t)])
(define-metafunction directed-matching
  [(right t) (,'right t)])
(define-metafunction directed-matching
  [(: F C) (F C)])

(define-syntax-rule (set-comp subforms ...)
  (:set-comp directed-matching subforms ...))