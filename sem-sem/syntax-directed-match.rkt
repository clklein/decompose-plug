#lang racket/base

(require redex/reduction-semantics
         racket/list
         "patterns.rkt"
         "common.rkt"
         "set-comp.rkt")

(provide (all-defined-out))

(define-extended-language directed-matching patterns
  (d (pair C t) •)
  (m (pair d b)))

(define-metafunction directed-matching
  matches : L p t -> (b ...)
  [(matches L p t)
   ,(set-comp* (term (b (in (pair • b) (M L p t)))))])

(define-metafunction directed-matching
  M : L p t -> (m ...)
  [(M L :hole :hole)
   (set (pair (pair :no-ctxt :hole) (no-bindings)) (pair • (no-bindings)))]
  [(M L :hole t) ; t ≠ :hole
   (set (pair (pair :no-ctxt t) (no-bindings)))]
  [(M L a a) ; a ≠ :hole
   (set (pair • (no-bindings)))]
  [(M L (:name x p) t)
   ,(set-comp* (term ((pair d b_^’) (guard (neq b_^’ ⊤)) 
                                    (eq b_^’ (⊔ (set (pair x (named d t))) b))
                                    (in (pair d b) (M L p t)))))]
  [(M L (:nt n) t)
   ,(set-comp* (term ((pair d (no-bindings)) (in (pair d b) (M L p t)) (in p (productions L n)))))]
  [(M L (:in-hole p_c p_h) t)
   ,(set-comp* (term ((pair d b) (eq d (combine C d_h))
                                 (guard (neq b ⊤))
                                 (eq b (⊔ b_c b_h))
                                 (in (pair d_h b_h) (M L p_h t_c))
                                 (in (pair (pair C t_c) b_c) (M L p_c t)))))]
  [(M L (:cons p_l p_r) (:cons t_l t_r))
   ,(set-comp* (term ((pair d b) (in d (select t_l d_l t_r d_r))
                                 (guard (neq b ⊤))
                                 (eq b (⊔ b_l b_r))
                                 (in (pair d_r b_r) (M L p_r t_r))
                                 (in (pair d_l b_l) (M L p_l t_l)))))]
  [(M L p t) ; else 
   (set)])

(define-metafunction directed-matching
  named : d t -> v
  [(named • t) t]
  [(named (pair C t) u) C])

(define-metafunction directed-matching
  select : t d t d -> (d) or ()
  [(select t • u •)
   (set •)]
  [(select t (pair C t_^’) u •)
   (set (pair (:left u C) t_^’))]
  [(select t • u (pair C u_^’))
   (set (pair (:right t C) u_^’))]
  [(select t (pair C t_^’) u (pair C_^’ u_^’))
   (set)])

(define-metafunction directed-matching
  combine : C d -> d
  [(combine C •) •]
  [(combine C_1 (pair C_2 t))
   (pair (append-contexts C_1 C_2) t)])

(define-syntax set-comp*
  (syntax-rules (term)
    [(_ (term (stuff ...)))
     (set-comp directed-matching stuff ...)]))

;; for typesetting
(define-metafunction patterns
  [(set any ...) (any ...)])