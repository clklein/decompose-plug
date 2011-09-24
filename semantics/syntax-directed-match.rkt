#lang racket/base

(require redex/reduction-semantics
         racket/list
         "patterns.rkt"
         "common.rkt"
         "set-comp.rkt")

(provide (all-defined-out))

(define-extended-language directed-matching patterns
  (m (pair d b))
  (d (pair C t) •))

(define-metafunction directed-matching
  matches : G p t -> (b ...)
  [(matches G p t)
   ,(set-comp* (term (b (in (pair • b) (M G p t)))))])

(define-metafunction directed-matching
  M : G p t -> (m ...)
  [(M G :hole :hole)
   (set/id (pair (pair :hole :hole) no-bindings) (pair • no-bindings))]
  [(M G :hole t) ; t ≠ :hole
   (set/id (pair (pair :hole t) no-bindings))]
  [(M G a a)
   (set/id (pair • no-bindings))]
  [(M G (:cons p_l p_r) (k t_l t_r))
   ,(set-comp* (term ((pair d b) (in k (set/id :cons :left :right)) ;; this line added to clarify typeset version
                                 (in d (select t_l d_l t_r d_r))
                                 (eq b (⊔ b_l b_r))
                                 (in (pair d_r b_r) (M G p_r t_r))
                                 (in (pair d_l b_l) (M G p_l t_l)))))]
  [(M G (:in-hole p_c p_h) t)
   ,(set-comp* (term ((pair d b) (eq d (combine C d_h))
                                 (eq b (⊔ b_c b_h))
                                 (in (pair d_h b_h) (M G p_h t_c))
                                 (in (pair (pair C t_c) b_c) (M G p_c t)))))]
  [(M G (:name x p) t)
   ,(set-comp* (term ((pair d b_^′) (eq b_^′ (⊔ (set (pair x (named d t))) b))
                                    (in (pair d b) (M G p t)))))]
  [(M G (:nt n) t)
   ,(set-comp* (term ((pair d no-bindings) (in (pair d b) (M G p t)) (in p (productions G n)))))]
  [(M G p t) ; else 
   (set/id)])

(define-metafunction directed-matching
  named : d t -> t
  [(named • t) t]
  [(named (pair C t_1) t_2) C])

(define-metafunction directed-matching
  select : t d t d -> (d) or ()
  [(select t_1 • t_2 •)
   (set/id •)]
  [(select t (pair C t_1^′) t_2 •)
   (set/id (pair (:left C t_2) t_1^′))]
  [(select t_1 • t_2 (pair C t_2^′))
   (set/id (pair (:right t_1 C) t_2^′))]
  [(select t_1 (pair C t_1^′) t_2 (pair C_^′ t_2^′))
   (set/id)])

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
  [(set/id any ...) (any ...)])