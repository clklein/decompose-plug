#lang racket

(require redex
         "patterns.rkt")
(provide match-top
         directed-matching)

(define-extended-language directed-matching patterns
  (d (C t)
     no-decomp)
  (m (d b)))

(define-metafunction directed-matching
  match-top : L p t -> (b ...)
  [(match-top L p t)
   (b_0 ...)
   (where ((no-decomp b_0) ...) (non-decompositions (match L p t)))])

(define-metafunction directed-matching
  non-decompositions : (m ...) -> (m ...)
  [(non-decompositions ())
   ()]
  [(non-decompositions (((C_0 :hole) b_0) m_1 ...))
   ((no-decomp b_0) m_1’ ...)
   (where (m_1’ ...) (non-decompositions (m_1 ...)))]
  [(non-decompositions ((no-decomp b_0) m_1 ...))
   ((no-decomp b_0) m_1’ ...)
   (where (m_1’ ...) (non-decompositions (m_1 ...)))]
  [(non-decompositions (((C_0 t_0) b_0) m_1 ...)) ; t_0 ≠ :hole
   (non-decompositions (m_1 ...))])

(define-metafunction directed-matching
  match : L p t -> (m ...)
  [(match L :hole t)
   (((no-frame t) ()))]
  [(match L a a) ; a ≠ :hole
   ((no-decomp ()))]
  [(match L (:name x p) t)
   ((d ([x (named d t)] [x_0 v_0] ...)) ...)
   (where ((d ([x_0 v_0] ...)) ...) (match L p t))]
  [(match (name L (n_0 ... [x_i (p_0 ...)] n_i+1 ...)) (:nt x_i) t)
   (no-dups (concat ((d_0 ()) ...) ...))
   (where (((d_0 b_0) ...) ...) ((match L p_0 t) ...))]
  [(match L (:in-hole p_c p_h) t)
   (concat (in-hole-merge m_0 (match L p_h t_0)) ...)
   (where ((name m_0 ((C_0 t_0) b_0)) ...) (decompositions (match L p_c t)))]
  [(match L (:cons p_1 p_2) (:cons t_1 t_2))
   (cons-merge/many-many t_1 (match L p_1 t_1)
                         t_2 (match L p_2 t_2))]
  [(match L p t) ; else 
   ()])

(define-metafunction directed-matching
  named : d t -> v
  [(named no-decomp t) t]
  [(named (C t) u) C])

(define-metafunction directed-matching
  decompositions : (m ...) -> (m ...)
  [(decompositions ())
   ()]
  [(decompositions (((C_0 t_0) b_0) m_1 ...))
   (((C_0 t_0) b_0) m_1 ...)
   (where (m_1 ...) (decompositions (m_1 ...)))]
  [(decompositions ((no-decomp b_0) m_1 ...))
   (decompositions (m_1 ...))])

(define-metafunction directed-matching
  in-hole-merge : ((C t) b) (m ...) -> (m ...)
  [(in-hole-merge ((C t) b) ())
   ()]
  [(in-hole-merge ((C_0 t_0) b_0) ((d_1 b_1) m_2 ...))
   (((merge-decomps C_0 t_0 d_1) b) m ...)
   (where b (merge-bindings b_0 b_1))
   (where (m ...) (in-hole-merge ((C_0 t_0) b_0) (m_2 ...)))]
  [(in-hole-merge ((C_0 t_0) b_0) ((d_1 b_1) m_2 ...))
   (in-hole-merge ((C_0 t_0) b_0) (m_2 ...))
   (where #f (merge-bindings b_0 b_1))])

(define-metafunction directed-matching
  merge-decomps : C t d -> d
  [(merge-decomps C t no-decomp)
   no-decomp]
  [(merge-decomps C_1 t_1 (C_2 t_2))
   ((append-contexts C_1 C_2) t_2)])

(define-metafunction directed-matching
  append-contexts : C C -> C
  [(append-contexts no-frame C) 
   C]
  [(append-contexts (F C_1) C_2)
   (F (append-contexts C_1 C_2))])

(define-metafunction directed-matching
  cons-merge/many-many : t (m ...) t (m ...) -> (m ...)
  [(cons-merge/many-many t_1 () t_2 (m ...))
   ()]
  [(cons-merge/many-many t_1 (m_0 m_1 ...) t_2 (m_i ...))
   (concat (cons-merge/one-many t_1 m_0 t_2 (m_i ...))
           (cons-merge/many-many t_1 (m_1 ...) t_2 (m_i ...)))])

(define-metafunction directed-matching
  cons-merge/one-many : t m t (m ...) -> (m ...)
  [(cons-merge/one-many t m u ())
   ()]
  [(cons-merge/one-many t m_0 u (m_1 m_2 ...))
   (concat (cons-merge/one-one t m_0 u m_1)
           (cons-merge/one-many t m_0 u (m_2 ...)))])

(define-metafunction directed-matching
  cons-merge/one-one : t m t m -> (m ...)
  [(cons-merge/one-one t_1 (d_1 b_1) t_2 (d_2 b_2))
   ()
   (where #f (merge-bindings b_1 b_2))]
  [(cons-merge/one-one t_1 (d_1 b_1) t_2 (d_2 b_2))
   ((d b) ...)
   (where b (merge-bindings b_1 b_2))
   (where (d ...) (select-decomp t_1 d_1 t_2 d_2))])

(define-metafunction directed-matching
  select-decomp : t d t d -> (d ...)
  [(select-decomp t no-decomp u no-decomp)
   (no-decomp)]
  [(select-decomp t (C t_0) u no-decomp)
   ((((left u) C) t_0))]
  [(select-decomp t no-decomp u (C u_0))
   ((((right t) C) u_0))]
  [(select-decomp t (C_t :hole) u (C_u :hole))
   ((((left u) C_t) :hole)
    (((right t) C_u) :hole))]
  [(select-decomp t (C_t t_0) u (C_u :hole)) ; t_0 ≠ :hole
   ((((left u) C_t) t_0))]
  [(select-decomp t (C_t :hole) u (C_u u_0)) ; t_0 ≠ :hole
   ((((right t) C_u) u_0))]
  [(select-decomp t (C_t t_0) u (C_u u_0)) ; t_0 ≠ :hole ∧ u_0 ≠ :hole
   ()])

(define-metafunction directed-matching
  merge-bindings : b b -> b or #f
  [(merge-bindings () b)
   b]
  [(merge-bindings ([x_0 v_0] [x_1 v_1] ...) b)
   (merge-bindings ([x_1 v_1] ...) b_1)
   (where b_1 (merge-binding x_0 v_0 b))]
  [(merge-bindings b_1 b_2) ; else
   #f])

(define-metafunction directed-matching
  merge-binding : x v b -> b or #f
  [(merge-binding x v ())
   ([x v])]
  [(merge-binding x v ([x v] [x_1 v_1] ...))
   ([x v] [x_1 v_1] ...)]
  [(merge-binding x v ([x_0 v_0] [x_1 v_1] ...))
   ([x_0 v_0] [x_1’ v_1’] ...)
   (side-condition (not (equal? (term x) (term x_0))))
   (where ([x_1’ v_1’] ...) (merge-binding x v ([x_1 v_1] ...)))]
  [(merge-binding x v b) ; else
   #f])

(define-metafunction directed-matching
  concat : (any ...) ... -> (any ...)
  [(concat)
   ()]
  [(concat (any_0 ...) any ...)
   (any_0 ... any_i ...)
   (where (any_i ...) (concat any ...))])

(define-metafunction directed-matching
  no-dups : (any ...) -> (any ...)
  [(no-dups ()) ()]
  [(no-dups (any_0 any_1 ... any_0 any_i ...))
   (no-dups (any_0 any_1 ... any_i ...))]
  [(no-dups (any_0 any_1 ...))
   (any_0 any_1’ ...)
   (side-condition (not (member (term any_0) (term (any_1 ...)))))
   (where (any_1’ ...) (no-dups (any_1 ...)))])