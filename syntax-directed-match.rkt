#lang racket

(require redex)
(provide match-top patterns)

(define-language patterns
  (p a
     (:name x p)
     (:nt x)
     (:in-hole p p)
     (:cons p p))
  (h p
     #f)
  (a :hole
     variable-not-otherwise-mentioned
     number)
  (x variable-not-otherwise-mentioned)
  
  ((t u) a
         (:cons t t))
  
  (b ([x v] ...))
  (v t C)
  
  (L (n ...))
  (n [x (p ...)])
  
  (F (left t)
     (right t))
  (C no-frame
     (F C))
  
  (d (C t)
     no-decomp)
  (m (d b)))

(define-metafunction patterns
  match-top : L p t -> (b ...)
  [(match-top L p t)
   (b_0 ...)
   (where ((no-decomp b_0) ...) (non-decompositions (match L p t)))])

(define-metafunction patterns
  non-decompositions : (m ...) -> (m ...)
  [(non-decompositions ())
   ()]
  [(non-decompositions (((C_0 :hole) b_0) m_1 ...))
   ((no-decomp b_0) m_1 ...)
   (where (m_1 ...) (non-decompositions (m_1 ...)))]
  [(non-decompositions ((no-decomp b_0) m_1 ...))
   ((no-decomp b_0) m_1’ ...)
   (where (m_1’ ...) (non-decompositions (m_1 ...)))]
  [(non-decompositions (((C_0 t_0) b_0) m_1 ...)) ; t_0 ≠ :hole
   (non-decompositions (m_1 ...))])

(define-metafunction patterns
  match : L p t -> (m ...)
  [(match L :hole t)
   (((no-frame t) ()))]
  [(match L a a) ; a ≠ :hole
   ((no-decomp ()))]
  [(match L (:name x p) t)
   ((d ([x (named d t)] [x_0 u_0] ...)) ...)
   (where ((d ([x_0 u_0] ...)) ...) (match L p t))]
  [(match (name L (n_0 ... [x_i (p_0 ...)] n_i+1 ...)) (:nt x_i) t)
   (no-dups (concat ((d_0 ()) ...) ...))
   (where (((d_0 b_0) ...) ...) ((match L p_0 t) ...))]
  [(match L (:in-hole p_c p_h) t)
   (concat (decomp-merge m_0 (match L p_h t_0)) ...)
   (where ((name m_0 ((C_0 t_0) b_0)) ...) (decompositions (match L p_c t)))]
  [(match L (:cons p_1 p_2) (:cons t_1 t_2))
   (cons-merge/many-many t_1 (match L p_1 t_1)
                         t_2 (match L p_2 t_2))]
  [(match L p t) ; else 
   ()])

(define-metafunction patterns
  named : d t -> v
  [(named no-decomp t) t]
  [(named (C t) u) C])

(define-metafunction patterns
  decompositions : (m ...) -> (m ...)
  [(decompositions ())
   ()]
  [(decompositions (((C_0 t_0) b_0) m_1 ...))
   (((C_0 t_0) b_0) m_1 ...)
   (where (m_1 ...) (decompositions (m_1 ...)))]
  [(decompositions ((no-decomp b_0) m_1 ...))
   (decompositions (m_1 ...))])

(define-metafunction patterns
  decomp-merge : m (m ...) -> (m ...)
  [(decomp-merge m ())
   ()]
  [(decomp-merge (d_0 b_0) ((d_1 b_1) m_2 ...))
   ((d_1 b) m ...)
   (where b (merge-bindings b_0 b_1))
   (where (m ...) (decomp-merge (d_0 b_0) (m_2 ...)))]
  [(decomp-merge (d_0 b_0) ((d_1 b_1) m_2 ...))
   (decomp-merge (d_0 b_0) (m_2 ...))
   (where #f (merge-bindings b_0 b_1))])

(define-metafunction patterns
  cons-merge/many-many : t (m ...) t (m ...) -> (m ...)
  [(cons-merge/many-many t_1 () t_2 (m ...))
   ()]
  [(cons-merge/many-many t_1 (m_0 m_1 ...) t_2 (m_i ...))
   (concat (cons-merge/one-many t_1 m_0 t_2 (m_i ...))
           (cons-merge/many-many t_1 (m_1 ...) t_2 (m_i ...)))])

(define-metafunction patterns
  cons-merge/one-many : t m t (m ...) -> (m ...)
  [(cons-merge/one-many t m u ())
   ()]
  [(cons-merge/one-many t m_0 u (m_1 m_2 ...))
   (concat (cons-merge/one-one t m_0 u m_1)
           (cons-merge/one-many t m_0 u (m_2 ...)))])

(define-metafunction patterns
  cons-merge/one-one : t m t m -> (m ...)
  [(cons-merge/one-one t_1 (d_1 b_1) t_2 (d_2 b_2))
   ()
   (where #f (merge-bindings b_1 b_2))]
  [(cons-merge/one-one t_1 (d_1 b_1) t_2 (d_2 b_2))
   ((d b) ...)
   (where b (merge-bindings b_1 b_2))
   (where (d ...) (select-decomp t_1 d_1 t_2 d_2))])

(define-metafunction patterns
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

(define-metafunction patterns
  merge-bindings : b b -> b or #f
  [(merge-bindings 
    ([x_0 v_0] ... [x_i v_i] [x_i+1 v_i+1] ...)
    ([x_0’ v_0’] ... [x_i v_i’] [x_i+1’ v_i+1’] ...))
   ([x_i v_i] [x_0’’ v_0’’] ...)
   (side-condition (equal? (term (reify v_i))
                           (term (reify v_i’))))
   (where ([x_0’’ v_0’’] ...)
          (merge-bindings 
           ([x_0 v_0] ... [x_i+1 v_i+1] ...)
           ([x_0’ v_0’] ... [x_i+1’ v_i+1’] ...)))]
  [(merge-bindings 
    ([x_0 v_0] [x_1 v_1] ...)
    ([x_0’ v_0’] ...))
   ([x_0 v_0] [x_0’’ v_0’’] ...)
   (where ([x_0’’ v_0’’] ...)
          (merge-bindings ([x_1 v_1] ...) ([x_0’ v_0’] ...)))
   (side-condition (not (memq (term x_0) (term (x_0’ ...)))))]
  [(merge-bindings () b)
   b]
  [(merge-bindings b_0 b_1) ; else
   #f])

(define-metafunction patterns
  reify : v -> t
  [(reify t) t]
  [(reify C) (reify-context C)])

(define-metafunction patterns
  reify-context : C -> t
  [(reify-context no-frame)
   :hole]
  [(reify-context ((left t) C))
   (:cons (reify-context C) t)]
  [(reify-context ((right t) C))
   (:cons t (reify-context C))])

(define-metafunction patterns
  concat : (any ...) ... -> (any ...)
  [(concat)
   ()]
  [(concat (any_0 ...) any ...)
   (any_0 ... any_i ...)
   (where (any_i ...) (concat any ...))])

(define-metafunction patterns
  no-dups : (any ...) -> (any ...)
  [(no-dups ()) ()]
  [(no-dups (any_0 any_1 ... any_0 any_i ...))
   (no-dups (any_0 any_1 ... any_i ...))]
  [(no-dups (any_0 any_1 ...))
   (any_0 any_1’ ...)
   (side-condition (not (member (term any_0) (term (any_1 ...)))))
   (where (any_1’ ...) (no-dups (any_1 ...)))])