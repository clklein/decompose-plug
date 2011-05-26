#lang racket/base
(require redex/reduction-semantics
         "double.rkt")

(provide arith :arith 
         Λ/red :Λ/red 
         Λk/red Λneed/red Λdk/red
         arith-red
         cbv-red 
         cont-red
         cbn-red
         delim-red
         
         Σ 
         subst subst-1 subst-A)

(define-double-language arith :arith 
  (a (+ a a) number)
  (C (+ C a) (+ a C) hole))

(define arith-red
  (reduction-relation
   arith
   (--> (in-hole C (+ number_1 number_2))
        (in-hole C (Σ number_1 number_2)))))
 
(define-double-language Λ/red :Λ/red
  (e (e e) x v)
  (x variable-not-otherwise-mentioned)
  (y x)
  (v (λ (x) e) |+1| number)
  (E (E e) (v E) hole))

(define cbv-red
  (reduction-relation 
   Λ/red
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst e (x v)))
        "βv")
   (--> (in-hole E (|+1| number))
        (in-hole E (Σ number 1))
        "+1")))

(define-extended-language Λk/red Λ/red
  (e .... call/cc (cont (hide-hole E)))
  (v .... call/cc (cont (hide-hole E))))


(define cont-partial-red
  (reduction-relation 
   Λk/red
   (--> (in-hole E (call/cc v))
        (in-hole E (v (cont E)))
        "call/cc")
   (--> (in-hole E_1 ((cont E_2) v))
        (in-hole E_2 v)
        "cont")))

(define cont-red
  (union-reduction-relations cont-partial-red
                             (extend-reduction-relation cbv-red Λk/red)))

(define-extended-language Λneed/red Λ/red
  (E hole 
     (E e)
     ((λ (x) E) e)
     ((λ (x) (in-hole E x)) E)
     (|+1| E))
  (A v ((λ (x) A) e))
  (v number (λ (x) e) |+1|))

(define cbn-red
  (reduction-relation
   Λneed/red
   (--> (in-hole E (|+1| number))
        (in-hole E (Σ number 1))
        "+1")
   (--> (in-hole E_1 ((λ (x) (in-hole E_2 x)) v))
        (in-hole E_1 ((λ (x) (in-hole E_2 v)) v))
        "deref")
   (--> (in-hole E (((λ (x) A) e_1) e_2))
        (in-hole E ((λ (x_2) ((subst A (x x_2)) e_2)) e_1))
        (fresh x_2)
        "lift")
   (--> (in-hole E_1 ((λ (x) (in-hole E_2 x)) ((λ (y) A) e)))
        (in-hole E_1 ((λ (y_2) ((λ (x) (in-hole E_2 x)) 
                                (subst A (y y_2)))) 
                      e))
        (fresh y_2)
        "assoc")))

(define-extended-language Λdk/red Λk/red
  (e .... (|#| e) call/comp)
  (v .... call/comp)
  (E (E e) (v E) (|+1| E) hole)
  (M hole (in-hole M (|#| E))))

(define delim-red
  (reduction-relation
   Λdk/red
   (--> (in-hole M (|#| (in-hole E (call/comp v))))
        (in-hole M (|#| (in-hole E (v (cont E))))))))


(define-metafunction Λneed/red
  subst-A : A -> e
  [(subst-A ((λ (x) A) e))
   (subst (subst-A A) (x e))]
  [(subst-A v) v])

(define-metafunction Λk/red
  [(Σ number ...)
   ,(foldr + 0 (term (number ...)))])

;; substitution function is like to the one here
;;    http://redex.racket-lang.org/lam-v.html
;; except that subst-n is called subst and
;; that subst is called subst-1

(define-metafunction Λk/red
  subst : e (x any) ... -> e
  [(subst e (x_1 any_1) (x_2 any_2) ...)
   (subst-1 x_1 any_1 (subst e (x_2 any_2) ...))]
  [(subst e) e])

(define-metafunction Λk/red
  subst-1 : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst-1 x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
   (λ (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1)
                                (term (x_2 ...)))))]
  ;; 2. general purpose capture avoiding case
  [(subst-1 x_1 any_1 (λ (x_2 ...) any_2))
   (λ (x_new ...) 
     (subst-1 x_1 any_1
              (subst-vars (x_2 x_new) ... 
                          any_2)))
   (where (x_new ...)
          ,(variables-not-in
            (term (x_1 any_1 any_2)) 
            (term (x_2 ...))))]
  ;; 3. replace x_1 with e_1
  [(subst-1 x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst-1 x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst-1 x_1 any_1 (any_2 ...))
   ((subst-1 x_1 any_1 any_2) ...)]
  [(subst-1 x_1 any_1 any_2) any_2])

(define-metafunction Λk/red
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...)) 
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) 
   (subst-vars (x_1 any_1) 
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

