#lang racket
(require redex/reduction-semantics)

(provide Λ Λ/red Λk/red red subst subst-1 Σ
         arith arith-red)

(define-language arith
  (e (+ e ...) number)
  (C (+ e ... C e ...) hole))

(define arith-red
  (reduction-relation
   arith
   (--> (in-hole C (+ number ...))
        (in-hole C (Σ number ...)))))
 
(define-language Λ
  (e (e e ...)
     x
     (λ (x ...) e)
     +
     number)
  (x variable-not-otherwise-mentioned))

(define-extended-language Λ/red Λ
  (v (λ (x ...) e)
     call/cc
     +
     number)
  (E (v ... E e ...)
     hole))

;; TAG: red
(define red
  (reduction-relation 
   Λk/red
   (--> (in-hole E ((λ (x ..._1) e) v ..._1))
        (in-hole E (subst e (x v) ...))
        "βv")
   (--> (in-hole E (+ number ...))
        (in-hole E (Σ number ...))
        "+")))

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
  (union-reduction-relations cont-partial-red red))

(define-extended-language Λneed/red Λ/red
  (E hole 
     (E e)
     (λ (x_1 ... x x_2 ...) (in-hole E x))
     ((λ (x ...) E) e))
  (A v ((λ (x ...) A) e)))

(define need-red
  (--> (in-hole E (+ number ...))
       (in-hole E (Σ number ...))
       "+")

;; TAG: Σ
(define-metafunction Λk/red
  [(Σ number ...)
   ,(foldr + 0 (term (number ...)))])

;; substitution function is like to the one here
;;    http://redex.racket-lang.org/lam-v.html
;; except that subst-n is called subst and
;; that subst is called subst-1

;; TAG: subst
(define-metafunction Λk/red
  subst : e (x v) ... -> e
  [(subst e (x_1 v_1) (x_2 v_2) ...)
   (subst-1 x_1 v_1 (subst e (x_2 v_2) ...))]
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

