#lang racket
(require redex/reduction-semantics)

(provide Λk Λk/red red subst subst-1 Σ
         any-which-way-Λk
         any-which-way-red)

;; TAG: cont
(define-language Λk
  (e (e e ...)
     x
     (λ (x ...) e)
     call/cc
     +
     number)
  (x variable-not-otherwise-mentioned))

;; TAG: cont/red
(define-extended-language
  Λk/red Λk
  (e .... (cont (hide-hole E)))
  (v (λ (x ...) e)
     call/cc
     +
     number
     (cont (hide-hole E)))
  (E (v ... E e ...)
     hole))

;; TAG: red
(define red
  (reduction-relation 
   Λk/red
   (--> (in-hole E (call/cc v))
        (in-hole E (v (cont E)))
        "call/cc")
   (--> (in-hole E_1 ((cont E_2) v))
        (in-hole E_2 v)
        "cont")
   (--> (in-hole E ((λ (x ..._1) e) v ..._1))
        (in-hole E (subst e (x v) ...))
        "βv")
   (--> (in-hole E (+ number ...))
        (in-hole E (Σ number ...))
        "+")))

;; TAG: Σ
(define-metafunction Λk/red
  [(Σ number ...)
   ,(foldr + 0 (term (number ...)))])


;; TAG: any-which-way-lang
(define-extended-language
  any-which-way-Λk Λk/red
  (E (e ... E e ...)
     hole))

;; TAG: any-which-way-red
(define any-which-way-red
  (extend-reduction-relation 
   red any-which-way-Λk))

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

