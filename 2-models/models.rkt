#lang racket/base
(require racket/match
         redex/reduction-semantics
         (for-syntax racket/base)
         "double.rkt")

(provide arith :arith 
         Λ/red :Λ/red 
         Λneed/red :Λneed/red
         Λk/red :Λk/red
         Λdk/red :Λdk/red Λk/red/no-hide-hole
         wacky :wacky
         wacky-inside-out :wacky-inside-out
         
         arith-red :arith-red
         cbv-red :cbv-red
         cont-red :cont-red
         cbn-red :cbn-red
         delim-red
         cont-plus-red :cont-plus-red
         cont-pair-red :cont-pair-red
         
         Σ 
         subst subst-1 subst-A)

(define-double-language arith :arith 
  (a (+ a a) number)
  (C (+ C a) (+ a C) hole))

(define-values (arith-red :arith-red)
  (double-reduction-relation
   arith :arith ([Σ (λ (x) (:Σ x))])
   (--> (in-hole C (+ number_1 number_2))
        (in-hole C (Σ number_1 number_2)))))
 
(define-double-language Λ/red :Λ/red
  (e (e e) x v)
  (x variable-not-otherwise-mentioned)
  (y x)
  (v (λ (x) e) |+1| number)
  (E (E e) (v E) hole))

(define :subst
  (match-lambda
    [`(,_ ,e (,_ (,_ ,x (,_ ,v empty)) empty))
     (term (subst ,e (,x ,v)))]))

(define-values (cbv-red :cbv-red)
  (double-reduction-relation 
   Λ/red :Λ/red ([Σ (λ (x) (:Σ x))]
                 [subst :subst])
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst e (x v)))
        "βv")
   (--> (in-hole E (|+1| number))
        (in-hole E (Σ number 1))
        "+1")))

(define-double-extended-language Λneed/red Λ/red :Λneed/red :Λ/red
  (E hole (|+1| E) (E e)
     ((λ (x) E) e)
     ((λ (x) (in-hole E x)) E))
  (A v ((λ (x) A) e))
  (v number (λ (x) e) |+1|))

(define-values (cbn-red :cbn-red)
  (double-reduction-relation
   Λneed/red :Λneed/red ([Σ (λ (x) (:Σ x))] [subst :subst])
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

(define-syntax-rule 
  (define-double-extended-language/hide-hole a b c d e 
    prods ...)
  (begin
    (define-double-extended-language a b c d
      prods ...)
    (remove-hide-hole
     (define-extended-language e b
       prods ...))))

(define-double-extended-language/hide-hole Λk/red Λ/red :Λk/red :Λ/red Λk/red/no-hide-hole
  (v .... call/cc (cont (hide-hole E))))


(define-values (cont-partial-red :cont-partial-red)
  (double-reduction-relation 
   Λk/red :Λk/red ()
   ;; no labels on these rules so the figure fits in the paper
   (--> (in-hole E (call/cc v))
        (in-hole E (v (cont E))))
   (--> (in-hole E_1 ((cont E_2) v))
        (in-hole E_2 v))))

(define cont-red
  (union-reduction-relations cont-partial-red
                             (extend-reduction-relation cbv-red Λk/red)))
(define :cont-red
  (union-red-rels :cont-partial-red
                  (reinterp-red-rel :cbv-red :Λk/red)))

(define-double-extended-language Λdk/red Λk/red :Λdk/red :Λk/red
  (e .... (|#| e) call/comp)
  (v .... call/comp)
  (E (E e) (v E) hole)
  (M E (in-hole M (|#| E))))

(define delim-red
  (reduction-relation
   Λdk/red
   (--> (in-hole M (|#| (in-hole E (call/comp v))))
        (in-hole M (|#| (in-hole E (v (cont E))))))))

(define-double-language wacky :wacky
  (C (in-hole C (f hole)) hole))
(define-double-language wacky-inside-out :wacky-inside-out
  (C (f C) hole))

(define-values (cont-plus-red :cont-plus-red)
  (let-values ([(cont-plus-partial-red :cont-plus-partial-red)
                (double-reduction-relation 
                 Λk/red :Λk/red ()
                 (--> (in-hole E (call/cc v))
                      (in-hole E (v (cont (|+1| E)))))
                 (--> (in-hole E_1 ((cont E_2) v))
                      (in-hole E_2 v)))])
    (values (union-reduction-relations cont-plus-partial-red
                                       (extend-reduction-relation cbv-red Λk/red))
            (union-red-rels :cont-plus-partial-red
                            (reinterp-red-rel :cbv-red :Λk/red)))))

(define-double-extended-language Λkp/red Λk/red :Λkp/red :Λk/red
  (e .... (cons e e))
  (v .... call/cc car cdr (cons v v))
  (E (E e) (v E) (cons E e) (cons v E) hole))

(define-values (cont-pair-red :cont-pair-red)
  (let-values ([(cont-pair-partial-red :cont-pair-partial-red)
                (double-reduction-relation 
                 Λkp/red :Λkp/red ()
                 (--> (in-hole E (call/cc v))
                      (in-hole E (v (cons (cont E) (cont (in-hole E E))))))
                 (--> (in-hole E_1 ((cont E_2) v))
                      (in-hole E_2 v))
                 (--> (in-hole E (car (cons v_1 v_2)))
                      (in-hole E v_1))
                 (--> (in-hole E (cdr (cons v_1 v_2)))
                      (in-hole E v_2)))])
    (values (union-reduction-relations cont-pair-partial-red
                                       (extend-reduction-relation cbv-red Λkp/red))
            (union-red-rels :cont-pair-partial-red
                            (reinterp-red-rel :cbv-red :Λkp/red)))))

(define-metafunction Λneed/red
  subst-A : A -> e
  [(subst-A ((λ (x) A) e))
   (subst (subst-A A) (x e))]
  [(subst-A v) v])

(define-metafunction Λk/red
  [(Σ number ...)
   ,(foldr + 0 (term (number ...)))])
(define :Σ
  (match-lambda
    [`(:cons ,m (:cons ,n empty))
     (+ m n)]))

;; substitution function is like to the one here
;;    http://redex.racket-lang.org/lam-v.html
;; except that subst-n is called subst and
;; that subst is called subst-1

(define-metafunction Λk/red
  subst : any (x any) ... -> any
  [(subst any (x_1 any_1) (x_2 any_2) ...)
   (subst-1 x_1 any_1 (subst any (x_2 any_2) ...))]
  [(subst any) any])

(define-extended-language subst-lang Λk/red
  (k :left :right :cons))
(define-metafunction subst-lang
  subst-1 : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst-1 x_1 any_1 (λ (x_1) any_2))
   (λ (x_1) any_2)]
  [(subst-1 x_1 any_1 (k_1 λ (k_2 (k_3 x_1 empty) (k_4 any_2 empty))))
   (k_1 λ (k_2 (k_3 x_1 empty) (k_4 any_2 empty)))]
  ;; 2. general purpose capture avoiding case
  [(subst-1 x_1 any_1 (λ (x_2) any_2))
   (λ (x_new) 
     (subst-1 x_1 any_1
              (subst-vars (x_2 x_new) 
                          any_2)))
   (where x_new
          ,(variable-not-in
            (term (x_1 any_1 any_2)) 
            (term x_2)))]
  [(subst-1 x_1 any_1 (k_1 λ (k_2 (k_3 x_2 empty) (k_4 any_2 empty))))
   (k_1 λ (k_2 (k_3 x_new empty) 
               (subst-1 x_1 any_1
                        (subst-vars (x_2 x_new) 
                                    (k_4 any_2 empty)))))
   (where x_new
          ,(variable-not-in
            (term (x_1 any_1 any_2)) 
            (term x_2)))]
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