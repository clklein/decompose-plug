#lang racket
(require "model.rkt"
         redex/reduction-semantics)

;; substitution tests

(test-equal (term (subst-1 x y x)) (term y))
(test-equal (term (subst-1 x y z)) (term z))
(test-equal (term (subst-1 x y (x (y z)))) (term (y (y z))))
(test-equal (term (subst-1 x y ((λ (x) x) ((λ (y1) y1) (λ (x) z)))))
            (term ((λ (x) x) ((λ (y2) y2) (λ (x) z)))))
(test-equal (term (subst-1 x y (if0 (+ 1 x) x x)))
            (term (if0 (+ 1 y) y y)))
(test-equal (term (subst-1 x (λ (z) y) (λ (y) x)))
            (term (λ (y1) (λ (z) y))))
(test-equal (term (subst-1 x 1 (λ (y) x)))
            (term (λ (y) 1)))
(test-equal (term (subst-1 x y (λ (y) x)))
            (term (λ (y1) y)))
(test-equal (term (subst-1 x (λ (y) y) (λ (z) (z2 z))))
            (term (λ (z1) (z2 z1))))
(test-equal (term (subst-1 x (λ (z) z) (λ (z) (z1 z))))
            (term (λ (z2) (z1 z2))))
(test-equal (term (subst-1 x z (λ (z) (z1 z))))
            (term (λ (z2) (z1 z2))))
(test-equal (term (subst-1 x3 5 (λ (x2) x2)))
            (term (λ (x1) x1)))
(test-equal (term (subst-1 z * (λ (z x) 1)))
            (term (λ (z x) 1)))
(test-equal (term (subst-1 q (λ (x) z) (λ (z x) q)))
            (term (λ (z1 x1) (λ (x) z))))
(test-equal (term (subst-1 x 1 (λ (x x) x)))
            (term (λ (x x) x)))

(test-equal (term (subst (+ x y) (x 1) (y 2)))
            (term (+ 1 2)))

;; reduction rules tests

(test--> red
         (term ((λ (x) x) 1))
         (term 1))
(test--> red
         (term ((λ (x) (λ (y) x)) 1))
         (term (λ (y) 1)))
(test--> red
         (term ((λ (x) (λ (x) x)) 1))
         (term (λ (x) x)))
(test--> red
         (term ((λ (x) (λ (y) x)) (λ (z) y)))
         (term (λ (y1) (λ (z) y))))
(test-->> red
          (term ((λ (f x y) (f x y))
                 (λ (g x) (g x))
                 (λ (x) x)
                 2))
          (term 2))
(test--> red
         (term (+ 1 2))
         (term 3))

(test--> red
         (term (+ 1 ((cont hole) 2)))
         (term 2))

(test-->> red
          (term (+ (call/cc (λ (k) (k 1))) 2))
          (term 3))

(test-->> red
          (term (+ (call/cc (λ (k) (k 2))) x))
          (term (+ 2 x)))

;; no irreducible terms reachable from here (and also a finite graph)
(test-->> red 
          #:cycles-ok
          (term ((λ (x) ((call/cc call/cc) x))
                 (call/cc call/cc))))
(test-->> red
          #:cycles-ok
          (term ((λ (x) (x x)) (λ (y) (y y)))))

;; arith tests
(test--> arith-red
         (term (+ (+ 1 2) (+ 3 4)))
         (term (+ 3 (+ 3 4)))
         (term (+ (+ 1 2) 7)))
(test-->> arith-red
          (term (+ (+ 1 2) (+ 3 4)))
          (term 10))

(test-results)



