#lang racket
(require "models.rkt"
         "double.rkt"
         redex/reduction-semantics)

;; substitution tests

(test-equal (term (subst-1 x y x)) (term y))
(test-equal (term (subst-1 x y z)) (term z))
(test-equal (term (subst-1 x y (x (y z)))) (term (y (y z))))
(test-equal (term (subst-1 x y ((λ (x) x) ((λ (y1) y1) (λ (x) z)))))
            (term ((λ (x) x) ((λ (y2) y2) (λ (x) z)))))
(test-equal (term (subst-1 x y (if0 (|+1| x) x x)))
            (term (if0 (|+1| y) y y)))
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
(test-equal (term (subst-1 z * (λ (z) 1)))
            (term (λ (z) 1)))
(test-equal (term (subst-1 q (λ (x) z) (λ (z) q)))
            (term (λ (z1) (λ (x) z))))

(test-equal (term (subst ((|+1| x) y) (x 1) (y 2)))
            (term ((|+1| 1) 2)))

;; arith tests
(test-double-match arith :arith 
                   a
                   1
                   (((a 1))))
(test-double-match arith :arith 
                   a
                   blahblah
                   #f)
(test-double-match arith :arith 
                   a 
                   (+ 1 2)
                   (((a (+ 1 2)))))
(test-double-match arith :arith 
                   (in-hole C a) 
                   (+ 1 2)
                   (((a 1)
                     (C (+ hole 2)))
                    ((a 2)
                     (C (+ 1 hole)))
                    ((a (+ 1 2))
                     (C hole))))

(test-double-match Λ/red :Λ/red 
                   e 
                   ((f x) (g y))
                   (((e ((f x) (g y))))))

(test-double-match Λ/red :Λ/red 
                   (in-hole E e)
                   ((f x) (g y))
                   (((e (f x))
                     (E (hole (g y))))
                    ((e ((f x) (g y)))
                     (E hole))
                    ((e f)
                     (E ((hole x) (g y))))))

(test-double-match Λneed/red :Λneed/red
                   (in-hole E (|+1| v))
                   ((λ (x) (|+1| 1)) (|+1| 2))
                   (((E ((λ (x) hole) (|+1| 2)))
                     (v 1))))

(test-double-match Λneed/red :Λneed/red
                   (in-hole E (|+1| v))
                   ((λ (x) (|+1| x)) (|+1| 2))
                   (((E ((λ (x) (|+1| x)) hole))
                     (v 2))))

(test-double-match Λk/red :Λk/red
                   (in-hole E (call/cc v))
                   (|+1| (call/cc (λ (k) (k 2))))
                   (((E (|+1| hole))
                     (v (λ (k) (k 2))))))

(test-double-match Λk/red :Λk/red
                   (in-hole E_1 ((cont E_2) v))
                   (|+1| ((cont (|+1| hole)) 2))
                   (((E_1 (|+1| hole))
                     (E_2 (|+1| hole))
                     (v 2))))

(test-double-match Λk/red :Λk/red
                   (in-hole E (|+1| v))
                   (|+1| ((cont (|+1| hole)) (|+1| 2)))
                   (((E (|+1| ((cont (|+1| hole)) hole)))
                     (v 2))))

(test-double-match Λk/red :Λk/red
                   (f number number_2)
                   (f 1 2)
                   (((number 1)
                     (number_2 2))))


(test-double-match Λk/red :Λk/red
                   (in-hole E_1 ((cont E_2) v))
                   (|+1| (|+1| ((cont (|+1| (|+1| (|+1| hole)))) 2)))
                   (((E_1 (|+1| (|+1| hole)))
                     (E_2 (|+1| (|+1| (|+1| hole))))
                     (v 2))))

(test-double-match #f :Λdk/red
                   e
                   (|+1| (|+1| (|#| (|+1| (|+1| (|+1| (call/comp (λ (k) (k 3)))))))))
                   (((e (|+1| (|+1| (|#| (|+1| (|+1| (|+1| (call/comp (λ (k) (k 3)))))))))))))

(test-double-match #f :Λdk/red
                   (in-hole M e)
                   (|+1| 2)
                   (((M (|+1| hole))
                     (e 2))
                    ((M (hole 2))
                     (e |+1|))
                    ((M hole)
                     (e (|+1| 2)))))

(test-double-match #f :Λdk/red
                   (in-hole M (|#| (in-hole E (call/comp v))))
                   (|+1| (|+1| (|#| (|+1| (|+1| (|+1| (call/comp (λ (k) (k 3)))))))))
                   (((M (|+1| (|+1| hole)))
                     (E (|+1| (|+1| (|+1| hole))))
                     (v (λ (k) (k 3))))))
                   
(test-double-match #f :wacky
                   C
                   (f hole)
                   (((C (f hole)))))
(test-double-match #f :wacky
                   (in-hole C_1 C_2)
                   (f (f (f hole)))
                   (((C_1 (f (f (f hole))))
                     (C_2 hole))
                    ((C_1 (f (f hole)))
                     (C_2 (f hole)))
                    ((C_1 (f hole))
                     (C_2 (f (f hole))))
                    ((C_1 hole)
                     (C_2 (f (f (f hole)))))))

(test-double-match wacky-inside-out :wacky-inside-out
                   C
                   (f hole)
                   (((C (f hole)))))
(test-double-match wacky-inside-out :wacky-inside-out
                   (in-hole C_1 C_2)
                   (f (f (f hole)))
                   (((C_1 (f (f (f hole))))
                     (C_2 hole))
                    ((C_1 (f (f hole)))
                     (C_2 (f hole)))
                    ((C_1 (f hole))
                     (C_2 (f (f hole))))
                    ((C_1 hole)
                     (C_2 (f (f (f hole)))))))

;; reduction rules tests

(test-double-reduction
 cbv-red :cbv-red
 (term ((λ (x) x) 1))
 (term 1))
(test-double-reduction
 cbv-red :cbv-red
 (term ((λ (x) (λ (y) x)) 1))
 (term (λ (y) 1)))
(test-double-reduction
 cbv-red :cbv-red
 (term ((λ (x) (λ (x) x)) 1))
 (term (λ (x) x)))
(test-double-reduction
 cbv-red :cbv-red
         (term ((λ (x) (λ (y) x)) (λ (z) y)))
         (term (λ (y1) (λ (z) y))))
(test-double-reduction*
 cbv-red :cbv-red
 (term ((((λ (f) (λ (x) (λ (y) ((f x) y))))
          (λ (g) (λ (x) (g x))))
         (λ (x) x))
        2))
 (term 2))
(test-double-reduction
 cbv-red :cbv-red
 (term (|+1| 2))
 (term 3))
(test-double-reduction*
 cbv-red :cbv-red
 (term (|+1| (|+1| (|+1| (|+1| 2)))))
 (term 6))
(test-double-reduction*
 cbv-red :cbv-red
 (term (((λ (f) (λ (x) (f (f x)))) |+1|) 3))
 (term 5))

;; in this language, call/cc is not a value, but a free variable
;; so this expression doesn't reduce
(test-double-reduction
 cbv-red :cbv-red
 (term ((λ (x) x) call/cc)))

(test-double-reduction*
 cbv-red :cbv-red
          (term (((λ (f) (λ (x) (f (f x)))) |+1|) 3))
          (term 5))

(test--> cont-red
         (term (|+1| ((cont hole) 2)))
         (term 2))

(test-double-reduction*
 cont-red :cont-red
 (term (call/cc (λ (k) (|+1| (k 0)))))
 (term 0))

(test-double-reduction*
 cont-red :cont-red
 (term (|+1| (call/cc (λ (k) (k 1)))))
 (term 2))

(test-double-reduction*
 cont-red :cont-red
 (term (((λ (x) x) (call/cc (λ (k) (k 2)))) x))
 (term (2 x)))
(test-double-reduction*
 cont-red :cont-red
 (term (|+1| (|+1| (|+1| (|+1| 2)))))
 (term 6))

(test-double-reduction*
 cont-red :cont-red
 ; plugs a context that contains another context
 (term (|+1| (call/cc (λ (k1) (k1 (call/cc (λ (k2) (k2 1))))))))
 (term 2))

;; no irreducible terms reachable from here (and also a finite graph)
(test-double-reduction*
 cont-red :cont-red
 (term ((λ (x) ((call/cc call/cc) x))
        (call/cc call/cc))))
(test-double-reduction*
 cont-red :cont-red
 (term ((λ (x) (x x)) (λ (y) (y y)))))

;; arith tests
(test-double-reduction
 arith-red :arith-red
 (term (+ (+ 1 2) (+ 3 4)))
 (term (+ 3 (+ 3 4)))
 (term (+ (+ 1 2) 7)))
(test-double-reduction*
 arith-red :arith-red
 (term (+ (+ 1 2) (+ 3 4)))
 (term 10))

(define (cbn-equiv a1 a2)
  (equal? (term (subst-A ,a1))
          (term (subst-A ,a2))))
(define (subst-A-norm e)
  (term (subst-A ,e)))

(test-equal (term (subst-A ((λ (x) 1) 2)))
            (term 1))
(test-equal (term (subst-A ((λ (x) ((λ (y) 17) 2)) (|+1| 2))))
            (term 17))
(test-equal (term (subst-A ((λ (x) (λ (y) x)) 2)))
            (term (λ (y) 2)))
(test-equal (term (subst-A ((λ (x) (λ (y) ((x |+1|) y))) 2)))
            (term (λ (y1) ((2 |+1|) y1))))

;; cbn tests
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term ((λ (x) 1) (|+1| 2)))
 (term ((λ (x) 1) (|+1| 2))))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term ((λ (x) x) (|+1| 2)))
 (term ((λ (x) 3) 3)))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (|+1| (|+1| 2)))
 (term 4))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (|+1| (|+1| (|+1| (|+1| 2)))))
 (term 6))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (((λ (x) x) (λ (y) (|+1| y))) 2))
 (term 3))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (((λ (x) (λ (y) (|+1| x))) 2) 1))
 (term ((λ (x) ((λ (y) 3) 1)) 2)))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (((λ (x) (λ (y) (|+1| y))) 2) 1))
 (term ((λ (x) ((λ (y) 2) 1)) 2)))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (((λ (f) (λ (x) (f (f (f x)))))
         (λ (x) (|+1| x)))
        0))
 (term 3))
(test-double-reduction*
 #:norm subst-A-norm cbn-red :cbn-red
 (term (((λ (f) (λ (x) (f (f x)))) |+1|) 3))
 (term 5))

;; tests of delimited continuation examples

(test-double-reduction*
   #f :delim-red
   (term (|+1| (|#| (|+1| (call/comp (λ (k) (k (k 0))))))))
   (term 4))

;; tests of exotic relations discussed in section 5

(test-double-reduction*
 cont-plus-red :cont-plus-red
 (term (call/cc (λ (k) (k 0))))
 (term 1))

(test-double-reduction*
 cont-pair-red :cont-pair-red
 (term (fst (tuple (|+1| 0) (|+1| 1))))
 (term 1))

(test-double-reduction*
 cont-pair-red :cont-pair-red
 (term (snd (tuple (|+1| 0) (|+1| 1))))
 (term 2))

(test-double-reduction*
 cont-pair-red :cont-pair-red
 (term (|+1| (call/cc (λ (k) (tuple ((fst k) 0) 1)))))
 (term 1))

(test-double-reduction*
   cont-pair-red :cont-pair-red
   (term (|+1| (call/cc (λ (k) (tuple ((snd k) 0) 1)))))
   (term 2))

(test-results)