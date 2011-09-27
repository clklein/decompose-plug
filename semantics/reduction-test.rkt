#lang racket

(require "reduction.rkt"
         (except-in redex/reduction-semantics plug))

(test-equal (term (inst a (set))) (term (tuple a :false)))
(test-equal (term (inst :hole (set))) (term (tuple :hole :true)))
(test-equal (term (inst (:var x) (set (pair x z)))) (term (tuple z :false)))
(test-equal (term (inst (:var x) (set (pair x :hole)))) (term (tuple :hole :true)))
(test-equal (term (inst (:cons x y) (set))) (term (tuple (:cons x y) :false)))
(test-equal (term (inst (:cons :hole y) (set))) (term (tuple (:left :hole y) :true)))
(test-equal (term (inst (:in-hole (:cons :hole z) (:cons y :hole)) (set))) 
            (term (tuple (:left (:right y :hole) z) :true)))
(test-equal (term (inst (:hide-hole (:cons :hole x)) (set)))
            (term (tuple (:left :hole x) :false)))
(test-equal (term (inst (:in-hole (:cons (:cons :hole x) (:hide-hole (:cons y :hole)))
                                  z)
                        (set)))
            (term (tuple (:cons (:cons z x) (:right y :hole))
                         :true)))

(test-equal (term (inst (:in-hole (:in-hole (:var x) (:var y)) b)
                        (set (pair x (:left :hole :hole))
                             (pair y (:right a :hole)))))
            (term (tuple (:cons (:cons a b) :hole) :true)))

(test-equal (term (inst (:cons (:cons (:var x) (:var y))
                               (:var z))
                        (set (pair x :hole)
                             (pair y :hole)
                             (pair z :hole))))
            (term (tuple (:cons (:cons :hole :hole) :hole)
                         :true)))


(define-syntax-rule (define-reduction-test-form name reduce)
  (define-syntax (name stx)
    (syntax-case stx ()
      [(_ language relation to-reduce expected)
       #`(let ([actual (reduce `language `relation `to-reduce)])
           #,(syntax/loc stx
               (test-equal (alphabetical actual) (alphabetical `expected))))])))
(define (alphabetical xs)
  (sort xs string<=? #:key (λ (x) (format "~s" x))))

(define-reduction-test-form test-reductions reductions/multi)
(define-reduction-test-form test-reductions* reductions*/multi)

(define bool-lang
  '((B (true
        false
        (:cons and (:cons (:nt B) (:nt B)))
        (:cons or (:cons (:nt B) (:nt B)))))
    (C ((:cons and (:cons (:nt C) (:nt B)))
        (:cons or (:cons (:nt C) (:nt B)))
        :hole))))

(define bool-rr
  '(((:in-hole (:name C (:nt C)) (:cons and (:cons false (:name B (:nt B)))))
     (:in-hole (:var C) false))
    ((:in-hole (:name C (:nt C)) (:cons and (:cons true (:name B (:nt B)))))
     (:in-hole (:var C) (:var B)))
    ((:in-hole (:name C (:nt C)) (:cons or (:cons false (:name B (:nt B)))))
     (:in-hole (:var C) (:var B)))
    ((:in-hole (:name C (:nt C)) (:cons or (:cons true (:name B (:nt B)))))
     (:in-hole (:var C) true))))

(test-reductions ,bool-lang
                 ,bool-rr
                 true
                 ())

(test-reductions ,bool-lang
                 ,bool-rr
                 false
                 ())
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons and (:cons true true))
                 (true))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons and (:cons false true))
                 (false))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons and (:cons true (:cons and (:cons true true))))
                 ((:cons and (:cons true true))))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons and (:cons false  (:cons and (:cons true true))))
                 (false))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons or (:cons true true))
                 (true))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons or (:cons false true))
                 (true))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons or (:cons true (:cons and (:cons true true))))
                 (true))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons or (:cons false (:cons and (:cons true true))))
                 ((:cons and (:cons true true))))
(test-reductions ,bool-lang
                 ,bool-rr
                 (:cons and (:cons (:cons or (:cons true false)) false))
                 ((:cons and (:cons true false))))

(test-reductions ((a (aa)))
                 (((:in-hole (:name x :hole) (:name a (:nt a)))
                   (:cons (:var x) (:var a)))
                  ((:name a (:nt a))
                   (:var a)))
                 aa
                 (aa (:left :hole aa)))
(test-reductions ((a (aa)))
                 (((:in-hole (:name x :hole) (:name a (:nt a)))
                   (:cons (:var a) (:var x))))
                 aa
                 ((:right aa :hole)))

(test-reductions ()
                 ([a (:cons a a)])
                 a
                 ((:cons a a)))

(test-reductions ()
                 ([(:in-hole (:name x (:cons :hole b)) a)
                   (:in-hole (:var x) (:var x))])
                 (:cons a b)
                 ((:left (:left :hole b) b)))
(test-reductions ()
                 ([(:in-hole (:name x (:cons a :hole)) b)
                   (:in-hole (:var x) (:var x))])
                 (:cons a b)
                 ((:right a (:right a :hole))))

(test-reductions ([C (:hole (:cons (:nt C) mt))]
                  [n (1 (:cons (:nt n) mt))]) 
                 ([(:in-hole (:name C (:nt C)) (:nt n)) 2])
                 (:cons 1 mt)
                 (2))

(test-reductions ([n (1 2 3 4 5)]) 
                 ([(:cons (:name x (:nt n)) (:name y (:nt n)))
                    (:app ,(match-lambda [`(:cons ,x ,y) (+ x y)])
                          (:cons (:var x) (:var y)))])
                 (:cons 1 3)
                 (4))
(test-reductions () 
                 ([(:in-hole (:name x (:cons :hole 2)) 1)
                    (:app ,values (:var x))])
                 (:cons 1 2)
                 ((:left :hole 2)))

(test-reductions ()
                 ([(:cons :variable :variable)
                   (:cons (:var x) (:var x1))
                   (x x1)])
                 (:cons x x1)
                 ((:cons x2 x3)))

(test-reductions ([n (1 2 3)])
                 ([(:cons (:name n_1 (:nt n)) (:name n_2 (:nt n)))
                   (:cons (:var n_1) :hole)])
                 (:cons 1 2)
                 ((:right 1 :hole)))

(test-reductions* ()
                  ([a b] [a e] [b c] [c c] [c d])
                  a
                  (d e))
(test-reductions* ()
                  ([a a])
                  a
                  ())
(test-results)