#lang racket

(require "reduction.rkt"
         redex)

(define-syntax (test-reductions stx)
  (syntax-case stx ()
    [(_ language relation to-reduce expected)
     #`(let ([actual (reductions `language `relation `to-reduce)])
         #,(syntax/loc stx
             (test-equal (alphabetical actual) (alphabetical `expected))))]))
(define (alphabetical xs)
  (sort xs string<=? #:key (λ (x) (format "~s" x))))

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
                 (aa (:cons :hole aa)))
(test-reductions ((a (aa)))
                 (((:in-hole (:name x :hole) (:name a (:nt a)))
                   (:cons (:var a) (:var x))))
                 aa
                 ((:cons aa :hole)))

(test-reductions ()
                 ([a (:cons a a)])
                 a
                 ((:cons a a)))

(test-reductions ()
                 ([(:in-hole (:name x (:cons :hole b)) a)
                   (:in-hole (:var x) (:var x))])
                 (:cons a b)
                 ((:cons (:cons :hole b) b)))
(test-reductions ()
                 ([(:in-hole (:name x (:cons a :hole)) b)
                   (:in-hole (:var x) (:var x))])
                 (:cons a b)
                 ((:cons a (:cons a :hole))))

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
                 ((:cons :hole 2)))
(test-results)