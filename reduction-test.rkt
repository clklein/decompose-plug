#lang racket

(require "reduction.rkt"
         redex)

(define-syntax (test-reduction stx)
  (syntax-case stx ()
    [(_ language relation to-reduce expected)
     #`(let ([actual (term (reduce language relation to-reduce))])
         #,(syntax/loc stx
             (test-equal (apply set actual) (apply set `expected))))]))

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

(test-reduction ,bool-lang
                ,bool-rr
                true
                ())
(test-reduction ,bool-lang
                ,bool-rr
                false
                ())
(test-reduction ,bool-lang
                ,bool-rr
                (:cons and (:cons true true))
                (true))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons and (:cons false true))
                (false))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons and (:cons true (:cons and (:cons true true))))
                ((:cons and (:cons true true))))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons and (:cons false  (:cons and (:cons true true))))
                (false))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons or (:cons true true))
                (true))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons or (:cons false true))
                (true))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons or (:cons true (:cons and (:cons true true))))
                (true))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons or (:cons false (:cons and (:cons true true))))
                ((:cons and (:cons true true))))
(test-reduction ,bool-lang
                ,bool-rr
                (:cons and (:cons (:cons or (:cons true false)) false))
                ((:cons and (:cons true false))))

(define a-lang
  '((a (aa))))

(define a-rr
  '(((:in-hole (:name x :hole) (:name a (:nt a)))
     (:cons (:var x) (:var a)))
    ((:name a (:nt a))
     (:var a))))

(test-reduction ,a-lang
                ,a-rr
                aa
                (aa ((left aa) no-context)))

(test-results)