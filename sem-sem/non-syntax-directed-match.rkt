#lang racket

(require (except-in racklog atom?)
         redex/reduction-semantics
         unstable/dict
         unstable/debug
         "common.rkt"
         "patterns.rkt")

(provide (all-defined-out))

(define-syntax-rule (relation [vars conc prem ...] ...)
  (let ([r %empty-rel])
    (%assert! r vars [conc prem ...]) ...
    r))

(define matches ; (G t p b)
  (relation
   [(G a) ; atom
    (G a a '(set))
    (is-atom a)]
   [(G) ; hole
    (G ':hole ':hole '(set))]
   [(G t x p b b+) ; name
    (G t `(:name ,x ,p) b+)
    (matches G t p b)
    (merges `(set (pair ,x ,t)) b b+)]
   [(G t x ps p b) ; non-terminal
    (G t `(:nt ,x) '(set))
    (%is/nonvar (G x) ps (productions/proc G x))
    (%member p ps)
    (matches G t p b)]
   [(G k t1 t2 p1 p2 b1 b2 b) ; cons
    (G `(,k ,t1 ,t2) `(:cons ,p1 ,p2) b)
    (is-constructor k)
    (matches G t1 p1 b1)
    (matches G t2 p2 b2)
    (merges b1 b2 b)]
   [(G t1 C t2 p1 p2 b1 b2 b) ; in-hole
    (G t1 `(:in-hole ,p1 ,p2) b)
    (decomposes G t1 C t2 p1 b1)
    (matches G t2 p2 b2)
    (merges b1 b2 b)]))

(define decomposes ; (G t C t p b)
  (relation
   [(G t) ; hole
    (G t ':hole t ':hole '(set))]
   [(G t1 C t2 x p t b b+) ; name
    (G t1 C t2 `(:name ,x ,p) b+)
    (decomposes G t1 C t2 p b)
    (merges `(set (pair ,x ,C)) b b+)]
   [(G k t1 t2 C t p1 p2 b1 b2 b) ; cons-left
    (G `(,k ,t1 ,t2) `(:left ,C ,t2) t `(:cons ,p1 ,p2) b)
    (is-constructor k)
    (decomposes G t1 C t p1 b1)
    (matches G t2 p2 b2)
    (merges b1 b2 b)]
   [(G k t1 t2 C t p1 p2 b1 b2 b) ; cons-right
    (G `(,k ,t1 ,t2) `(:right ,t1 ,C) t `(:cons ,p1 ,p2) b)
    (is-constructor k)
    (matches G t1 p1 b1)
    (decomposes G t2 C t p2 b2)
    (merges b1 b2 b)]
   [(G t t1 t2 C C1 C2 p1 p2 b1 b2 b) ; in-hole
    (G t C t2 `(:in-hole ,p1 ,p2) b)
    (decomposes G t C1 t1 p1 b1)
    (decomposes G t1 C2 t2 p2 b2)
    (%is/nonvar (C1 C2) C (append-contexts/proc C1 C2))
    (merges b1 b2 b)]
   [(G t C u x b ps p) ; non-terminal
    (G t C u `(:nt ,x) '(set))
    (%is/nonvar (G x) ps (productions/proc G x))
    (%member p ps)
    (decomposes G t C u p b)]))

(define (productions/proc G x)
  (term (productions ,G ,x)))

(define merges
  (relation
   [(b1 b2 b)
    (b1 b2 b)
    (%is/nonvar (b1 b2) b (⊔/proc b1 b2))
    (%/= b '⊤)]))

(define (append-contexts/proc C D)
  (term (append-contexts ,C ,D)))

(define-syntax-rule (make-matches p)
  (let ([p? (redex-match patterns p)])
    (λ (x)
      (%is/nonvar (x) true (and (p? x) true)))))
(define is-atom (make-matches a))
(define is-constructor (make-matches k))

(define-syntax (%is/nonvar stx)
  (syntax-case stx ()
    [(_ () E1 E2)
     #'(%is E1 E2)]
    [(form-name (X0 X1 ...) E1 E2)
     (identifier? #'X0)
     #'(%if-then-else
        (%nonvar X0)
        (form-name (X1 ...) E1 E2)
        (%is 'dont-care (error 'form-name "~s uninstantiated" 'X0)))]))