#lang racket

(require racklog
         redex
         unstable/dict
         unstable/debug
         "patterns.rkt")

(provide (all-defined-out))

(define-syntax-rule (relation [vars conc prem ...] ...)
  (let ([r %empty-rel])
    (%assert! r vars [conc prem ...]) ...
    r))

(define matches ; (L t p b)
  (relation
   [(L a) ; atom
    (L a a empty)
    (is-atom a)]
   [(L t x p b) ; name
    (L t `(:name ,x ,p) (cons (cons x t) b))
    (matches L t p b)]
   [(L t x p ps b) ; non-terminal
    (L t `(:nt ,x) empty)
    (productions L x ps)
    (%member p ps)
    (matches L t p b)]
   [(L t1 t2 p1 p2 b1 b2 b) ; cons
    (L `(:cons ,t1 ,t2) `(:cons ,p1 ,p2) b)
    (matches L t1 p1 b1)
    (matches L t2 p2 b2)
    (merges b1 b2 b)]
   [(L t1 C t2 p1 p2 b1 b2 b) ; in-hole
    (L t1 `(:in-hole ,p1 ,p2) b)
    (decomposes L t1 C t2 p1 b1)
    (matches L t2 p2 b2)
    (merges b1 b2 b)]))

(define decomposes ; (L t C t p b)
  (relation
   [(L t) ; hole
    (L t 'no-context t ':hole empty)]
   [(L t1 C t2 x p t b) ; name
    (L t1 C t2 `(:name ,x ,p) (cons (cons x t) b))
    (decomposes L t1 C t2 p b)
    (%is/nonvar (C) t (uncontext/proc C))]
   [(L t1 t2 C t p1 p2 b1 b2 b) ; cons-left
    (L `(:cons ,t1 ,t2) `((left ,t2) ,C) t `(:cons ,p1 ,p2) b)
    (decomposes L t1 C t p1 b1)
    (matches L t2 p2 b2)
    (merges b1 b2 b)]
   [(L t1 t2 C t p1 p2 b1 b2 b) ; cons-right
    (L `(:cons ,t1 ,t2) `((right ,t1) ,C) t `(:cons ,p1 ,p2) b)
    (matches L t1 p1 b1)
    (decomposes L t2 C t p2 b2)
    (merges b1 b2 b)]
   [(L t t1 t2 C C1 C2 p1 p2 b1 b2 b) ; in-hole
    (L t C t2 `(:in-hole ,p1 ,p2) b)
    (decomposes L t C1 t1 p1 b1)
    (decomposes L t1 C2 t2 p2 b2)
    (%is/nonvar (C1 C2) C (append-contexts/proc C1 C2))
    (merges b1 b2 b)]
   [(L t C u x b p ps) ; non-terminal
    (L t C u `(:nt ,x) empty)
    (productions L x ps)
    (%member p ps)
    (decomposes L t C u p b)]))

(define productions
  (relation
   [(x L ps)
    (L x ps)
    (%is/nonvar (x L) ps (car (dict-ref L x)))]))

(define merges
  (relation
   [(b1 b2 b)
    (b1 b2 b)
    (%is/nonvar (b1 b2) b (merge-bindings b1 b2))
    (%/= b false)]))

(define (uncontext/proc C)
  (term (uncontext ,C)))
(define (append-contexts/proc C D)
  (term (append-contexts ,C ,D)))

(define is-atom
  (let ([a? (redex-match patterns a)])
    (λ (x)
      (%is/nonvar (x) true (and (a? x) true)))))

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