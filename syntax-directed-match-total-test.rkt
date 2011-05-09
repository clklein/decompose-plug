#lang racket

(require "shared-test-cases.rkt"
         "patterns.rkt"
         "syntax-directed-match-total.rkt"
         rackunit)

(define test-syntax-directed-total
  (match-lambda
    [(test:match _ L p t)
     (not (empty? (matches L p t)))]
    [(test:no-match _ L p t)
     (empty? (matches L p t))]
    [(test:bind _ L p t bs)
     (equal-bindings? 
      (matches L p t)
      (no-contexts-bindings bs))]))

(run-tests test-syntax-directed-total)

(let ([W? (λ (t)
            (not
             (empty?
              (matches '([W (:hole (:in-hole (:nt W) (:cons :hole 1)))])
                       '(:nt W)
                       t))))])
  (check-true (W? ':hole))
  (check-true (W? '(:cons :hole 1)))
  (check-true (W? '(:cons (:cons :hole 1) 1)))
  (check-false (W? '(:cons 1 (:cons :hole 1)))))

(let ([A? (λ (t)
            (not
             (empty?
              (matches '([L (:hole (:in-hole (:cons (:nt L) e) (:cons λ :hole)))]
                         [A (:hole (:in-hole (:nt L) (:nt A)))])
                       '(:nt A)
                       t))))])
  (check-true (A? ':hole))
  (check-true (A? '(:cons (:cons (:cons λ (:cons λ :hole)) e) e)))
  (check-false (A? '(:cons (:cons (:cons λ :hole) e) e)))
  (check-false (A? '(:cons (:cons λ (:cons λ :hole)) e)))
  (check-true (A? '(:cons (:cons λ (:cons (:cons (:cons λ (:cons λ :hole)) e) e)) e)))
  (check-false (A? '(:cons (:cons λ (:cons (:cons λ (:cons λ :hole)) e)) e)))
  (check-false (A? '(:cons (:cons (:cons λ (:cons (:cons λ (:cons λ :hole)) e)) e) e))))