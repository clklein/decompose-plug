#lang racket

(require "non-syntax-directed-match.rkt"
         "shared-test-cases.rkt"
         "common.rkt"
         redex/reduction-semantics
         racklog
         rackunit)

(define (all-matches lang pat term)
  (match (%find-all (b) (matches lang term pat b))
    [`(#f) empty]
    [`(((b . ,bindings)) ...)
     (remove-duplicates bindings)]))

(define test-non-syntax-directed
  (match-lambda
    [(test:match _ L p t)
     (not (empty? (all-matches L p t)))]
    [(test:no-match _ L p t)
     (empty? (all-matches L p t))]
    [(test:bind _ L p t bs)
     (equal-bindings? (map raw-bindings (all-matches L p t)) bs)]))

(run-tests test-non-syntax-directed)

(check-equal? (%find-all () (is-atom 'adfd))
              '(()))
(check-equal? (%find-all () (is-atom ':cons))
              '(#f))

(check-exn exn:fail? (λ () (%find-all (X) (%is/nonvar (X) 1 X))))
(check-equal? (%find-all (X) (%and (%is X 1) (%is/nonvar (X) 1 X)))
              '(((X . 1))))