#lang racket

(require "non-syntax-directed-match.rkt"
         "shared-test-cases.rkt"
         "common.rkt"
         redex/reduction-semantics)

(define (all-matches lang pat term)
  (remove-duplicates (judgment-holds (matches ,lang ,term ,pat b) b)))

(define test-non-syntax-directed
  (match-lambda
    [(test:match _ L p t)
     (not (empty? (all-matches L p t)))]
    [(test:no-match _ L p t)
     (empty? (all-matches L p t))]
    [(test:bind _ L p t bs)
     (equal-bindings? (map raw-bindings (all-matches L p t)) bs)]))

(run-tests test-non-syntax-directed)