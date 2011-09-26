#lang racket/base
(require racket/runtime-path)

(define files
  '("semantics/non-syntax-directed-match-test.rkt"
    "semantics/syntax-directed-match-test.rkt"
    "semantics/syntax-directed-match-total-test.rkt"
    "semantics/set-comp-test.rkt"
    "semantics/reduction-test.rkt"
    "aplas2011/2-models/test.rkt"))

(define-runtime-path here ".")

(for ([file (in-list files)])
  (printf "running ~s\n" file)
  (dynamic-require (build-path here file) #f))
