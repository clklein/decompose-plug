#lang racket

(require racklog
         redex
         "patterns.rkt")

(define-syntax-rule (relation [vars conc prem ...] ...)
  (let ([r %empty-rel])
    (%assert! r vars [conc prem ...]) ...
    r))

(define matches
  (relation
   []))

(define decomposes
  (relation))

(define ++
  (relation))

(define ~
  (relation))

(define ~1
  (relation))

(define is-atom
  (let ([a? (redex-match patterns a)])
    (λ (x)
      (%is true (a? x)))))