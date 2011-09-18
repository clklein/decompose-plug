#lang racket

(require redex/reduction-semantics
         "set-comp.rkt")

(define-language L
  ((x y z n) any))

(test-equal (set-comp L (x y) (in x (1 2)) (in y (a b)))
            '((1 a) (2 a) (1 b) (2 b)))
(test-equal (set-comp L x (in (x n) ((a 1) (b 2))))
            '(a b))
(test-equal (set-comp L n (in (a n) ((a 1) (b 2))))
            '(1))
(test-equal (set-comp L z (eq z (y x)) (in x (1 2)) (in y (a b)))
            '((a 1) (a 2) (b 1) (b 2)))
(test-equal (let ()
              (define-metafunction L
                [(not-b b) #f]
                [(not-b any) #t])
              (set-comp L (x y) (in x (1 2)) (guard (not-b y)) (in y (a b))))
            '((1 a) (2 a)))
(test-equal (set-comp L ,(+ (term x) (term y)) (in x (1 2)) (in y (1 2)))
            '(2 3 4))