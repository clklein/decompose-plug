#lang racket

(require "non-syntax-directed-match.rkt"
         "shared-test-cases.rkt"
         "patterns.rkt"
         redex
         racklog
         rackunit)

(define (all-matches lang pat term)
  (match (%find-all (b) (matches lang term pat b))
    [`(#f) empty]
    [`(((b . ,bindings)) ...)
     (remove-duplicates 
      (for/list ([binding bindings])
        (dict-map binding list)))]))

(define test-non-syntax-directed
  (match-lambda
    [(test:match _ L p t)
     (not (empty? (all-matches L p t)))]
    [(test:no-match _ L p t)
     (empty? (all-matches L p t))]
    [(test:bind _ L p t bs)
     (equal-bindings?
      (all-matches L p t) 
      (no-contexts-bindings bs))]))

(run-tests test-non-syntax-directed)

(check-equal? (%find-all (x) (merges '([a . 1] [b . 2]) '([a . 1] [b . 2]) x))
              '(((x . ([a . 1] [b . 2])))))
(check-equal? (%find-all (x) (merges '([a . 1] [b . 2]) '([a . 1] [b . 3]) x))
              '(#f))
(check-equal? (%find-all (x) (merges '([a . 1] [b . 2]) '([b . 2] [a . 1]) x))
              '(((x . ([a . 1] [b . 2])))))
(check-equal? (%find-all (x) (merges '([a . 1]) '([b . 2]) x))
              '(((x . ([a . 1] [b . 2])))))

(check-equal? (%find-all () (is-atom 'adfd))
              '(()))
(check-equal? (%find-all () (is-atom ':cons))
              '(#f))

(check-exn exn:fail? (λ () (%find-all (X) (%is/nonvar (X) 1 X))))
(check-equal? (%find-all (X) (%and (%is X 1) (%is/nonvar (X) 1 X)))
              '(((X . 1))))