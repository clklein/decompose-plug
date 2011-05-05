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
     (remove-duplicates bindings)]))

(define-metafunction patterns
  no-distinguished-hole : C -> t
  [(no-distinguished-hole no-frame)
   :hole]
  [(no-distinguished-hole ((left t) C))
   (:cons (no-distinguished-hole C) t)]
  [(no-distinguished-hole ((right t) C))
   (:cons t (no-distinguished-hole C))])

(define test-non-syntax-directed
  (let ([context? (redex-match patterns C)])
    (match-lambda
      [(test:match _ L p t)
       (not (empty? (all-matches L p t)))]
      [(test:no-match _ L p t)
       (empty? (all-matches L p t))]
      [(test:bind _ L p t bs)
       (equal-bindings?
        (all-matches L p t) 
        (for/list ([b bs])
          (for/list ([m b])
            (match m
              [(list x v)
               (list x
                     (if (context? v)
                         (term (no-distinguished-hole ,v))
                         v))]))))])))

(run-tests test-non-syntax-directed)

(check-equal? (%find-all (x) (merges '([a 1] [b 2]) '([a 1] [b 2]) x))
              '(((x . ([a 1] [b 2])))))
(check-equal? (%find-all (x) (merges '([a 1] [b 2]) '([a 1] [b 3]) x))
              '(#f))
(check-equal? (%find-all (x) (merges '([a 1] [b 2]) '([b 2] [a 1]) x))
              '(((x . ([a 1] [b 2])))))
(check-equal? (%find-all (x) (merges '([a 1]) '([b 2]) x))
              '(((x . ([a 1] [b 2])))))

(check-equal? (%find-all () (is-atom 'adfd))
              '(()))
(check-equal? (%find-all () (is-atom ':cons))
              '(#f))

(check-exn exn:fail? (λ () (%find-all (X) (%is/nonvar (X) 1 X))))
(check-equal? (%find-all (X) (%and (%is X 1) (%is/nonvar (X) 1 X)))
              '(((X . 1))))