#lang racket

(require "non-syntax-directed-match.rkt"
         "patterns.rkt"
         racklog
         rackunit)

(define-syntax-rule (all-matches lang pat term)
  (...
   (match (%find-all (b) (matches `lang `term `pat b))
     [`(#f) empty]
     [`(((b . ,bindings)) ...)
      bindings])))

(check-equal? (all-matches () a a) '(()))
(check-equal? (all-matches () :hole :hole) '(()))
(check-equal? (all-matches () a b) '())
(check-equal? (all-matches () a (:cons b c)) '())

(check-equal? (all-matches () (:cons a b) (:cons a b)) '(()))
(check-equal? (all-matches () (:cons a b) (:cons a c)) '())
(check-equal? (all-matches () (:cons a b) (:cons c b)) '())
(check-equal? (all-matches () (:cons a b) c) '())

(check-equal? (all-matches () (:name x a) a) '(((x a))))
(check-equal? (all-matches () (:cons (:name x a) b) (:cons a b))
              '(((x a))))

(let ([ones-lang '((ones (mt (:cons 1 (:nt ones)))))])
  (check-equal? (all-matches ,ones-lang (:nt ones) mt) '(()))
  (check-equal? (all-matches ,ones-lang (:nt ones) (:cons 1 mt)) '(()))
  (check-equal? (all-matches ,ones-lang (:nt ones) (:cons 1 (:cons 1 mt))) '(())))
(check-equal? (all-matches ((1s2s (mt 
                                   (:cons (:name x 1) (:nt 1s2s))
                                   (:cons (:name x 2) (:nt 1s2s)))))
                           (:nt 1s2s)
                           (:cons 1 (:cons 2 mt)))
              '(()))

(check-equal? (all-matches () (:in-hole :hole a) a) '(()))
(check-equal? (all-matches () (:in-hole :hole a) b) '())

(check-equal? (all-matches () (:in-hole (:name x (:cons a :hole)) b) (:cons a b))
              '(((x (:cons a :hole)))))

(check-equal? (all-matches () (:in-hole (:cons :hole b) a) (:cons a b)) '(()))
(check-equal? (all-matches () (:in-hole (:cons a :hole) b) (:cons a b)) '(()))

(check-equal? (all-matches ((n (:hole))) (:in-hole (:nt n) 2) 2) '(()))
(check-equal? (all-matches ((n (:hole (:cons 1 (:nt n))))) 
                           (:in-hole (:nt n) 2)
                           (:cons 1 (:cons 1 (:cons 1 2))))
              '(()))

(check-equal? (all-matches () (:in-hole (:cons :hole :hole) :hole) (:cons :hole :hole))
              '(() ()))
(check-equal? (all-matches ((hole-or-n (:hole (:nt n)))
                            (n (1 2))) 
                           (:in-hole (:name x
                                            (:cons (:nt hole-or-n)
                                                   (:nt hole-or-n)))
                                     (:nt n))
                           (:cons 1 2))
              '(((x (:cons :hole 2)))
                ((x (:cons 1 :hole)))))

(check-equal? (all-matches () 
                           (:in-hole (:in-hole (:cons a :hole) (:cons b :hole)) c)
                           (:cons a (:cons b c)))
              '(()))
(check-equal? (all-matches () 
                           (:in-hole (:name x (:in-hole (:cons a :hole) (:cons b :hole))) c)
                           (:cons a (:cons b c)))
              '(((x (:cons a (:cons b :hole))))))

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