#lang racket

(require redex
         "shared-test-cases.rkt"
         (prefix-in syn-dir: "syntax-directed-match.rkt")
         (prefix-in syn-dir-total: "syntax-directed-match-total.rkt")
         (only-in "non-syntax-directed-match-test.rkt"
                  all-matches
                  raw-bindings
                  no-contexts-bindings))

(define-language match-grammar
  (pat a
       (:cons pat pat)
       (:in-hole (:name x pat-hole) (:name x pat))
       (:name x (:nt A))
       (:name x (:nt B))
       (:name x (:nt C))
       (:name x (:nt D)))
  (pat-hole
   :hole
   (:cons pat-hole pat)
   (:cons pat pat-hole)
   (:in-hole (:name x pat-hole) (:name x pat-hole)))
  (a :hole
     literal)
  (literal variable-not-otherwise-mentioned
           number)
  (x variable-not-otherwise-mentioned)
  ; non-terminals for random grammar
  (A-pat a
         (:cons A-pat A-pat)
         (:name x (:in-hole (:name x A-pat-hole) (:name x A-pat)))
         (:name x (:nt B))
         (:name x (:nt C))
         (:name x (:nt D)))
  (A-pat-hole
   :hole
   (:cons A-pat-hole A-pat)
   (:cons A-pat A-pat-hole)
   (:in-hole (:name x A-pat-hole) (:name x A-pat-hole)))
  (B-pat a
         (:cons B-pat B-pat)
         (:name x (:in-hole (:name x B-pat-hole) (:name x B-pat)))
         (:name x (:nt C))
         (:name x (:nt D)))
  (B-pat-hole
   :hole
   (:cons B-pat-hole B-pat)
   (:cons B-pat B-pat-hole)
   (:in-hole (:name x B-pat-hole) (:name x B-pat-hole)))
  (C-pat a
         (:cons C-pat C-pat)
         (:name x (:in-hole (:name x C-pat-hole) (:name x C-pat)))
         (:name x (:nt D)))
  (C-pat-hole
   :hole
   (:cons C-pat-hole C-pat)
   (:cons C-pat C-pat-hole)
   (:in-hole (:name x C-pat-hole) (:name x C-pat-hole)))
  (D-pat a
         (:cons D-pat D-pat)
         (:name x (:in-hole (:name x D-pat-hole) (:name x D-pat))))
  (D-pat-hole
   :hole
   (:cons D-pat-hole D-pat)
   (:cons D-pat D-pat-hole)
   (:in-hole (:name x D-pat-hole) (:name x D-pat-hole)))
  
  ; random grammar w/ 4 non-terminals
  (lang ((A (A-pat A-pat ...)) (B (B-pat B-pat ...)) (C (C-pat C-pat ...)) (D (D-pat D-pat ...)))))

; pat -> pattern
(define (toredex t)
  (define state '())
  (define (get-fresh-name)
    (let ((x (variable-not-in state 'x)))
      (set! state (cons x state))
      x))
  (define (prep t)
    (match t
      [':hole
       'hole]
      [`(:name ,x ,p)
       `(name ,(get-fresh-name) ,(prep p))]
      [`(:in-hole ,p1 ,p2)
       `(in-hole ,(prep p1) ,(prep p2))]
      [`(:cons ,p1 ,p2)
       `(,(prep p1) ,(prep p2))]
      [`(:nt ,N)
       N]
      [_
       t]))
  (prep t))

; pattern -> pat
(define (fromredex t)
  (define (prep t)
    (match t
      [(? (λ (x) (or (struct? x) (equal? 'hole x))))
       ':hole]
      #;[`(,p1)
         `(:cons ,(prep p1) ())]
      [`(,p1 ,p2)
       `(:cons ,(prep p1) ,(prep p2))]
      [`(in-hole ,p1 ,p2)
       `(:in-hole ,(prep p1) ,(prep p2))]
      [`(name ,n ,p)
       `(:name ,(prep n) ,(prep p))]
      ['A
       '(:nt A)]
      ['B
       '(:nt B)]
      ['C
       '(:nt C)]
      ['D
       '(:nt D)]
      [_
       t]))
  (prep t))

(define test-size 2)

(define namespace
  (let ([n (make-base-namespace)])
    (parameterize ([current-namespace n])
      (namespace-require 'redex/reduction-semantics))
    n))

(define (make-term-generator language pattern size)
  (parameterize ([current-namespace namespace])
    (eval `(define-language L ,@language))
    (eval `(let ([g (generate-term L ,pattern)]
                 [n 0])
             (λ ()
               (set! n (+ 1 n))
               (g ,size #:attempt-num n))))))

(define (nt-to-redex nt)
  (match nt
    [`(,N ,(list pat ...))
     `(,N ,@(map toredex pat))]))

(define (nt-from-redex nt)
  (match nt
    [`(,N ,@(list pat ...))
     `(,N ,(map fromredex pat))]))

(define (test)
  (let* ((pat (toredex (generate-term match-grammar pat test-size)))
         (red-lang (map nt-to-redex (generate-term match-grammar lang test-size)))
         (gen (make-term-generator red-lang pat test-size))
         (lang (map nt-from-redex red-lang))
         (p (fromredex pat))
         (t (fromredex (gen)))
         (syn-dir (no-contexts-bindings (map raw-bindings (term (syn-dir:matches ,lang ,p ,t)))))
         (syn-dir-total (no-contexts-bindings (map raw-bindings (syn-dir-total:matches lang p t))))
         (non-syn-dir (no-contexts-bindings (map raw-bindings (all-matches lang p t)))))
    (unless
        (and (equal-bindings? non-syn-dir syn-dir)
             (equal-bindings? syn-dir syn-dir-total))
      (printf "lang: ~s\n pat: ~s\n term: ~s \n~s\n~s\n~s\n\n" lang p t non-syn-dir syn-dir syn-dir-total))))

(define (random-test n)
  (unless (< n 1)
    (begin (test)
           (random-test (- n 1)))))

(random-test 50)