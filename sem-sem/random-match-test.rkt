#lang racket

(require redex
         "sem-sem/shared-test-cases.rkt"
         (prefix-in syn-dir: "sem-sem/syntax-directed-match.rkt")
         (prefix-in syn-dir-total: "sem-sem/syntax-directed-match-total.rkt")
         (only-in "sem-sem/non-syntax-directed-match-test.rkt"
                  all-matches
                  no-contexts-bindings))

(define-language match-grammar
  (pat a
       (:cons pat pat)
       (:name x p))
  (p (:in-hole (:name x pat-one-hole) (:name x pat)))
  (pat-one-hole
   :hole
   (:name x p-one-hole)
   (:cons pat-one-hole pat-no-hole)
   (:cons pat-no-hole pat-one-hole))
  (p-one-hole
   (:in-hole (:name x pat-one-hole) (:name x pat-one-hole)))
  (pat-no-hole
   literal
   (:cons pat-no-hole pat-no-hole)
   (:in-hole (:name x pat-one-hole) (:name x pat-no-hole)))
  (a :hole
     literal)
  (literal variable-not-otherwise-mentioned
           number)
  (x variable-not-otherwise-mentioned))

; pat -> pat
; transforms patterns so that names are unique
(define (prepare t)
  (define state '())
  (define (get-fresh-name)
    (let ((x (variable-not-in state 'x)))
      (set! state (cons x state))
      x))
  (define (prep t)
    (match t
      [`(:name ,x ,p)
       `(:name ,(get-fresh-name) ,(prep p))]
      [`(:in-hole ,p1 ,p2)
       `(:in-hole ,(prep p1) ,(prep p2))]
      [`(:cons ,p1 ,p2)
       `(:cons ,(prep p1) ,(prep p2))]
      [_
       t]))
  (prep t))

(define (gen-term pat)
  (match pat
    [`(:name ,x ,p)
     (gen-term p)]
    [`(:in-hole (:cons ,p1 ,p2) ,p3)
     (if (redex-match match-grammar pat-one-hole p1)
         `(:cons ,(gen-term `(:in-hole ,p1 ,p3)) ,(gen-term p2))
         `(:cons ,(gen-term p1) ,(gen-term `(:in-hole ,p2 ,p3))))]
    [`(:in-hole (:in-hole ,p1 ,p2) ,p3)
     (gen-term `(:in-hole ,p1 (:in-hole ,p2 ,p3)))]
    [`(:in-hole :hole ,p1)
     (gen-term p1)]
    [`(:in-hole (:name ,x ,p1) ,p2)
     (gen-term `(:in-hole ,p1 ,p2))]
    [`(:cons ,p1 ,p2)
     `(:cons ,(gen-term p1) ,(gen-term p2))]
    [:hole
     :hole]
    [_
     pat]))

(define test-size 4)

(define (test)
  (let* ((p (prepare (generate-term match-grammar pat test-size)))
         (t (gen-term p))
         (syn-dir (no-contexts-bindings (term (syn-dir:matches () ,p ,t))))
         (syn-dir-total (no-contexts-bindings (syn-dir-total:matches '() p t)))
         (non-syn-dir (all-matches '() p t)))
    (unless
        (and (equal-bindings? non-syn-dir syn-dir)
             (equal-bindings? syn-dir syn-dir-total))
      (pretty-print p))))

(define (random-test n)
  (unless (< n 1)
    (begin (test)
           (random-test (- n 1)))))

(random-test 1000)