#lang racket

(require "shared-test-cases.rkt"
         "patterns.rkt"
         "syntax-directed-match-total.rkt"
         rackunit)

(define test-syntax-directed-total
  (match-lambda
    [(test:match _ L p t)
     (not (empty? (matches L p t)))]
    [(test:no-match _ L p t)
     (empty? (matches L p t))]
    [(test:bind _ L p t bs)
     (equal-bindings? (matches L p t) bs)]))

(run-tests test-syntax-directed-total)

(let ([W? (λ (t)
            (not
             (empty?
              (matches '([W (:hole (:in-hole (:nt W) (:cons :hole 1)))])
                       '(:nt W)
                       t))))])
  (check-true (W? ':hole))
  (check-true (W? '(:cons :hole 1)))
  (check-true (W? '(:cons (:cons :hole 1) 1)))
  (check-false (W? '(:cons 1 (:cons :hole 1)))))

(let ([A? (λ (t)
            (not
             (empty?
              (matches '([L (:hole (:in-hole (:cons (:nt L) e) (:cons λ :hole)))]
                         [A (:hole (:in-hole (:nt L) (:nt A)))])
                       '(:nt A)
                       t))))])
  (check-true (A? ':hole))
  (check-true (A? '(:cons (:cons (:cons λ (:cons λ :hole)) e) e)))
  (check-false (A? '(:cons (:cons (:cons λ :hole) e) e)))
  (check-false (A? '(:cons (:cons λ (:cons λ :hole)) e)))
  (check-true (A? '(:cons (:cons λ (:cons (:cons (:cons λ (:cons λ :hole)) e) e)) e)))
  (check-false (A? '(:cons (:cons λ (:cons (:cons λ (:cons λ :hole)) e)) e)))
  (check-false (A? '(:cons (:cons (:cons λ (:cons (:cons λ (:cons λ :hole)) e)) e) e))))

(let ([shift-reset ; like http://arxiv.org/pdf/cs/0508048v4 (Section 4.4)
       `([t ((:nt v) 
             (:nt x)
             (:cons (:nt t) (:cons (:nt t) mt)) ; app
             (:cons succ (:cons (:nt t) mt))
             (:cons reset (:cons (:nt t) mt))
             (:cons shift (:cons (:nt x) (:cons (:nt t) mt))))]
         [v ((:nt m)
             (:cons λ (:cons (:nt x) (:cons (:nt t) mt)))
             (:cons cont (:cons (:nt C) mt)))]
         [C (:hole
             (:cons (:nt C) (:cons (:nt t) mt))
             (:cons (:nt v) (:cons (:nt C) mt))
             (:cons succ (:cons (:nt C) mt)))]
         [M (:hole
             (:in-hole (:nt C) (:cons reset (:cons (:nt M) mt))))]
         [x ,(build-list 
              26
              (λ (i) 
                (string->symbol
                 (list->string
                  (list (integer->char (+ i (char->integer #\a))))))))]
         [m ,(build-list 10 values)])])
  (check-equal?
   (matches 
    shift-reset
    '(:in-hole (:in-hole (:name M (:nt M)) (:name C (:nt C))) r)
    (encode-term '((λ x x) (reset (succ (reset ((λ x x) (r 2))))))))
   `(((C ((right ,(encode-term '(λ x x)))
          ((left mt)
           ((left ,(encode-term '(2)))
            no-context))))
      (M ((right ,(encode-term '(λ x x)))
          ((left mt)
           ((right reset)
            ((left mt)
             ((right succ)
              ((left mt)
               ((right reset)
                ((left mt)
                 no-context)))))))))))))