#lang racket

(require redex)
(provide set-comp guard in eq)

(define-for-syntax (keyword stx)
  (raise-syntax-error #f "must be inside of a set comprehension" stx))

(define-syntax guard keyword)
(define-syntax in keyword)
(define-syntax eq keyword)

(define-syntax (set-comp stx)
  (syntax-case stx (guard in eq)
    [(_ language element)
     #'(list (term element))]
    [(form language element clause ... (guard t))
     #'(if (term t)
           (form language element clause ...)
           '())]
    [(form language element clause ... (in p t))
     #'(remove-duplicates
        (append-map 
         (λ (x) (form language element clause ... (eq p ,x)))
         (term t)))]
    [(form language element clause ... (eq p t))
     #'((term-match/single 
         language
         [p (form language element clause ...)]
         [any '()])
        (term t))]
    [(form language element clause ... malformed)
     (raise-syntax-error (syntax-e #'form) "malformed clause" #'malformed)]))