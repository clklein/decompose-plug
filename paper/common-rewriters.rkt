#lang racket

(require redex/pict)
(provide (all-defined-out))

(define rewrite-productions
  (match-lambda
    [(list _ _ lang non-term _)
     (list "" lang "(" non-term ")")]))

(define (make-infix-binop-rewriter op)
  (match-lambda
    [(list _ _ l r _)
     (list l (just-after (format " ~a " op) l) r)]))

(define rewrite-lub (make-infix-binop-rewriter '⊓))
(define rewrite-neq (make-infix-binop-rewriter '≠))
(define rewrite-eq (make-infix-binop-rewriter '=))
(define rewrite-in (make-infix-binop-rewriter '∈))

(define (rewrite-no-bindings _)
  (list "∅"))

(define (rewrite-append-contexts lws)
  (list ""
        (list-ref lws 2)
        " ++ "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        ""))