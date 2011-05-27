#lang racket

(require redex/pict)
(provide (all-defined-out))

(define rewrite-productions
  (match-lambda
    [(list _ _ lang non-term _)
     (list "" lang "(" non-term ")")]))

(define (make-infix-binop-rewriter op)
  (match-lambda
    [(list s _ l r _)
     (list (struct-copy lw s [e ""] [column-span (- (lw-column l) (lw-column s))])
           l 
           (just-after (format " ~a " op) l)
           r)]))

(define rewrite-lub (make-infix-binop-rewriter '⊔))
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

(define (rewrite-pair lws)
  (list "(" (list-ref lws 2) ", " (list-ref lws 3) ")"))

(define rewrite-set
  (match-lambda
    [(list _ _ elems ... _)
     `("{" ,@(intersperse elems ", ") "}")]))

(define (intersperse xs y)
  (match xs
    [(list) (list)]
    [(list x) (list x)]
    [(list-rest x1 x2 xs)
     (list* x1 (just-after y x1) (intersperse (cons x2 xs) y))]))