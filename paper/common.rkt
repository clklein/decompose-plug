#lang racket

(require redex/pict slideshow/pict)
(provide (all-defined-out))

(define (rewrite-matches lws)
  (list ""
        (list-ref lws 2)
        " ⊢ "
        (list-ref lws 3)
        " : "
        (list-ref lws 4)
        " | "
        (list-ref lws 5)))

(define rewrite-productions
  (match-lambda
    [(list _ _ lang non-term _)
     (list "" lang "(" non-term ")")]))

(define (make-infix-binop-rewriter op)
  (match-lambda
    [(list s _ l r e)
     (list (struct-copy lw s [e ""] [column-span (- (lw-column l) (lw-column s))])
           l 
           (just-after (format " ~a " op) l)
           r
           (struct-copy lw e [e ""]))]))

(define rewrite-lub (make-infix-binop-rewriter '⊔))
(define rewrite-neq (make-infix-binop-rewriter '≠))
(define rewrite-eq (make-infix-binop-rewriter '=))
(define rewrite-in (make-infix-binop-rewriter '∈))

(define (rewrite-no-bindings _)
  (list "∅"))

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

(define word-gap (pict-width (text " ")))

(define (non-bnf-def var dom)
  (hbl-append
   word-gap
   (non-terminal-text var)
   (text "∈")
   (pict/nt dom)))

(define (function-domain arrow dom codom)
  (hbl-append word-gap (pict/nt dom) arrow (pict/nt codom)))
(define (arbitrary-function-domain dom codom)
  (function-domain (text "→") dom codom))
(define finite-function-domain arbitrary-function-domain)

(define (non-terminal-text t)
  (text t (non-terminal-style) (default-font-size)))

(define (pict/nt t)
  (cond [(string? t)
         (non-terminal-text t)]
        [(pict? t) t]
        [else (error 'pict/nt "expected pict or string but got ~s" t)]))

(define (powerset s)
  (hbl-append (text "℘(") (pict/nt s) (text ")")))

(define (metafunction-signature name . contract)
  (define domain (drop-right contract 1))
  (define codomain (last contract))
  (apply
   hbl-append word-gap 
   (text name (metafunction-style) (metafunction-font-size))
   (text ":")
   (append (map pict/nt domain) (list (text "→") (pict/nt codomain)))))

(define vertical-gap-size 10)
(define horizontal-gap-size 30)

(define atomic-rewriters
  '((:name "name")
    (:nt "nt")
    (:in-hole "in-hole")
    (:cons "cons")
    (:no-ctxt "no-ctxt")
    (:var "var")
    (:app "app")
    (:left "left")
    (:right "right")
    (:hole "hole")))

(define (with-keyword-rewriters thunk)
  (let loop ([rs atomic-rewriters])
    (match rs
      ['() (thunk)]
      [(cons (list s r) rs)
       (with-atomic-rewriter s r (loop rs))])))