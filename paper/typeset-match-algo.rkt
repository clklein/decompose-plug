#lang racket

(require redex
         "../sem-sem/syntax-directed-match.rkt")
(provide render-algorithm)

(define set-rw
  (match-lambda
    [(list _ _ elems ... _)
     `("{" ,@(intersperse elems ", ") "}")]))

(define productions-rw
  (match-lambda
    [(list _ _ lang non-term _)
     (list "" lang "(" non-term ")")]))

(define (infix-binop-rw op)
  (match-lambda
    [(list h _ l r _)
     (list l (just-after (format " ~a " op) l) r)]))

(define lub-rw (infix-binop-rw '⊓))
(define neq-rw (infix-binop-rw '≠))

(define (no-context-rw _)
  (list "[]"))
(define (no-bindings-rw _)
  (list "∅"))

(define (intersperse xs y)
  (match xs
    [(list) (list)]
    [(list x) (list x)]
    [(list-rest x1 x2 xs)
     (list* x1 (just-after y x1) (intersperse (cons x2 xs) y))]))

(define compound-rewriters
  (list (list 'set set-rw)
        (list 'no-context no-context-rw)
        (list 'no-bindings no-bindings-rw)
        (list '⊓ lub-rw)
        (list 'neq neq-rw)
        (list 'productions productions-rw)))

(define (unquote-rewriter unquoted)
  (define rewrite
    (match-lambda
      [(list-rest _ (? set-comp?) body _)
       (rewrite-set-comp 
        (cdr (drop-right (lw-e (caddr (lw-e body))) 1)))]))
  (define (set-comp? x)
    (and (lw? x) (eq? 'set-comp* (lw-e x))))
  (define rewrite-set-comp
    (match-lambda
      [(cons elem conds)
       `(,(just-before "{" elem)
         ,elem
         ,(just-after " |" elem)
         ,@(intersperse
            (for/list ([c conds])
              (define (replace with)
                (struct-copy lw c [e with]))
              (case (lw-e (second (lw-e c)))
                [(guard) (replace (list (struct-copy lw (first (lw-e c)) [e ""] [column-span 0])
                                        'spring
                                        (third (lw-e c))))]
                [(eq) (replace ((infix-binop-rw "=") (lw-e c)))]
                [(in) (replace ((infix-binop-rw "∈") (lw-e c)))]))
            ",")
         ,(just-after "}" (last conds)))]))
  (struct-copy lw unquoted
               [e (rewrite (lw-e unquoted))]
               [unq? false]))

(define (render-algorithm)
  (let loop ([rws compound-rewriters])
    (match rws
      ['() 
       (with-unquote-rewriter unquote-rewriter
                              (render-metafunction M))]
      [(cons (list name rewriter) rs)
       (with-compound-rewriter name rewriter (loop rs))])))
