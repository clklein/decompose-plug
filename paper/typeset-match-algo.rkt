#lang racket

(require redex/pict
         "common-rewriters.rkt"
         "../sem-sem/syntax-directed-match.rkt")
(provide render-algorithm)

(define compound-rewriters
  (list (list 'set rewrite-set)
        (list 'pair rewrite-pair)
        (list 'no-bindings rewrite-no-bindings)
        (list '⊔ rewrite-lub)
        (list 'neq rewrite-neq)
        (list 'productions rewrite-productions)))

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
              (define (replace lws)
                (define space
                  (list (struct-copy lw (first (lw-e c)) 
                                     [e ""]
                                     [column-span 0])
                        'spring))
                (struct-copy lw c [e (append space lws)]))
              (case (lw-e (second (lw-e c)))
                [(guard) (replace (list (third (lw-e c))))]
                [(eq) (replace (rewrite-eq (lw-e c)))]
                [(in) (replace (rewrite-eq (lw-e c)))]))
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
