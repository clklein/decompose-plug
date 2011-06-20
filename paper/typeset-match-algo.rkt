#lang racket

(require redex/pict
         slideshow/pict
         "common.rkt"
         "../sem-sem/syntax-directed-match.rkt")
(provide mt
         render-algorithm)

(define-syntax-rule (mt t) ; "matching term"
  (with-rewriters (lw->pict directed-matching (to-lw t))))

(define compound-rewriters
  (list (list 'append-contexts rewrite-append-contexts)
        (list 'set/id rewrite-set)
        (list 'set rewrite-set)
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
                (struct-copy lw c [e (append space lws '(spring))]))
              (case (lw-e (second (lw-e c)))
                [(guard) (replace (list (third (lw-e c))))]
                [(eq) (replace (rewrite-eq (lw-e c)))]
                [(in) (replace (rewrite-in (lw-e c)))]))
            ",")
         ,(just-after "}" (last conds)))]))
  (struct-copy lw unquoted
               [e (rewrite (lw-e unquoted))]
               [unq? false]))

(define-syntax-rule (with-rewriters expr)
  (with-keyword-rewriters
   (λ ()
     (with-unquote-rewriter 
      unquote-rewriter
      (let loop ([rws compound-rewriters])
        (match rws
          ['() expr]
          [(cons (list name rewriter) rs)
           (with-compound-rewriter name rewriter (loop rs))]))))))

(define (render-algorithm)
  (with-rewriters
   (vl-append
    (htl-append 
     (vl-append
      (metafunction-signature "matches" "G" "p" "t" (powerset "b"))
      (render-metafunction matches))
     (blank 80 0)
     (parameterize ([render-language-nts '(d m)])
       (render-language directed-matching)))
    
    (blank 0 vertical-gap-size)
    
    (metafunction-signature "M" "G" "p" "t" (powerset "m"))
    (render-metafunction M)
    
    (blank 0 vertical-gap-size)
    
    (metafunction-signature "select" "t" "d" "t" "d" (powerset "d"))
    (render-metafunctions select)
    
    (blank 0 vertical-gap-size)
    
    (metafunction-signature "combine" "C" "d" "d")
    (render-metafunctions combine)
    
    (blank 0 vertical-gap-size)
    
    (metafunction-signature "named" "d" "t" "t")
    (render-metafunctions named))))
