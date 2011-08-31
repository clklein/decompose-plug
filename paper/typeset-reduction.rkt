#lang racket

(require redex/pict
         slideshow/pict
         "common.rkt"
         "../sem-sem/common.rkt"
         "../sem-sem/reduction.rkt")
(provide rt render-reduction)

(define (rewrite-reduces lws)
  (list ""
        (list-ref lws 2)
        " ⊢ "
        (list-ref lws 3)
        " / "
        (list-ref lws 4)
        (hbl-append (text " ") (arrow->pict '-->) (text " "))
        (list-ref lws 5)
        " / "
        (list-ref lws 6)
        ""))

(define (rewrite-lookup lws)
  (list "" (list-ref lws 2) "(" (list-ref lws 3) ")"))

(define (rewrite-meta-app lws)
  (list "δ(" (list-ref lws 2) ", " (list-ref lws 3) ")"))

(define (rewrite-no-ctxts lws)
  (list (text
         (symbol->string (lw-e (list-ref lws 1)))
         (metafunction-style)
         (metafunction-font-size)) 
        " "
        (list-ref lws 2)))

(define compound-rewriters
  (list (list 'meta-app rewrite-meta-app)
        (list 'no-ctxts rewrite-no-ctxts)
        (list 'lookup rewrite-lookup)
        (list 'eq rewrite-eq)
        (list 'set rewrite-set)
        (list 'pair rewrite-pair)
        (list 'matches rewrite-matches)
        (list 'reduces rewrite-reduces)))

(define-syntax-rule (with-rewriters expr)
  (let loop ([rws compound-rewriters])
    (match rws
      ['() 
       (with-keyword-rewriters
        (λ () expr))]
      [(cons (list name rewriter) rs)
       (with-compound-rewriter name rewriter (loop rs))])))

(define (render-reduction)
  (with-rewriters
   (vc-append
    (hc-append
     horizontal-gap-size
     (parameterize ([metafunction-cases '(0)])
       (render-judgment-form reduces))
     (parameterize ([render-language-nts '(r)])
       (vl-append
        (render-language reduction)
        (non-bnf-def "f" (arbitrary-function-domain "t" "t")))))
    
    (blank 0 vertical-gap-size)
    
    (vl-append
     (metafunction-signature "inst" "r" "b" "t")
     (render-metafunctions inst)
     
     (blank 0 vertical-gap-size)
     
     (metafunction-signature "plug" "C" "t" "t")
     (render-metafunctions plug)
     
     (blank 0 vertical-gap-size)
     
     (metafunction-signature "join" "t" "t" "t")
     (render-metafunctions join))
    
    (blank 0 vertical-gap-size)
    
    (parameterize ([relation-clauses-combine (λ (l) (apply hbl-append 20 l))])
      (render-relation no-ctxts)))))

(define-syntax-rule (rt t) ; "reduction term"
  (with-rewriters (lw->pict reduction (to-lw t))))