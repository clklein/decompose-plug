#lang racket

(require redex/pict
         slideshow/pict
         "common.rkt"
         "../sem-sem/common.rkt"
         "../sem-sem/reduction.rkt")
(provide render-reduction)

(define (rewrite-reduces lws)
  (list ""
        (list-ref lws 2)
        " ⊢ "
        (list-ref lws 3)
        " / "
        (list-ref lws 4)
        " → "
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

(define (render-reduction)
  (let loop ([rws compound-rewriters])
    (match rws
      ['() 
       (with-keyword-rewriters
        (λ ()
          (vc-append
           vertical-gap-size 
           (hc-append
            horizontal-gap-size
            (render-relation reduces)
            (parameterize ([render-language-nts '(r)])
              (vl-append
               (render-language reduction)
               (non-bnf-def "f" (arbitrary-function-domain "t" "t")))))
           (render-metafunctions inst join plug)
           (render-relation no-ctxts))))]
      [(cons (list name rewriter) rs)
       (with-compound-rewriter name rewriter (loop rs))])))
