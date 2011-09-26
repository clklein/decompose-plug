#lang racket

(require redex/pict
         slideshow/pict
         "common.rkt"
         "../semantics/common.rkt"
         "../semantics/reduction.rkt")
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

(define (rewrite-3-as-fn lws)
  (list (text
         (symbol->string (lw-e (list-ref lws 1)))
         (metafunction-style)
         (metafunction-font-size))
        (white-bracket "[") (list-ref lws 2) "," (list-ref lws 3) (white-bracket "]")
        " = " (list-ref lws 4)))

(define (white-bracket str)
  (let-values ([(left-inset-amt right-inset-amt left-space right-space)
                ((white-bracket-sizing) str 
                                        (default-font-size))])
    (let ([main-bracket (text str (default-style) (default-font-size))])
      (inset (refocus (cbl-superimpose main-bracket
                                       (hbl-append (blank left-inset-amt)
                                                   (text str (default-style) (default-font-size))
                                                   (blank right-inset-amt)))
                      main-bracket)
             left-space
             0
             right-space
             0))))

(define (no-white-brackets lws)
  (list (text
         (symbol->string (lw-e (list-ref lws 1)))
         (metafunction-style)
         (metafunction-font-size)) 
        " "
        (list-ref lws 2)))

(define (rewrite-tuple lws)
  (list left-tuple 
        (list-ref lws 2)
        ", "
        (list-ref lws 3)
        right-tuple))

(define (rewrite-wedge lws)
  (list ""
        (list-ref lws 2)
        " ∧ "
        (list-ref lws 3)
        ""))

(define (rewrite-vee lws)
  (list ""
        (list-ref lws 2)
        " ∨ "
        (list-ref lws 3)
        ""))

(define compound-rewriters
  (list (list 'meta-app rewrite-meta-app)
        (list 'lookup rewrite-lookup)
        (list 'eq rewrite-eq)
        (list 'set rewrite-set)
        (list 'pair rewrite-pair)
        (list 'no-ctxts no-white-brackets)
        (list 'matches rewrite-matches)
        (list 'reduces rewrite-reduces)
        (list 'tuple rewrite-tuple)
        (list '∧ rewrite-wedge)
        (list '∨ rewrite-vee)))

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
    20
    (hc-append
     horizontal-gap-size
     (parameterize ([metafunction-cases '(0)])
       (render-judgment-form reduces))
     (parameterize ([render-language-nts '(r)])
       (vl-append
        (render-language reduction)
        (non-bnf-def "f" (arbitrary-function-domain "t" "t")))))
    
    (vl-append
     20 
     (vl-append
      (metafunction-signature "inst" "r" "b" (list "t" "bool"))
      (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
        (render-metafunctions inst)))
     
     (vl-append
      (metafunction-signature "plug" "C" (list "t" "bool") (list "t" "bool"))
      (render-metafunctions plug))
     
     (vl-append
      (metafunction-signature "join" (list "t" "bool") (list "t" "bool") (list "t" "bool"))
      (parameterize ([metafunction-pict-style 'left-right/beside-side-conditions])
        (render-metafunctions join)))
     
     (vl-append
      (metafunction-signature "has-context" "t" "bool")
      (render-metafunctions has-context))
     
     (vl-append
      (metafunction-signature "δ" "(t → t)" (list "t" "bool") (list "t" "bool"))
      (text "An unspecified function that applies metafunctions"
            (cons 'italic (default-style)))))
    
    #;
    (parameterize ([relation-clauses-combine 
                    (λ (l) (apply hbl-append 40 l))])
      (render-judgment-form no-ctxts)))))

(define-syntax-rule (rt t) ; "reduction term"
  (with-rewriters (lw->pict reduction (to-lw t))))