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

(define (rewrite-plug lws)
  (list "" (list-ref lws 2) "[" (list-ref lws 3) "] = " (list-ref lws 4)))

(define (no-white-brackets lws)
  (list (text
         (symbol->string (lw-e (list-ref lws 1)))
         (metafunction-style)
         (metafunction-font-size)) 
        " "
        (list-ref lws 2)))

(define compound-rewriters
  (list (list 'meta-app rewrite-meta-app)
        (list 'no-ctxts no-white-brackets)
        (list 'non-ctxt no-white-brackets)
        (list 'plug rewrite-plug)
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

(define (horizontal-clauses l)
  (apply hbl-append 20 l))

(define (plug-rules . rule-indices)
  (parameterize ([relation-clauses-combine horizontal-clauses]
                 [metafunction-cases rule-indices])
    (render-judgment-form plug)))

(define plug-schema
  (with-rewriters
   (rule-schema reduction (plug t_1 t_2 t_3))))
(define framed-plug-schema
  (frame-rule-schema plug-schema))

(define no-ctxts-schema
  (with-rewriters
   (rule-schema reduction (no-ctxts t))))
(define framed-no-ctxts-schema
  (frame-rule-schema no-ctxts-schema))

(define non-ctxt-schema
  (with-rewriters
   (rule-schema reduction (non-ctxt t))))
(define framed-non-ctxt-schema
  (frame-rule-schema non-ctxt-schema))

(define (render-reduction)
  (with-rewriters
   (let ([plug-schema-placeholder (ghost framed-plug-schema)]
         [no-ctxts-schema-placeholder (ghost framed-no-ctxts-schema)]
         [non-ctxt-schema-placeholder (ghost framed-non-ctxt-schema)])
     (define without-schemas
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
         (metafunction-signature "inst" "r" "b" "t")
         (render-metafunctions inst))
        
        (vc-append
         plug-schema-placeholder
         (vc-append
          vertical-gap-size
          (plug-rules 0)
          (plug-rules 1 2)
          (plug-rules 3 4)
          (plug-rules 5 6)))
        
        (vc-append
         no-ctxts-schema-placeholder
         (parameterize ([relation-clauses-combine horizontal-clauses])
           (render-judgment-form no-ctxts)))
        
        (vc-append
         non-ctxt-schema-placeholder
         (parameterize ([relation-clauses-combine horizontal-clauses])
           (render-judgment-form non-ctxt)))))
     (define (schema-offset placeholder)
       (define-values (x y)
         (ct-find without-schemas placeholder))
       y)
     (define (pin-schema placeholder schema base)
       (pin-over base 0 (schema-offset placeholder) schema))
     (foldl pin-schema
            without-schemas
            (list plug-schema-placeholder
                  no-ctxts-schema-placeholder
                  non-ctxt-schema-placeholder)
            (list framed-plug-schema 
                  framed-no-ctxts-schema
                  framed-non-ctxt-schema)))))

(define-syntax-rule (rt t) ; "reduction term"
  (with-rewriters (lw->pict reduction (to-lw t))))