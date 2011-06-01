#lang racket
(require redex/pict
         slideshow/pict
         "../sem-sem/patterns.rkt"
         "../sem-sem/common.rkt"
         "../sem-sem/non-syntax-directed-match-define-relation.rkt"
         "common-rewriters.rkt")
(provide combined-matching-rules
         binding-consistency
         patterns-and-terms
         matching-data-defs
         matches-schema
         matches-rules
         decomposes-schema
         decomposes-rules)

(define-syntax-rule 
  (with-rewriters arg)
  (with-rewriters/proc (λ () arg)))

(define-syntax-rule
  (rule-schema language schema)
  (frame (inset (render-lw language (to-lw schema)) 3 3)))

(define (with-rewriters/proc thunk)
  (let loop ([rs compound-rewriters])
    (match rs
      ['() (thunk)]
      [(cons (list name rewriter) rs*)
       (with-compound-rewriter name rewriter (loop rs*))])))

(define (rewrite-~ lws)
  (list ""
        (list-ref lws 2)
        " ~ "
        (list-ref lws 3)
        ""))

(define (rewrite-matches lws)
  (list ""
        (list-ref lws 2)
        " ⊢ "
        (list-ref lws 3)
        " : "
        (list-ref lws 4)
        " | "
        (list-ref lws 5)))

(define (rewrite-decomposes lws)
  (list ""
        (list-ref lws 2)
        " ⊢ "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        "["
        (list-ref lws 5)
        "]"
        " : "
        (list-ref lws 6)
        " | "
        (list-ref lws 7)))

(define (rewrite-nt-has-prod lws)
  (list ""
        (list-ref lws 2)
        " ∈ "
        (list-ref lws 3)
        "("
        (list-ref lws 4)
        ")"))

(define (rewrite-lub-not-top lws)
  (list ""
        (list-ref lws 2)
        " ⊔ "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        " ≠ "
        "⊤"))

(define compound-rewriters
  (list (list 'append-contexts rewrite-append-contexts)
        (list '~ rewrite-~)
        (list 'matches rewrite-matches)
        (list 'decomposes rewrite-decomposes)
        (list 'no-bindings rewrite-no-bindings)
        (list 'nt-has-prod rewrite-nt-has-prod)
        (list 'lub-not-top rewrite-lub-not-top)
        (list '⊔ rewrite-lub)
        (list 'pair rewrite-pair)
        (list 'set rewrite-set)))

(define gap-size 10)

(define matches-schema
  (with-rewriters (rule-schema patterns (matches L t p b))))
(define matches-rules
  (with-rewriters (render-relation matches)))

(define decomposes-schema
  (with-rewriters (rule-schema patterns (decomposes L t C t p b))))
(define decomposes-rules 
  (with-rewriters (with-rewriters (render-relation decomposes))))

(define matching-data-defs
  (ht-append
   gap-size
   (vl-append
    (text "L ∈ Non-Terminals -o> 2^p")
    (text "b ∈ Variable -o> v")
    (parameterize ([render-language-nts '(v)])
      (render-language patterns)))
   (parameterize ([render-language-nts '(C)])
     (render-language patterns))))

(define combined-matching-rules
  (vl-append
   gap-size
   matching-data-defs
   (pin-over
    (pin-over
     (vc-append gap-size
                matches-rules 
                decomposes-rules)
     0 0 
     matches-schema)
    0 (+ (pict-height matches-rules) gap-size) 
    decomposes-schema)))

(define binding-consistency
  (with-rewriters 
   (render-metafunctions ⊔ merge-binding merge-value)))

(define patterns-and-terms
  (ht-append
   gap-size
   (parameterize ([render-language-nts '(p)])
     (render-language patterns))
   (vl-append
    (parameterize ([render-language-nts '(a t)])
      (render-language patterns))
    (text "x ∈ Variables")
    (text "n ∈ Non-Terminals"))))