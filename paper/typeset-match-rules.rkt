#lang racket
(require redex/pict
         slideshow/pict
         "../sem-sem/patterns.rkt"
         "../sem-sem/common.rkt"
         "../sem-sem/non-syntax-directed-match-define-relation.rkt"
         "common-rewriters.rkt")
(provide combined-matching-rules
         binding-consistency
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

(define matches-schema
  (with-rewriters (rule-schema patterns (matches L t p b))))
(define matches-rules
  (with-rewriters (render-relation matches)))

(define decomposes-schema
  (with-rewriters (rule-schema patterns (decomposes L t C t p b))))
(define decomposes-rules 
  (with-rewriters (with-rewriters (render-relation decomposes))))

(define combined-matching-rules
  (let ([vertical-space 10])
    (pin-over
     (pin-over
      (vc-append vertical-space
                 matches-rules 
                 decomposes-rules)
      0 0 
      matches-schema)
     0 (+ (pict-height matches-rules) vertical-space) 
     decomposes-schema)))

(define binding-consistency
  (with-rewriters 
   (render-metafunctions ⊔ merge-binding merge-value)))