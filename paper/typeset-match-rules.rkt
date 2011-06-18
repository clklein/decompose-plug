#lang racket
(require redex/pict
         slideshow/pict
         "../sem-sem/patterns.rkt"
         "../sem-sem/common.rkt"
         "../sem-sem/non-syntax-directed-match-define-relation.rkt"
         "common.rkt")
(provide pt
         with-rewriters
         combined-matching-rules
         patterns-and-terms
         matching-data-defs
         matches-schema matches-schema/unframed
         matches-rules
         decomposes-schema decomposes-schema/unframed
         decomposes-rules)

(define-syntax-rule 
  (with-rewriters arg)
  (with-rewriters/proc (λ () arg)))

(define-syntax-rule
  (rule-schema language schema)
  (render-lw language (to-lw schema)))
(define (frame-rule-schema s)
  (frame (inset s 3 3)))

(define (with-rewriters/proc thunk)
  (let loop ([rs compound-rewriters])
    (match rs
      ['() (with-keyword-rewriters (λ () (thunk)))]
      [(cons (list name rewriter) rs*)
       (with-compound-rewriter name rewriter (loop rs*))])))

(define-syntax-rule (pt t) ; "pattern term"
  (with-rewriters (lw->pict patterns (to-lw t))))

(define (rewrite-~ lws)
  (list ""
        (list-ref lws 2)
        " ~ "
        (list-ref lws 3)
        ""))

(define rewrite-decomposes
  (match-lambda
    [(list lp d G t C t′ p b rb)
     (list "" G " ⊢ " t " = " C "[" t′ "]" " : " p " | " b)]
    [(list lp d t C t′ p rb)
     (list "" t " = " C "[" t′ "]" " : " p)]))

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

(define (rewrite-set-adjoin lws)
  (list "{" (list-ref lws 2) "} ∪ " (list-ref lws 3)))

(define compound-rewriters
  (list (list 'append-contexts rewrite-append-contexts)
        (list 'matches rewrite-matches)
        (list 'decomposes rewrite-decomposes)
        (list 'no-bindings rewrite-no-bindings)
        (list 'nt-has-prod rewrite-nt-has-prod)
        (list 'lub-not-top rewrite-lub-not-top)
        (list '⊔ rewrite-lub)
        (list 'neq rewrite-neq)
        (list 'pair rewrite-pair)
        (list 'set-adjoin rewrite-set-adjoin)
        (list 'set rewrite-set)))

(define matches-schema/unframed
  (with-rewriters (rule-schema patterns (matches G t p b))))
(define matches-schema
  (frame-rule-schema matches-schema/unframed))
(define matches-rules
  (with-rewriters 
   (parameterize ([relation-clauses-combine
                   (match-lambda
                     [(list-rest atom hole others)
                      (apply vc-append 20 (hb-append 20 atom hole) others)])])
     (render-relation matches))))

(define decomposes-schema/unframed
  (with-rewriters (rule-schema patterns (decomposes G t C t_^′ p b))))
(define decomposes-schema
  (frame-rule-schema decomposes-schema/unframed))
(define decomposes-rules 
  (with-rewriters (with-rewriters (render-relation decomposes))))

(define matching-data-defs
  (with-rewriters
   (vl-append
    (non-bnf-def "G" (finite-function-domain "Non-Terminals" (powerset "p")))
    (non-bnf-def "b" (finite-function-domain "Variables" "t")))))

(define combined-matching-rules
  (vl-append
   20
   matching-data-defs
   (pin-over
    (pin-over
     (vc-append (+ (* 2 vertical-gap-size)
                   (pict-height decomposes-schema))
                matches-rules 
                decomposes-rules)
     0 (+ vertical-gap-size
          (pict-height matches-rules))
     decomposes-schema)
    0 0 matches-schema)))

(define patterns-and-terms
  (with-rewriters
   (vl-append
    (parameterize ([render-language-nts '(t p)])
      (render-language patterns))
    (non-bnf-def "a" "Literals")
    (non-bnf-def "x" "Variables")
    (non-bnf-def "n" "Non-Terminals"))))
