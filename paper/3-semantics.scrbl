#lang scribble/base

@(require scriblib/figure
          redex/pict
          slideshow/pict
          "typesetting-setup.rkt"
          "../sem-sem/patterns.rkt"
          "../sem-sem/non-syntax-directed-match-define-relation.rkt")

@title{A Semantics for Matching}

@figure["matching" "Matching and Decomposition"]{
  @(centered
    (with-rewriters
     (let ([matches-schema (rule-schema patterns (matches L t p b))]
           [matches-rules (render-relation matches)]
           [decomposes-schema (rule-schema patterns (decomposes L t C t p b))]
           [decomposes-rules (render-relation decomposes)]
           [vertical-space 10])
       (pin-over
        (pin-over
         (vc-append (+ (pict-height decomposes-schema) (* 2 vertical-space))
                    matches-rules 
                    decomposes-rules)
         0 0 
         matches-schema)
        0 (+ (pict-height matches-rules) vertical-space) 
        decomposes-schema))))
}
