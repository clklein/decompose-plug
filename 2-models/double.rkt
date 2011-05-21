#lang racket/base
(require redex/reduction-semantics
         (for-syntax racket/base))
(provide define-double-language)
(define-syntax (define-double-language stx)
  (syntax-case stx ()
    [(_ id1 id2 rhs ...)
     #'(define-language id1 rhs ...)]))