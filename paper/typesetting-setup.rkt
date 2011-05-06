#lang racket/base
(require redex/pict)
(provide with-rewriters)
(define-syntax-rule 
  (with-rewriters arg)
  (with-rewriters/proc (λ () arg)))

(define (with-rewriters/proc thunk)
  (with-compound-rewriter
   'append-contexts
   (λ (lws) 
     (list ""
           (list-ref lws 2)
           " ++ "
           (list-ref lws 3)
           " = "
           (list-ref lws 4)
           ""))
   (with-compound-rewriter
    '~
    (λ (lws) 
      (list ""
            (list-ref lws 2)
            " ~ "
            (list-ref lws 3)
            ""))
    (thunk))))
