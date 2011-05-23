#lang racket/base
(require redex/pict
         slideshow/pict)
(provide with-rewriters
         rule-schema)

(define-syntax-rule 
  (with-rewriters arg)
  (with-rewriters/proc (λ () arg)))

(define-syntax-rule
  (rule-schema language schema)
  (frame (inset (render-lw language (to-lw schema)) 3 3)))

(define (with-rewriters/proc thunk)
  (with-compound-rewriter
   'append-contexts rewrite-append-contexts
   (with-compound-rewriter
    '~ rewrite-~
    (with-compound-rewriter
     'matches rewrite-matches
     (with-compound-rewriter
      'decomposes rewrite-decomposes
      (thunk))))))

(define (rewrite-append-contexts lws)
  (list ""
        (list-ref lws 2)
        " ++ "
        (list-ref lws 3)
        " = "
        (list-ref lws 4)
        ""))

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