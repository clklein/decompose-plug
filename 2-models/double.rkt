#lang racket/base
(require racket/match
         "../sem-sem/syntax-directed-match-total.rkt")
#|

This file does the translation from
Redex-looking notation to the notation
for the sem-sem/ directory models

|#

(require redex/reduction-semantics
         (for-syntax racket/base))
(provide define-double-language)

;; nts : (list-of symbol)
;; lang : `([,nt ,pat ...] ...)   -- matches the language setup
;;                                -- of syntax directed match total
(struct lang (nts lang) #:transparent)

(define-syntax (define-double-language stx)
  (syntax-case stx ()
    [(_ id1 id2 (nt prods ...) ...)
     (begin
       (for ([nt (in-list (syntax->list #'(nt ...)))])
         (unless (identifier? nt)
           (raise-syntax-error #f "expected identifier" stx nt)))
       #'(begin
           (define id2
             (let ([nts '(nt ...)]) 
               (lang nts `((nt (,(rp->p nts 'prods #f) ...)) ...))))
           (define-language id1 (nt prods ...) ...)))]))

(define (rp->p nts pat [name-nts? #t])
  (let loop ([pat pat])
    (match pat
      ['hole ':hole]
      [(? symbol?)
       (define-values (prefix has-suffix?)
         (let ([m (regexp-match #rx"^([^_])_(.*)" (symbol->string pat))])
           (if m
               (values (string->symbol (list-ref m 1)) #t)
               (values pat #f))))
       (cond
         [(memq prefix nts)
          (if name-nts?
              `(:name ,pat (:nt ,prefix))
              `(:nt ,prefix))]
         [else pat])]
      [`(in-hole ,p1 ,p2) `(:in-hole ,(loop p1) ,(loop p2))]
      [(? pair?) `(:cons ,(loop (car pat)) ,(loop (cdr pat)))]
      [(? null?) `empty]
      [(? number?) pat])))
      

;; a hand-translated version of the C/a 
#;
(matches '([C (:hole 
               (:cons + (:cons (:nt C) (:cons (:nt a) empty)))
               (:cons + (:cons (:nt a) (:cons (:nt C) empty))))]
           [a ((:cons + (:cons (:nt a) (:cons (:nt a) empty)))
               1 
               2)])
         '(:in-hole (:name C (:nt C)) (:name a (:nt a)))
         '(:cons + (:cons 1 (:cons 2 empty))))

(define-double-language a b
  (C hole (+ C a) (+ a C))
  (a (+ a a) 1 2))

;(define (conv-b b)
;  (for ([x (in-list 
;  (match b

(define (p->rt p)
  (let loop ([p p])
    (match p
      [`:hole 'hole]
      [`(:in-hole ,a ,b) `(in-hole ,(loop a) ,(loop b))]
      [`(:cons ,a ,b) (cons (loop a) (loop b))]
      [`empty '()]
      [`(:name ,x (:nt ,b)) x]
      [`(:name ,x ,p) `(name ,x ,(loop p))]
      [`(:nt ,x) x]
      [`((right ,x) ,stuff ...) (c->rt p)]
      [`((left ,x) ,stuff ...) (c->rt p)]
      [`(no-context) (c->rt p)]
      [else p])))

(define (c->rt p)
  (printf "c->rt ~s\n" p)
  (match p
    [`((left ,x) . ,rest)
     (cons (p->rt x) (c->rt rest))]
    [`((right ,x) . ,rest)
     (cons (c->rt rest) (p->rt x))]
    [`(no-context)
     'hole]))

(define (b->rb b)
  (for/list ([b (in-list b)])
    (for/list ([pr (in-list b)])
      (match pr
        [`(,x ,p) `(,x ,(p->rt p))]))))

(b->rb
 (matches (lang-lang b)
          (rp->p (lang-nts b) '(in-hole C a))
          (rp->p (lang-nts b) '(+ 1 2))))

#|
(lang-lang b)
'([C (:hole 
      (:cons + (:cons (:nt C) (:cons (:nt a) empty)))
      (:cons + (:cons (:nt a) (:cons (:nt C) empty))))]
  [a ((:cons + (:cons (:nt a) (:cons (:nt a) empty)))
      1 
      2)])
|#