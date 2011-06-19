#lang racket/base
#|

This file does the translation from
Redex-looking notation to the notation
for the sem-sem/ directory models

Also has hide-hole removing capabilities

|#

(require racket/match
         racket/list
         racket/set
         racket/contract
         rackunit
         "../sem-sem/syntax-directed-match-total.rkt"
         "../sem-sem/patterns.rkt"
         (only-in "../sem-sem/reduction.rkt" reductions reductions*))

(require redex/reduction-semantics
         (for-syntax racket/base))

(provide define-double-language
         define-double-extended-language
         
         double-reduction-relation
         reinterp-red-rel
         union-red-rels
         apply-red-rel
         apply-red-rel*
         
         test-double-reduction
         test-double-reduction*
         
         ;; (test-double-match lang :lang r-pat r-term p-bindings)
         ;; lang should be an identifier bound to a Redex language or (literally) #f
         ;;    if it is #f, then Redex isn't tested, only sem-sem
         ;; :lang should be bound to a lang struct (sem-sem language, as below)
         ;; r-pat should be a Redex pattern
         ;; r-term should be a Redex term, but without the (term ...) wrapper
         ;; bindings should be `(((,id ,r-term) ...) ...), ie a list of
         ;;    association tables that determine the match, or #f
         ;;    if this example should not matchs
         test-double-match
         
         sem-sem-match
         remove-hide-hole)

;; nts : (listof symbol)
;; lang : `([,nt ,pat ...] ...)   -- matches the language setup
;;                                -- of syntax directed match total
;; grammar : sexp representation of the original, input grammar (used for language extension)
(struct lang (nts lang grammar) #:transparent)

(define-syntax (define-double-language stx)
  (syntax-case stx ()
    [(_ id1 id2 (nt prods ...) ...)
     (begin
       (for ([nt (in-list (syntax->list #'(nt ...)))])
         (unless (identifier? nt)
           (raise-syntax-error #f "expected identifier" stx nt)))
       #'(begin
           (define id2 (grammar->lang '((nt prods ...) ...) '()))
           (define-language id1 (nt prods ...) ...)))]))

(struct red-rel (lang rules) #:transparent)
(struct rule (lhs rhs fresh) #:transparent)

(define ((make-red-rel-app app) model-rel redex-term)
  (map p->rt
       (app (lang-lang (red-rel-lang model-rel))
            (for/list ([r (red-rel-rules model-rel)])
              (match r
                [(rule lhs rhs fresh)
                 (list lhs rhs fresh)]))
            (rt->t redex-term))))
(define apply-red-rel (make-red-rel-app reductions))
(define apply-red-rel* (make-red-rel-app reductions*))

(define (reinterp-red-rel rel lang)
  (red-rel lang (red-rel-rules rel)))
(define (union-red-rels . rels)
  (unless (> (length rels) 1)
    (error 'union-red-rels "expected at least 2 but got ~s" (length rels)))
  (let check-same ([rs rels])
    (match rs
      [(list r) (void)]
      [(list-rest r s others)
       (if (equal? (red-rel-lang r) (red-rel-lang s))
           (check-same (cons s others))
           (error 'union-red-rels "expected relations on the same language"))]))
  (red-rel (red-rel-lang (car rels))
           (append-map red-rel-rules rels)))

(define-syntax (double-reduction-relation stx)
  (syntax-case stx (-->)
    [(_ lang :lang ([mf-name mf-impl] ...) (--> lhs rhs . extras) ...)
     #'(values (reduction-relation lang (--> lhs rhs . extras) ...)
               (red-rel :lang
                        (for/list ([l '(lhs ...)] [r '(rhs ...)] [e '(extras ...)])
                          (compile-red-rule :lang l r e 
                                            (list (cons 'mf-name mf-impl) ...)))))]))

(define (compile-red-rule lang lhs rhs extras mf-map)
  (define :lhs (rp->p (lang-nts lang) lhs))
  (define fresh (fresh-vars extras))
  (define bound (append fresh (bound-vars :lhs)))
  (rule :lhs
        (redex-rhs->model-rhs rhs bound mf-map)
        fresh))

(define (redex-rhs->model-rhs rhs bound mf-map)
  (define ((in xs) x) (member x xs))
  (define (mf r) (assoc r mf-map))
  (let translate ([r rhs])
    (match r
      [(? (in bound)) `(:var ,r)]
      [`(in-hole ,s ,t)
       `(:in-hole ,(translate s) ,(translate t))]
      [`() 'empty]
      [(cons (? mf f) args)
       `(:app ,(cdr (mf f)) ,(translate args))]
      [(cons s t)
       `(:cons ,(translate s) ,(translate t))]
      [_ r])))

(define (fresh-vars extras)
  (for/fold ([vars '()]) ([extra extras])
            (match extra
              [(or (? string?) (? symbol?)) vars]
              [`(fresh ,(? symbol? xs) ...)
               (append xs vars)])))

(define (bound-vars pat)
  (set-map
   (let bound ([p pat])
     (match p
       [(? atom?) (set)]
       [`(:name ,x ,p)
        (set-add (bound p) x)]
       [`(:nt ,n) (set)]
       [`(:in-hole ,p ,q) 
        (set-union (bound p) (bound q))]
       [`(:cons ,p ,q) 
        (set-union (bound p) (bound q))]
       [`:hole (set)]))
   values))

(define (grammar->lang orig-grammar extensions)
  (define grammar (combine-grammars orig-grammar extensions))
  (define nts (map car grammar))
  (lang nts
        (map (λ (nt/prods)
               (define nt (car nt/prods))
               (define prods (cdr nt/prods))
               `(,nt (,@(map (λ (prod) (rp->p nts prod #f))
                             prods))))
             grammar)
        grammar))

(define (combine-grammars orig-grammar extensions)
  (define extended-nts (map car extensions))
  (define not-mentioned-in-extensions
    (filter (λ (nt-prods)
              (not (member (car nt-prods) extended-nts)))
            orig-grammar))
  (define extensions-filled-out
    (for/list ([extension (in-list extensions)])
      (define nt (car extension))
      (define prods (cdr extension))
      (define orig (for/or ([prod (in-list orig-grammar)])
                     (and (eq? (car prod) nt)
                          prod)))
      (define full-prods
        (apply append
               (for/list ([x (in-list prods)])
                 (cond
                   [(equal? x '....)
                    (unless orig
                      (error 'combine-grammars "when building combined non-terminal ~s, could not find original productions"
                             nt))
                    orig]
                   [else
                    (list x)]))))
      `(,nt ,@full-prods)))
  (append extensions-filled-out
          not-mentioned-in-extensions))
          
(define-syntax (define-double-extended-language stx)
  (syntax-case stx ()
    [(_ id1 id1-orig id2 id2-orig (nt prods ...) ...)
     (begin
       (for ([nt (in-list (syntax->list #'(nt ...)))])
         (unless (identifier? nt)
           (raise-syntax-error #f "expected identifier" stx nt)))
       #'(begin
           (define-extended-language id1 id1-orig (nt prods ...) ...)
           (define id2 (grammar->lang (lang-grammar id2-orig)
                                      '((nt prods ...) ...)))))]))

(define/contract (rt->t pat)
  (-> any/c any/c)
  (let loop ([pat pat])
    (match pat
      ['hole ':hole]
      [(? number?) pat]
      [(? pair?) `(:cons ,(loop (car pat)) ,(loop (cdr pat)))]
      [(? null?) `empty]
      [(? symbol?) pat])))

(define/contract (rp->p nts pat [name-nts? #t])
  (->* ((listof symbol?) any/c) (boolean?) any/c)
  (let loop ([pat pat])
    (match pat
      ['hole ':hole]
      [(? number?) pat]
      ['variable-not-otherwise-mentioned
       ':variable]
      [(? symbol?)
       (define-values (prefix has-suffix?)
         (let ([m (regexp-match #rx"^([^_]+)_(.*)" (symbol->string pat))])
           (if m
               (values (string->symbol (list-ref m 1)) #t)
               (values pat #f))))
       (cond
         [(eq? prefix 'number)
          (if name-nts?
              `(:name ,pat :number)
              `:number)]
         [(memq prefix nts)
          (if name-nts?
              `(:name ,pat (:nt ,prefix))
              `(:nt ,prefix))]
         [else pat])]
      [`(hide-hole ,p) (loop p)]
      [`(in-hole ,p1 ,p2) `(:in-hole ,(loop p1) ,(loop p2))]
      [(? pair?) `(:cons ,(loop (car pat)) ,(loop (cdr pat)))]
      [(? null?) `empty])))

(define (p->rt p)
  (let loop ([p p])
    (match p
      [`:hole (term hole)]
      [`(:in-hole ,a ,b) `(in-hole ,(loop a) ,(loop b))]
      [`(:cons ,a ,b) (cons (loop a) (loop b))]
      [`empty '()]
      [`(:name ,x (:nt ,b)) x]
      [`(:name ,x ,p) `(name ,x ,(loop p))]
      [`(:nt ,x) x]
      [`(:right ,x . ,stuff) (c->rt p)]
      [`(:left ,x . ,stuff) (c->rt p)]
      [`:hole (c->rt p)]
      [else p])))

(define (c->rt p)
  (match p
    [`(:right ,x ,rest)
     (cons (p->rt x) (c->rt rest))]
    [`(:left ,rest ,x)
     (cons (c->rt rest) (p->rt x))]
    [`:hole
     (term hole)]))

(define (b->rb b)
  (normalize-bindings
   (and (not (null? b))
        (for/list ([b (in-list b)])
          (match b
            [`(set ,prs ...)
             (for/list ([pr (in-list prs)])
               (match pr
                 [`(pair ,x ,p) `(,x ,(p->rt p))]))])))))

(define-double-language a b
  (C hole (+ C a) (+ a C))
  (a (+ a a) 1 2))

(define (rb/f->b b)
  (normalize-bindings
   (and b
        (for/list ([mth (in-list b)])
          (for/list ([bd (in-list (match-bindings mth))])
            (list (bind-name bd)
                  (bind-exp bd)))))))

(define-syntax-rule (define-double-reduction-test name app :app)
  (... (define-syntax (name stx)
         (syntax-case stx ()
           [(_ #:norm norm rr :rr trm expected ...)
            #`(begin
                #,(syntax/loc #'rr
                    (test-equal
                     (sort-alphabetically (map norm (app rr trm)))
                     (sort-alphabetically (map norm (list expected ...)))))
                #,(syntax/loc #':rr
                    (test-equal
                     (sort-alphabetically (map norm (:app :rr trm)))
                     (sort-alphabetically (map norm (list expected ...))))))]
           [(_ rr :rr trm expected ...)
            #'(name #:norm values rr :rr trm expected ...)]))))
(define-double-reduction-test test-double-reduction apply-reduction-relation apply-red-rel)
(define-double-reduction-test test-double-reduction* apply-reduction-relation* apply-red-rel*)

(define-syntax (test-double-match stx)
  (syntax-case stx ()
    [(_ lang :lang pat trm bindings)
     #`(begin
         #,@(if (syntax-e #'lang)
                (list (syntax/loc #'lang
                        (test-equal (rb/f->b (redex-match lang pat (term trm)))
                                    (normalize-bindings (term bindings)))))
                (list))
         #,(syntax/loc #':lang
             (test-equal (sem-sem-match :lang 'pat 'trm)
                         (normalize-bindings (term bindings)))))]))

(define (normalize-bindings bindings)
  (and bindings
       (sort-alphabetically
        (for/list ([binding (in-list bindings)])
          (sort binding string<=? #:key (compose symbol->string car))))))

(define (sort-alphabetically xs)
  (sort xs string<=? 
        #:key (λ (x) (format "~s" x))
        #:cache-keys? #t))

(define (sem-sem-match lang r-pat r-term)
  (define pat (rp->p (lang-nts lang) r-pat))
  (define trm (rt->t r-term))
  (b->rb (matches (lang-lang lang) pat trm)))


(define-syntax (remove-hide-hole stx)
  (syntax-case stx ()
    [(_ arg)
     (let loop ([arg #'arg])
       (syntax-case arg (hide-hole)
         [(hide-hole exp) 
          (let ([res (loop #'exp)])
            (datum->syntax res (syntax-e res) arg))]
         [_
          (cond
            [(syntax? arg)
             (datum->syntax arg (loop (syntax-e arg)) arg)]
            [(pair? arg)
             (cons (loop (car arg))
                   (loop (cdr arg)))]
            [else arg])]))]))