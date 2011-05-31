#lang racket/base
#|

This file does the translation from
Redex-looking notation to the notation
for the sem-sem/ directory models

Also has hide-hole removing capabilities

|#

(require racket/match
         racket/contract
         rackunit
         "../sem-sem/syntax-directed-match-total.rkt"
         "../sem-sem/patterns.rkt")

(require redex/reduction-semantics
         (for-syntax racket/base))

(provide define-double-language
         define-double-extended-language
         
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
;; kwds : (listof symbol)
;; grammar : sexp representation of the original, input grammar (used for language extension)
(struct lang (nts lang kwds grammar) #:transparent)

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

(define (grammar->lang orig-grammar extensions)
  (define grammar (combine-grammars orig-grammar extensions))
  (define nts (map car grammar))
  (define kwds (find-kwds grammar))
  (lang nts
        (append (map (λ (nt/prods)
                       (define nt (car nt/prods))
                       (define prods (cdr nt/prods))
                       `(,nt (,@(map (λ (prod) (rp->p nts kwds prod #f))
                                     prods))))
                     grammar)
                built-in-nts)
        kwds
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

(define (find-kwds grammar)
  (define nts (append '(variable-not-otherwise-mentioned in-hole hole number)
                      (map car built-in-nts)
                      (map car grammar)))
  (define kwds (make-hash))
  (let loop ([x (map cdr grammar)])
    (cond
      [(pair? x)
       (loop (car x))
       (loop (cdr x))]
      [(symbol? x)
       (unless (member x nts)
         (hash-set! kwds x #t))]))
  (sort (hash-map kwds (λ (x y) x))
        string<=?
        #:key symbol->string))

;; add this non-terminal into each language 
;; and translate number constants back and
;; forth from this notation; eg the number
;; 123 is (:cons 1 (:cons 2 (:cons 3 empty)))
(define built-in-nts
  `([num ((:cons num (:nt digits)))]
    [var ((:cons var (:nt digits)))]
    [digits ((:cons 0 (:nt digits))
             (:cons 1 (:nt digits))
             (:cons 2 (:nt digits))
             (:cons 3 (:nt digits))
             (:cons 4 (:nt digits))
             (:cons 5 (:nt digits))
             (:cons 6 (:nt digits))
             (:cons 7 (:nt digits))
             (:cons 8 (:nt digits))
             (:cons 9 (:nt digits))
             empty)]))

(define/contract (rt->t nts kwds pat)
  (-> (listof symbol?) (listof symbol?) any/c any/c)
  (let loop ([pat pat])
    (match pat
      ['hole ':hole]
      [(? number?) (number->p-number pat)]
      [(? pair?) `(:cons ,(loop (car pat)) ,(loop (cdr pat)))]
      [(? null?) `empty]
      [(? symbol?) 
       (cond
         [(member pat kwds) pat]
         [else (symbol->p-symbol pat)])])))

(define/contract (rp->p nts kwds pat [name-nts? #t])
  (->* ((listof symbol?) (listof symbol?) any/c) (boolean?) any/c)
  (let loop ([pat pat])
    (match pat
      ['hole ':hole]
      [(? number?) (number->p-number pat)]
      ['variable-not-otherwise-mentioned
       ;; this one is simpler than the 'number' case because
       ;; we assume that no examples do things like variable-not-otherwise-mentioned_1
       ;; and non of the examples actually expect variable-not-otherwise-mentioned
       ;; to be in the result binding table of a match
       `(:nt var)]
      [(? symbol?)
       (define-values (prefix has-suffix?)
         (let ([m (regexp-match #rx"^([^_]+)_(.*)" (symbol->string pat))])
           (if m
               (values (string->symbol (list-ref m 1)) #t)
               (values pat #f))))
       (cond
         [(eq? prefix 'number)
          (if name-nts?
              `(:name ,pat (:nt num))
              `(:nt num))]
         [(memq prefix nts)
          (if name-nts?
              `(:name ,pat (:nt ,prefix))
              `(:nt ,prefix))]
         [(member pat kwds) pat]
         [else (symbol->p-symbol pat)])]
      [`(hide-hole ,p) (loop p)]
      [`(in-hole ,p1 ,p2) `(:in-hole ,(loop p1) ,(loop p2))]
      [(? pair?) `(:cons ,(loop (car pat)) ,(loop (cdr pat)))]
      [(? null?) `empty])))

(define known-vars-table
  '((x 0)
    (y 1)
    (f 2)
    (g 3)
    (k 4)
    (blahblah 5)))

(define (known-variable? v)
  (for/or ([x (in-list known-vars-table)])
    (eq? (car x) v)))

(define/contract (symbol->p-symbol v)
  (-> symbol? any/c)
  (unless (known-variable? v)
    (error 'double.rkt "found a variable that doesn't have an entry in the table: ~s" v))
  `(:cons var ,(number->p-digits (cadr (assoc v known-vars-table)))))

(define/contract (number->p-number n)
  (-> exact-nonnegative-integer? any)
  `(:cons num
          ,(number->p-digits n)))

(define (number->p-digits n)
  (define digits
    (let loop ([n n])
      (cond
        [(= n 0) '()]
        [else (cons (modulo n 10) (loop (quotient n 10)))])))
  (let loop ([ds (reverse digits)])
    (cond
      [(null? ds) 'empty]
      [else `(:cons ,(car ds) ,(loop (cdr ds)))])))

(check-equal? (number->p-number 0) '(:cons num empty))
(check-equal? (number->p-number 1) '(:cons num (:cons 1 empty)))
(check-equal? (number->p-number 2) '(:cons num (:cons 2 empty)))
(check-equal? (number->p-number 10) '(:cons num (:cons 1 (:cons 0 empty))))
(check-equal? (number->p-number 1234567890) 
              `(:cons num (:cons 1 (:cons 2 (:cons 3 (:cons 4 (:cons 5 (:cons 6 (:cons 7 (:cons 8 (:cons 9 (:cons 0 empty))))))))))))

(define (p->rt p)
  (let loop ([p p])
    (match p
      [`:hole (term hole)]
      [`(:in-hole ,a ,b) `(in-hole ,(loop a) ,(loop b))]
      [`(:cons var ,stuff) (p-variable->variable stuff)]
      [`(:cons num ,stuff) (p-digits->number stuff)]
      [`(:cons ,a ,b) (cons (loop a) (loop b))]
      [`empty '()]
      [`(:name ,x (:nt ,b)) x]
      [`(:name ,x ,p) `(name ,x ,(loop p))]
      [`(:nt ,x) x]
      [`((:right ,x) . ,stuff) (c->rt p)]
      [`((:left ,x) . ,stuff) (c->rt p)]
      [`:no-context (c->rt p)]
      [else p])))

(define/contract (p-digits->number stuff)
  (-> any/c exact-nonnegative-integer?)
  (define digits
    (let loop ([stuff stuff])
      (match stuff
        [`(:cons ,d ,n)
         (cons d (loop n))]
        [`empty '()])))
  (let loop ([digits (reverse digits)])
    (cond
      [(null? digits) 0]
      [else
       (+ (car digits)
          (* 10 (loop (cdr digits))))])))

(define (p-variable->variable stuff)
  (define n (p-digits->number stuff))
  (define v (assoc n (map reverse known-vars-table)))
  (unless v
    (error 'p-variable->variable "unknown encoded variable: ~s ~s" stuff n))
  (cadr v))

(check-equal? (p->rt '(:cons num empty)) 0)
(check-equal? (p->rt '(:cons num (:cons 1 empty))) 1)
(check-equal? (p->rt '(:cons num (:cons 1 (:cons 2 empty)))) 12)
(check-equal? (p->rt '(:cons num (:cons 1 (:cons 2 (:cons 3 empty))))) 123)

(define (c->rt p)
  (match p
    [`((:right ,x) ,rest)
     (cons (p->rt x) (c->rt rest))]
    [`((:left ,x) ,rest)
     (cons (c->rt rest) (p->rt x))]
    [`:no-context
     (term hole)]))

(define (b->rb b)
  (normalize-bindings
   (and (not (null? b))
        (for/list ([b (in-list b)])
          (for/list ([pr (in-list b)])
            (match pr
              [`(pair ,x ,p) `(,x ,(p->rt p))]))))))

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
       (sort (for/list ([binding (in-list bindings)])
               (sort binding string<=? #:key (compose symbol->string car)))
             string<=?
             #:key (λ (x) (format "~s" x))
             #:cache-keys? #t)))

(define (sem-sem-match lang r-pat r-term)
  (define pat (rp->p (lang-nts lang) (lang-kwds lang) r-pat))
  (define trm (rt->t (lang-nts lang) (lang-kwds lang) r-term))
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