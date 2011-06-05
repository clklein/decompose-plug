#lang racket/base

(require "patterns.rkt"
         "common.rkt"
         redex/reduction-semantics
         unstable/debug
         unstable/contract
         racket/contract
         racket/dict
         racket/set
         racket/match)
(provide/contract
 [matches (-> grammar? pattern? term? (listof bindings?))])

; decomposition : (or/c decomp no-decomp)
; binding : bindings?
(define-struct mtch (decomposition binding) #:transparent)

(define-struct decomp (context contractum) #:transparent)
(define-struct no-decomp () #:transparent)

(define empty-bindings '(set))

; matches : grammar pattern term -> (set/c mtch)
; grammar ≡ (dict symbol (listof pattern))
(define (matches lang pat term)
  (define memo-table (make-hash))
  (define continue? #false)
  (define (update-memo-table x t ds)
    (unless (equal? ds (hash-ref memo-table (cons x t) (set)))
      (hash-set! memo-table (cons x t) ds)
      (set! continue? #true)))
  (define (memo-table-lookup x t)
    (hash-ref memo-table (cons x t) (set)))
  
  (define (go pat term seen)
    (match pat
      [':hole
       (if (eq? ':hole term)
           (set (mtch (decomp ':no-ctxt ':hole) empty-bindings)
                (mtch (no-decomp) empty-bindings))
           (set (mtch (decomp ':no-ctxt term) empty-bindings)))]
      [(? atom?)
       (if (eq? pat term)
           (set (mtch (no-decomp) empty-bindings))
           (set))]
      [`(:name ,x ,p)
       (==> (go p term seen)
            (match-lambda
              [(mtch d b)
               (match (⊔/proc `(set (pair ,x ,(named d term))) b)
                 ['⊤ (set)]
                 [b+ (set (mtch d b+))])]))]
      [`(:in-hole ,p1 ,p2)
       (==> (go p1 term seen)
            (match-lambda
              [(mtch (and (decomp C t1)) b1)
               (==> (go p2 t1 (if (eq? C ':no-ctxt) seen (set)))
                    (match-lambda
                      [(mtch d2 b2)
                       (merge-decomp C b1 d2 b2)]))]
              [(mtch (no-decomp) b)
               (set)]))]
      [`(:cons ,p1 ,p2)
       (match term
         [`(:cons ,t1 ,t2)
          (==> (go p1 t1 (set))
               (λ (m1)
                 (==> (go p2 t2 (set))
                      (λ (m2) (merge-cons m1 t1 m2 t2)))))]
         [_ (set)])]
      [`(:nt ,x)
       (if (set-member? seen x)
           (memo-table-lookup x term)
           (let ([ms (apply set-union
                            (map (λ (p)
                                   (==> (go p term (set-add seen x)) 
                                        (λ (m) 
                                          (set (mtch (mtch-decomposition m) 
                                                     empty-bindings)))))
                                 (car (dict-ref lang x))))])
             (update-memo-table x term ms)
             ms))]))
  
  (let loop ()
    (define all-matches (go pat term (set)))
    (if continue?
        (begin
          (set! continue? #false)
          (loop))
        (for/fold ([top-matches '()]) ([m all-matches])
                  (match m
                    [(mtch (decomp C t) b)
                     top-matches]
                    [(mtch (no-decomp) b)
                     (cons b top-matches)])))))

(define (named d t)
  (match d
    [(decomp C u) C]
    [(no-decomp) t]))

(define (merge-decomp C1 b1 d2 b2)
  (match (⊔/proc b1 b2)
    ['⊤ (set)]
    [b (match d2
         [(decomp C2 t2)
          (set (mtch (decomp (term (append-contexts ,C1 ,C2)) t2) b))]
         [(no-decomp)
          (set (mtch (no-decomp) b))])]))

(define (merge-cons m1 t1 m2 t2)
  (match* (m1 m2)
          [((mtch d1 b1) (mtch d2 b2))
           (match (⊔/proc b1 b2)
             ['⊤ (set)]
             [b (match* (d1 d2)
                        [((no-decomp) (no-decomp))
                         (set (mtch (no-decomp) b))]
                        [((decomp C t) (no-decomp))
                         (set (mtch (decomp `(:left ,t2 ,C) t) b))]
                        [((no-decomp) (decomp C t))
                         (set (mtch (decomp `(:right ,t1 ,C) t) b))]
                        [((decomp C1 u1) (decomp C2 u2))
                         (set)])])]))

(define (==> xs f)
  (for/fold ([ys (set)]) ([x xs])
    (set-union (f x) ys)))

(define atom? (redex-match patterns a))