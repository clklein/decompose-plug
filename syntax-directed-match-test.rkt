#lang racket

(require redex "syntax-directed-match.rkt")

(define-syntaxes (test-matches test-doesnt-match)
  (let ([make-test
         (λ (test-spec no-match?)
           (syntax-case test-spec ()
             [(_ L p t)
              (quasisyntax/loc test-spec
                (test-equal (empty? (term (match-top L p t))) #,no-match?))]))])
    (values (λ (stx) (make-test stx #'false))
            (λ (stx) (make-test stx #'true)))))

(test-matches () a a)
(test-doesnt-match () a b)
(test-equal (term (match-top () (:name x a) a))
            '(((x a))))

(test-matches () (:cons a b) (:cons a b))
(test-doesnt-match () (:cons a c) (:cons a b))
(test-doesnt-match () (:cons c b) (:cons a b))

(test-doesnt-match () (:cons (:name x a) (:name x b)) (:cons a b))
(test-equal (term (match-top () (:cons (:name x a) (:name x a)) (:cons a a)))
            '(((x a))))
(test-equal (term (match-top () (:cons (:name x a) (:name y b)) (:cons a b)))
            '(((y b) (x a))))

(term-let ([ones-lang (term ([ones (mt (:cons 1 (:nt ones)))]))])
          (test-matches ones-lang (:nt ones) mt)
          (test-matches ones-lang (:nt ones) (:cons 1 mt))
          (test-matches ones-lang (:nt ones) (:cons 1 (:cons 1 mt)))
          (test-doesnt-match ones-lang (:nt ones) (:cons 1 (:cons 2 mt))))

(test-matches ([n ((:name x b))]) (:cons (:name x a) (:nt n)) (:cons a b))

(test-matches () :hole :hole)
(test-matches () (:in-hole :hole a) a)
(test-doesnt-match () (:in-hole :hole a) b)

(test-matches () (:in-hole (:cons a :hole) b) (:cons a b))
(test-matches () (:in-hole (:cons :hole a) b) (:cons b a))

(test-matches () (:in-hole (:in-hole :hole :hole) a) a)
(test-matches () 
              (:in-hole (:in-hole (:cons a :hole) (:cons b :hole)) c) 
              (:cons a (:cons b c)))

(test-equal (term
             (match-top () 
                        (:in-hole (:cons :hole a) (:name x b))
                        (:cons b a)))
            '(((x b))))
(test-matches () 
              (:cons (:name x b) (:in-hole (:cons :hole a) (:name x b)))
              (:cons b (:cons b a)))
(test-doesnt-match () 
                   (:cons (:name x c) (:in-hole (:cons :hole a) (:name x b)))
                   (:cons c (:cons b a)))

(test-equal
 (term
  (match-top ()
             (:in-hole (:name x (:cons a (:cons :hole c))) b) 
             (:cons a (:cons b c))))
 '(((x ((right a) ((left c) no-frame))))))

(test-matches 
 ()
 (:in-hole
  (:cons (:name C (:cons :hole b))
         (:name C (:cons :hole b)))
  a)
 (:cons (:cons :hole b)
        (:cons a b)))
(test-equal
 (term
  (match-top ()
             (:cons (:name C (:cons :hole b))
                    (:name C (:cons :hole b)))
             (:cons (:cons :hole b)
                    (:cons :hole b))))
 '(((C ((left b) no-frame)))))

(test-equal
 (term (match-top ()
                  (:in-hole (:name x (:in-hole (:cons a :hole)
                                               (:cons :hole c)))
                            b)
                  (:cons a (:cons b c))))
 '(((x ((right a) ((left c) no-frame))))))

(define vars-a-to-z
  (build-list 
   26
   (λ (i) 
     (string->symbol
      (list->string
       (list (integer->char (+ i (char->integer #\a)))))))))

(define encode-term
  (match-lambda
    ['() 'mt]
    [(cons t u)
     `(:cons ,(encode-term t)
             ,(encode-term u))]
    [t t]))

(define decode-term
  (term-match/single
   directed-matching
   [no-frame (term :hole)]
   [((left t) C)
    (cons (decode-term (term C))
          (decode-term (term t)))]
   [((right t) C)
    (cons (decode-term (term t))
          (decode-term (term C)))]
   [mt '()]
   [(:cons t_1 t_2)
    (cons (decode-term (term t_1))
          (decode-term (term t_2)))]
   [a (term a)]))

(define decode-bindings
  (match-lambda
    ['() '()]
    [(cons `([,xs ,vs] ...) bs)
     (cons (map (λ (x v) `[,x ,(decode-term v)]) xs vs)
           (decode-bindings bs))]))

(term-let ([λv (term
                ([e ((:cons (:nt e) (:cons (:nt e) mt))
                     (:nt x)
                     (:nt v))]
                 [x ,vars-a-to-z]
                 [v ((:cons λ (:cons (:cons (:nt x) mt) (:cons (:nt e) mt)))
                     (:nt E))]
                 [E (:hole 
                     (:cons (:nt E) (:cons (:nt e) mt))
                     (:cons (:nt v) (:cons (:nt E) mt)))]))])
          (test-matches λv (:nt v) ,(encode-term '(λ (x) x)))
          (test-matches λv (:nt e) ,(encode-term '(λ (x) x)))
          (test-matches λv (:nt v) ,(encode-term '(λ (x) (x x))))
          
          (test-equal
           (decode-bindings
            (term 
             (match-top λv 
                        (:in-hole (:name E (:nt E)) (:name e (:nt e)))
                        ,(encode-term '((λ (x) x) (λ (y) y))))))
           `(((e ((λ (x) x) (λ (y) y))) (E :hole))
             ((e (λ (x) x)) (E (:hole (λ (y) y))))
             ((e (λ (y) y)) (E ((λ (x) x) :hole)))))
          
          (test-equal
           (decode-bindings
            (term 
             (match-top λv 
                        (:in-hole
                         (:name E (:nt E))
                         (:name r
                                (:cons (:cons λ (:cons (:cons x mt) (:cons (:nt e) mt)))
                                       (:cons (:nt v) mt))))
                        ,(encode-term '(:hole ((λ (x) x) (λ (y) y)))))))
           `(((r ((λ (x) x) (λ (y) y)))
              (E (:hole :hole))))))

(term-let ([L (term ([W (:hole
                         (:in-hole (:cons (:name x (:nt W)) mt)
                                   (:cons (:nt n) (:cons :hole mt))))]
                     [n (1 2 3 4 5)]))])
          (test-matches L (:nt W) :hole)
          (test-matches L (:nt W) ,(encode-term '((1 :hole))))
          (test-matches L (:nt W) ,(encode-term '((((1 (2 (3 :hole))))))))
          (test-doesnt-match L (:nt W) ,(encode-term '(((1 (2 (3 :hole))))))))

(term-let ([L (term ([W (:hole
                         (:cons (:in-hole (:name x (:nt W))
                                          (:cons (:nt n) (:cons :hole mt)))
                                (:cons (:name x (:nt W))
                                       mt)))]
                     [n (1 2 3 4 5)]))])
          (test-matches L (:nt W) ,(encode-term '((1 :hole) :hole)))
          (test-matches L (:nt W) ,(encode-term '(((1 (2 :hole)) :hole) 
                                                  ((1 :hole) :hole))))
          (test-matches L (:nt W) ,(encode-term '((((1 (2 (3 :hole))) :hole) ((1 :hole) :hole))
                                                  (((1 (2 :hole)) :hole) ((1 :hole) :hole)))))
          (test-equal
           (term
            (match-top L (:in-hole (:nt W) (:name n (:nt n)))
                       ,(encode-term '((((1 (2 (3 4))) :hole) ((1 :hole) :hole))
                                       (((1 (2 :hole)) :hole) ((1 :hole) :hole))))))
           '(((n 4)))))