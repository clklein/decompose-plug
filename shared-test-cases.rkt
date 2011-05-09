#lang racket

(require "patterns.rkt")
(provide run-tests
         equal-bindings?
         (struct-out test:match)
         (struct-out test:no-match)
         (struct-out test:bind))

(struct test (src-loc lang pat term) #:transparent)
(struct test:match test () #:transparent)
(struct test:no-match test () #:transparent)
(struct test:bind test (bindings) #:transparent)

(define (equal-bindings? bs cs)
  (define (sets bs)
    (apply set (for/list ([b bs]) (apply set b))))
  (equal? (sets bs) (sets cs)))

(define-syntax (build-test stx)
  (syntax-case stx ()
    [(_ ctor lang pat term rest-args ...)
     (with-syntax ([src-loc (format "~a:~a:~a"
                                    (syntax-source stx)
                                    (syntax-line stx)
                                    (syntax-column stx))])
       #'(ctor src-loc `lang `pat `term `rest-args ...))]))

(define unflattened-tests
  (list
   
   ;;;; tests originally written for syntax-directed matcher ;;;;
   
   (build-test test:match () a a)
   (build-test test:no-match () a b)
   
   (build-test test:bind () (:name x a) a
               (((x a))))
   
   (build-test test:match () (:cons a b) (:cons a b))
   (build-test test:no-match () (:cons a c) (:cons a b))
   (build-test test:no-match () (:cons c b) (:cons a b))
   
   (build-test test:no-match () (:cons (:name x a) (:name x b)) (:cons a b))
   (build-test test:bind () (:cons (:name x a) (:name x a)) (:cons a a)
               (((x a))))
   (build-test test:bind () (:cons (:name x a) (:name y b)) (:cons a b)
               (((y b) (x a))))
   
   (let ([ones-lang '([ones (mt (:cons 1 (:nt ones)))])])
     (list
      (build-test test:match ,ones-lang (:nt ones) mt)
      (build-test test:match ,ones-lang (:nt ones) (:cons 1 mt))
      (build-test test:match ,ones-lang (:nt ones) (:cons 1 (:cons 1 mt)))
      (build-test test:no-match ,ones-lang (:nt ones) (:cons 1 (:cons 2 mt)))))
   
   (build-test test:match
               ([n ((:name x b))])
               (:cons (:name x a) (:nt n))
               (:cons a b))
   
   (build-test test:match () :hole :hole)
   (build-test test:match () (:in-hole :hole a) a)
   (build-test test:no-match () (:in-hole :hole a) b)
   
   (build-test test:match () (:in-hole (:cons a :hole) b) (:cons a b))
   (build-test test:match () (:in-hole (:cons :hole a) b) (:cons b a))
   
   (build-test test:match () (:in-hole (:in-hole :hole :hole) a) a)
   (build-test test:match 
               () 
               (:in-hole (:in-hole (:cons a :hole) (:cons b :hole)) c) 
               (:cons a (:cons b c)))
   
   (build-test test:bind
               ()
               (:in-hole (:cons :hole a) (:name x b))
               (:cons b a)
               (((x b))))
   (build-test test:match
               () 
               (:cons (:name x b) (:in-hole (:cons :hole a) (:name x b)))
               (:cons b (:cons b a)))
   (build-test test:no-match
               () 
               (:cons (:name x c) (:in-hole (:cons :hole a) (:name x b)))
               (:cons c (:cons b a)))
   
   (build-test test:bind
               ()
               (:in-hole (:name x (:cons a (:cons :hole c))) b) 
               (:cons a (:cons b c))
               (((x ((right a) ((left c) no-frame))))))
   
   (build-test test:bind
               ()
               (:name C
                      (:cons (:name D (:cons :hole b))
                             mt))
               (:cons (:cons :hole b)
                      mt)
               (((C (:cons (:cons :hole b) mt))
                 (D (:cons :hole b)))))
   
   (build-test test:match 
               ()
               (:in-hole
                (:cons (:name C (:cons :hole b))
                       (:name C (:cons :hole b)))
                a)
               (:cons (:cons :hole b)
                      (:cons a b)))
   
   (build-test test:bind
               ()
               (:cons (:name C (:cons :hole b))
                      (:name C (:cons :hole b)))
               (:cons (:cons :hole b)
                      (:cons :hole b))
               (((C (:cons :hole b)))))
   
   (build-test test:bind
               ()
               (:in-hole
                (:cons (:cons :hole b)
                       (:cons :hole b))
                :hole)
               (:cons (:cons :hole b)
                      (:cons :hole b))
               ; would be '(() ()) without no-dups in (syntax-directed) match-top
               (()))
   
   (build-test test:bind
               ()
               (:in-hole
                (:name C
                       (:cons (:cons :hole b)
                              (:cons :hole b)))
                :hole)
               (:cons (:cons :hole b)
                      (:cons :hole b))
               (((C ((left (:cons :hole b)) ((left b) no-frame))))
                ((C ((right (:cons :hole b)) ((left b) no-frame))))))
   
   (build-test test:bind
               ()
               (:cons (:in-hole (:name x (:cons :hole a))
                                :hole)
                      (:name x (:cons :hole a)))
               (:cons (:cons :hole a)
                      (:cons :hole a))
               (((x ((left a) no-frame)))))
   
   (build-test test:bind
               ()
               (:in-hole (:name x (:in-hole (:cons a :hole)
                                            (:cons :hole c)))
                         b)
               (:cons a (:cons b c))
               (((x ((right a) ((left c) no-frame))))))
   
   (let ([λv `([e ((:cons (:nt e) (:cons (:nt e) mt))
                   (:nt x)
                   (:nt v))]
               [x ,(build-list 
                    26
                    (λ (i) 
                      (string->symbol
                       (list->string
                        (list (integer->char (+ i (char->integer #\a))))))))]
               [v ((:cons λ (:cons (:cons (:nt x) mt) (:cons (:nt e) mt)))
                   (:nt E))]
               [E (:hole 
                   (:cons (:nt E) (:cons (:nt e) mt))
                   (:cons (:nt v) (:cons (:nt E) mt)))])])
     (list (build-test test:match ,λv (:nt v) ,(encode-term '(λ (x) x)))
           (build-test test:match ,λv (:nt e) ,(encode-term '(λ (x) x)))
           (build-test test:match ,λv (:nt v) ,(encode-term '(λ (x) (x x))))
           
           (build-test test:bind
                       ,λv 
                       (:in-hole (:name E (:nt E)) (:name e (:nt e)))
                       ,(encode-term '((λ (x) x) (λ (y) y)))
                       (((e ,(encode-term '((λ (x) x) (λ (y) y))))
                         (E no-frame))
                        ((e ,(encode-term '(λ (x) x)))
                         (E ((left ,(encode-term '((λ (y) y))))
                             no-frame)))
                        ((e ,(encode-term '(λ (y) y)))
                         (E ((right ,(encode-term '(λ (x) x)))
                             ((left mt)
                              no-frame))))))
           
           (build-test test:bind
                       ,λv 
                       (:in-hole
                        (:name E (:nt E))
                        (:name r
                               (:cons (:cons λ (:cons (:cons x mt) (:cons (:nt e) mt)))
                                      (:cons (:nt v) mt))))
                       ,(encode-term '(:hole ((λ (x) x) (λ (y) y))))
                       (((r ,(encode-term '((λ (x) x) (λ (y) y))))
                         (E ((right :hole)
                             ((left mt)
                              no-frame))))))
           
           (build-test test:match ,λv (:nt v) ,(encode-term '(:hole :hole)))
           
           (build-test test:bind
                       ,λv
                       (:in-hole (:name E (:nt E)) (:name e (:nt e)))
                       ,(encode-term '(:hole :hole))
                       (((e (:cons :hole (:cons :hole mt)))
                         (E no-frame))
                        ((e :hole)
                         (E ((left (:cons :hole mt)) no-frame)))
                        ((e :hole)
                         (E ((right :hole) ((left mt) no-frame))))))
           
           (build-test test:bind
                       ,λv
                       (:cons (:cons λ 
                                     (:cons (:cons (:name x (:nt x)) mt) 
                                            (:cons (:name e (:nt e)) mt)))
                              (:cons (:name v (:nt v))
                                     mt))
                       ,(encode-term '((λ (y) y) (:hole :hole)))
                       (((v (:cons :hole (:cons :hole mt)))
                         (e y)
                         (x y))))))
   
   (let ([L '([W (:hole
                  (:in-hole (:cons (:name x (:nt W)) mt)
                            (:cons (:nt n) (:cons :hole mt))))]
              [n (1 2 3 4 5)])])
     (build-test test:match ,L (:nt W) :hole)
     (build-test test:match ,L (:nt W) ,(encode-term '((1 :hole))))
     (build-test test:match ,L (:nt W) ,(encode-term '((((1 (2 (3 :hole))))))))
     (build-test test:no-match ,L (:nt W) ,(encode-term '(((1 (2 (3 :hole))))))))
   
   (let ([L '([W (:hole
                  (:cons (:in-hole (:name x (:nt W))
                                   (:cons (:nt n) (:cons :hole mt)))
                         (:cons (:name x (:nt W))
                                mt)))]
              [n (1 2 3 4 5)])])
     (build-test test:match ,L (:nt W) ,(encode-term '((1 :hole) :hole)))
     (build-test test:match ,L (:nt W)
                 ,(encode-term '(((1 (2 :hole)) :hole) 
                                 ((1 :hole) :hole))))
     (build-test test:match ,L (:nt W) 
                 ,(encode-term '((((1 (2 (3 :hole))) :hole) ((1 :hole) :hole))
                                 (((1 (2 :hole)) :hole) ((1 :hole) :hole)))))
     (build-test test:bind
                 ,L (:in-hole (:nt W) (:name n (:nt n)))
                 ,(encode-term '((((1 (2 (3 4))) :hole) ((1 :hole) :hole))
                                 (((1 (2 :hole)) :hole) ((1 :hole) :hole))))
                 (((n 4)))))
   
   ;;;; tests originally written for non-syntax-directed matcher ;;;;
   
   (build-test test:match () a a)
   (build-test test:match () :hole :hole)
   (build-test test:no-match () a b)
   (build-test test:no-match () a (:cons b c))
   
   (build-test test:match () (:cons a b) (:cons a b))
   (build-test test:no-match () (:cons a b) (:cons a c))
   (build-test test:no-match () (:cons a b) (:cons c b))
   (build-test test:no-match () (:cons a b) c)
   
   (build-test test:bind () (:name x a) a (((x a))))
   (build-test test:bind () (:cons (:name x a) b) (:cons a b) (((x a))))
   
   (let ([ones-lang '((ones (mt (:cons 1 (:nt ones)))))])
     (list
      (build-test test:match ,ones-lang (:nt ones) mt)
      (build-test test:match ,ones-lang (:nt ones) (:cons 1 mt))
      (build-test test:match ,ones-lang (:nt ones) (:cons 1 (:cons 1 mt)))))
   
   (build-test test:match () (:in-hole :hole a) a)
   (build-test test:no-match () (:in-hole :hole a) b)
   
   (build-test test:bind () (:in-hole (:name x (:cons a :hole)) b) (:cons a b)
               (((x ((right a) no-frame)))))
   
   (build-test test:match () (:in-hole (:cons :hole b) a) (:cons a b))
   (build-test test:match () (:in-hole (:cons a :hole) b) (:cons a b))
   
   (build-test test:match ((n (:hole))) (:in-hole (:nt n) 2) 2)
   (build-test test:match
               ((n (:hole (:cons 1 (:nt n))))) 
               (:in-hole (:nt n) 2)
               (:cons 1 (:cons 1 (:cons 1 2))))
   
   (build-test test:bind () (:in-hole (:cons :hole :hole) :hole) (:cons :hole :hole)
               (()))
   (build-test test:bind
               ((hole-or-n (:hole (:nt n)))
                (n (1 2))) 
               (:in-hole (:name x
                                (:cons (:nt hole-or-n)
                                       (:nt hole-or-n)))
                         (:nt n))
               (:cons 1 2)
               (((x ((left 2) no-frame)))
                ((x ((right 1) no-frame)))))
   
   (build-test test:match
               () 
               (:in-hole (:in-hole (:cons a :hole) (:cons b :hole)) c)
               (:cons a (:cons b c)))
   (build-test test:bind
               () 
               (:in-hole (:name x (:in-hole (:cons a :hole) (:cons b :hole))) c)
               (:cons a (:cons b c))
               (((x ((right a) ((right b) no-frame))))))))

(define flattened-tests
  (foldr (λ (test/list tests)
           ((if (list? test/list) append cons) test/list tests))
         empty
         unflattened-tests))

(define (run-tests run-one #:show-passes? [show-passes? #f])
  (define num-failures 0)
  (define (fail)
    (set! num-failures (+ 1 num-failures)))
  
  (for ([test flattened-tests])
       (with-handlers ([exn:fail? (λ (exn)
                                    (fail)
                                    (displayln "Execution of the test:")
                                    (pretty-print test)
                                    (displayln "raises an exception:")
                                    (pretty-print exn))])
         (if (run-one test)
             (when show-passes?
               (printf "Pass: ~a\n" (test-src-loc test)))
             (begin
               (fail)
               (displayln "Failure:")
               (pretty-print test)))))
  (if (zero? num-failures)
      (printf "Passed all ~a tests.\n" (length flattened-tests))
      (printf "Failed ~a of ~a tests.\n" num-failures (length flattened-tests))))