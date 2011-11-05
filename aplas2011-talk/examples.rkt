#lang racket/base
(require slideshow
         redex
         (only-in redex/private/reduction-semantics make-match)
         racket/sandbox
         framework
         racket/gui/base
         slideshow/code
         "util.rkt"
         "../aplas2011/2-models/double.rkt"
         "../aplas2011/2-models/models.rkt")
(provide example flush-examples)

(define-syntax-rule
  (example lang :lang nts-to-drop _pat _term stuff ...)
  (save-slides
   lang
   'nts-to-drop
   (pat _pat)
   (pat _term)
   (sem-sem-match :lang '_pat '_term)
   (list 'lang
         '_pat
         '_term)
   (λ ()
     (redex-match 
      lang
      _pat
      (term _term)))
   stuff ...))

(define examples-cache '())
(define example-thunks '())

(define (save-slides lang nts-to-drop _pat _term sem-sem-answer input-pict thunk #:out-of-memory? [out-of-memory? #f])
  (define l-p (pair-em (t "Language:")
                       (render-language lang #:nts (remove* nts-to-drop (language-nts lang)))))
  (define p-p (pair-em (t "Pattern:")
                       _pat))
  (define t-p (pair-em (t "Term:")
                       _term))
  (set! examples-cache
        (append examples-cache 
                (list (list l-p p-p t-p (blank))
                      (list l-p p-p t-p
                            (pair-em (t (if (or (not (list? sem-sem-answer)) (= (length sem-sem-answer) 1))
                                            "Answer:"
                                            "Answers:"))
                                     (render-sem-sem-answer sem-sem-answer))))))
  (let ([t (if (and #f out-of-memory?)
               (λ ()
                 (error "ran out of memory"))
               thunk)])
    (set! example-thunks (append example-thunks (list (list t input-pict) (list t input-pict))))))

(define (flush-examples)
  (when (null? examples-cache)
    (error 'flush-examples "not examples saved"))
  (define backgrounds (map (λ (l) (launder (ghost (apply cc-superimpose l)))) (transpose examples-cache)))
  (for ([example (in-list examples-cache)]
        [thunk-pair (in-list example-thunks)])
    (define (combine i) (lt-superimpose (list-ref example i) (list-ref backgrounds i)))
    (define main
      (vl-append
       20
       (combine 0)
       (combine 1)
       (combine 2)
       (combine 3)))
    (slide
     (vl-append (rc-superimpose (blank (pict-width main) 0)
                                (inset (mk-button thunk-pair)
                                       0 0 0 (- (pict-height (t "something")))))
                main)))
  (set! examples-cache '())
  (set! example-thunks '()))

(define (transpose m) (apply map list m))

(define-syntax-rule 
  (with-pat-fonts exp)
  (parameterize ([current-main-font (default-style)]
                 [current-font-size (default-font-size)])
    exp))

(define (render-sem-sem-answer sem-sem-answer)
  (cond
    [(list? sem-sem-answer)
     (apply
      vl-append
      10
      (for/list ([table (in-list sem-sem-answer)])
        (apply para
               (add-between
                (for/list ([bind (in-list table)])
                  (hbl-append (render-sexp (list-ref bind 0))
                              (t " = ")
                              (render-sexp (list-ref bind 1))))
                (t ", ")))))]
    [else
     (with-pat-fonts 
      (t (format "~s" sem-sem-answer)))]))

  
(define (mk-button thunk-pair)
  (define thunk (list-ref thunk-pair 0))
  (define input-pict (list-ref thunk-pair 1))
  (define button-label (inset (t "Run in Redex") 20 10))
  (clickback
   (cc-superimpose (linewidth 10
                              (rounded-rectangle (pict-width button-label) 
                                                 (pict-height button-label)))
                   button-label)
   (λ () (render-output thunk input-pict))))


(define (pair-em a b)
  (vl-append a (hc-append (blank 20 0) b)))



;                                                   
;                                                                                                                      
;                               ;;;                 
;                                                   
;  ;;; ;;;; ;;; ;;; ;;  ;;; ;;  ;;; ;;; ;;   ;; ;;; 
;  ;;;;;;;; ;;; ;;;;;;; ;;;;;;; ;;; ;;;;;;; ;;;;;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; 
;  ;;;  ;;;;;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;;;;; 
;  ;;;   ;; ;;; ;;; ;;; ;;; ;;; ;;; ;;; ;;;  ;; ;;; 
;                                               ;;; 
;                                           ;;;;;;  
;                                                                                                                      
;                                                                                                                      


(define e (parameterize ([sandbox-security-guard (current-security-guard)])
            (make-evaluator 'racket
                            '(require redex))))

(define (render-output thnk input-details)
  (define res (with-handlers ((exn:fail? values))
                (call-in-sandbox-context e thnk)))
  (define-values (w h) (get-display-size))
  (define f (new frame% [label "Redex’s Answer"] [width w] [height h]))
  (define mb (new menu-bar% [parent f]))
  (define fm (new menu% [label "File"] [parent mb]))
  (define mi (new menu-item% [label "Close"] [parent fm] [shortcut #\w] [callback (λ x (send f show #f))]))
  (send f center 'both)
  
  (define drawer (make-pict-drawer (vl-append (build-input-pict input-details)
                                              (build-output-pict res))))
  (define c (new canvas% 
                 [parent f]
                 [paint-callback
                  (λ (c dc)
                    (drawer dc 0 0))]))
  (send f show #t))

(define (build-output-pict res)
  (cond
    [(exn:fail? res)
     (colorize (it (exn-message res)) "red")]
    [else
     
     (define (rewrite-hole exp)
       (let loop ([exp exp])
         (cond
           [(pair? exp)
            (cons (loop (car exp))
                  (loop (cdr exp)))]
           [(match? exp)
            (make-match (loop (match-bindings exp)))]
           [(bind? exp)
            (make-bind (loop (bind-name exp))
                       (loop (bind-exp exp)))]
           [(equal? exp (term hole)) 'hole]
           [else exp])))
     
     (define sp (open-output-string))
     (parameterize ([pretty-print-columns 40])
       (pretty-print (rewrite-hole res) sp))
     (define rp (open-input-string (get-output-string sp)))
     (apply
      vl-append
      (let loop ()
        (let ([l (read-line rp)])
          (cond
            [(eof-object? l) '()]
            [else (cons (tt l) (loop))]))))]))

(define (build-input-pict input-details)
  (define lang (list-ref input-details 0))
  (define pat (list-ref input-details 1))
  (define term (list-ref input-details 2))
  (htl-append (tt "> ")
              (vl-append
               (tt "(redex-match")
               (tt (format "  ~s" lang))
               (tt (format "  ~s" pat))
               (tt (format "  (term ~s))" term)))))

(define red-italic (make-object style-delta% 'change-italic))
(void (send red-italic set-delta-foreground "red"))
