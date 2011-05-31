#lang racket/base
(require slideshow
         redex
         racket/sandbox
         framework
         racket/gui/base
         "util.rkt"
         "../2-models/double.rkt"
         "../2-models/models.rkt")
(provide example)

(define-syntax-rule
  (example lang :lang nts-to-drop _pat _term stuff ...)
  (mk-slide
   lang
   'nts-to-drop
   (pat _pat)
   (pat _term)
   (sem-sem-match :lang '_pat '_term)
   (λ ()
     (render-output 
      (redex-match 
       lang
       _pat
       (term _term))))
   stuff ...))

(define (mk-slide lang nts-to-drop _pat _term sem-sem-answer thunk #:out-of-memory? [out-of-memory? #f])
  (define main
    (vl-append
     20
     (pair-em (t "Language:")
              (render-language lang #:nts (remove* nts-to-drop (language-nts lang))))
     (pair-em (t "Pattern:")
              _pat)
     (pair-em (t "Term:")
              _term)
     (pair-em (t (if (and (list? sem-sem-answer) (= (length sem-sem-answer) 1)) "Answer:" "Answers:"))
              (render-sem-sem-answer sem-sem-answer))))
  (slide
   (vl-append (rc-superimpose (blank (pict-width main) 0)
                              (mk-button (if out-of-memory?
                                             (λ ()
                                               (error "ran out of memory"))
                                             thunk)))
              main)))

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

  
(define (mk-button thunk)
  (define button-label (inset (t "Run in Redex") 20 10))
  (clickback
   (cc-superimpose (linewidth 10
                              (rounded-rectangle (pict-width button-label) 
                                                 (pict-height button-label)))
                   button-label)
   thunk))

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

(define (render-output/proc thnk)
  (define res (with-handlers ((exn:fail? values))
                (call-in-sandbox-context e thnk)))
  (define-values (w h) (get-display-size))
  (define f (new frame% [label "Redex’s Answer"] [width w] [height h]))
  (define mb (new menu-bar% [parent f]))
  (define fm (new menu% [label "File"] [parent mb]))
  (define mi (new menu-item% [label "Close"] [parent fm] [shortcut #\w] [callback (λ x (send f show #f))]))
  (send f center 'both)
  (define t (if (exn:fail? res)
                (new text%)
                (new scheme:text%)))
  (define ec (new editor-canvas% [parent f] [editor t]))
  (define (il str)
    (send t insert str (send t last-position) (send t last-position)))
  (cond
    [(exn:fail? res)
     (define before (send t last-position))
     (il (exn-message res))
     (define after (send t last-position))
     (send t change-style red-italic before after)]
    [(and (list? res) (andmap match? res))
     (for ([match (in-list res)])
       (for ([bind (in-list (match-bindings match))])
         (define str (format "~s = " (bind-name bind)))
         (il str)
         (pretty-write (bind-exp bind) (open-output-text-editor t)))
       (il "\n"))]
    [else
     (pretty-write res (open-output-text-editor t))])
  (send t hide-caret #t)
  (send f show #t))

(define red-italic (make-object style-delta% 'change-italic))
(void (send red-italic set-delta-foreground "red"))


(define-syntax-rule 
  (render-output e)
  (render-output/proc (λ () e)))
