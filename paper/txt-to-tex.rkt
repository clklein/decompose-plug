#lang racket/base
(require racket/cmdline)

(define txt-files
  (command-line 
   #:program "txt-to-tex.rkt"
   #:args args args))

(define max-line-width 86)

;; txt-to-tex : string[path] -> void
;; reads in the file, munges the contents to be latex-friendly, and then writes it out.
(define (txt-to-tex file)
  (define tex-file (txt-to-tex-name file))
  (printf "converting ~a into ~a\n" file tex-file)
  (call-with-input-file file
    (λ (in-port)
      (call-with-output-file tex-file
        (λ (out-port)
          (let loop ([line-number 1])
            (let ([line (read-line in-port)])
              (cond
                [(eof-object? line) (void)]
                [else
                 
                 (unless (<= (string-length line) max-line-width)
                   (fprintf (current-error-port) "line ~a too long in ~a:\n~a\n\n" 
                            line-number
                            file
                            line))
                 
                 (cond
                   [(regexp-match #rx"-*- Mode" line)
                    (void)]
                   [(regexp-match #rx"^ *$" line)
                    (fprintf out-port "\\texttt{~~}\\\\\n")]
                   [else
                    (let ([bold? (regexp-match #rx"^[*]" line)])
                      (fprintf out-port "\\texttt{\\small{}\\hbox{}")
                      (when bold? (fprintf out-port "\\textbf{"))
                      (fprintf out-port "~a" (escape-latex line))
                      (when bold? (fprintf out-port "}"))
                      (fprintf out-port "}\\\\\n"))])
                 (loop (+ line-number 1))]))))
        #:exists 'truncate))))

;; escape-latex : string -> string
(define (escape-latex str)
  (apply string-append (map latex-translate (string->list str))))

;; latex-translate : char -> string
(define (latex-translate x)
  (case x 
    [(#\~) "\\~{}"]
    [(#\^) "{}$^{}${}"]
    [(#\#) "\\#"]
    [(#\{) "\\char`\\{"]
    [(#\}) "\\char`\\}"]
    [(#\$) "\\char`\\$"]
    [(#\%) "\\char`\\%"]
    [(#\&) "\\char`\\&"]
    [(#\_) "\\char`\\_"]
    [(#\\) "\\char`\\\\"]
    [(#\space) "~"]
    [(#\&) "\\&"]
    [(#\∈) "\\ensuremath{\\in}"]
    [(#\⊢) "\\ensuremath{\\vdash}"]
    [(#\≠) "\\ensuremath{\\neq}"]
    [(#\⊔) "\\ensuremath{\\sqcup}"]
    [(#\⊤) "\\ensuremath{\\top}"]
    [(#\•) "\\ensuremath{\\bullet}"]
    [else
     (unless (<= (char->integer x) 127)
       (eprintf "WARNING: cannot translate ~a\n" x))
     (string x)]))

;; txt-to-tex-name : string -> string
;; converts the name of a file from its .txt to its latex name
(define (txt-to-tex-name file)
  (define-values (base name dir) (split-path file))
  (bytes->path (regexp-replace #rx"\\.txt$" (path->bytes name) #"-txt.tex")))

;; go!
(for-each txt-to-tex txt-files)

