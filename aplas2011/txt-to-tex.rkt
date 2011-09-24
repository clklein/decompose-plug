#lang racket/base
(require racket/cmdline
         racket/match)

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
                   [(equal? "BEGIN-INLINE-LATEX" line)
                    (let loop ()
                      (define next-line (read-line in-port))
                      (when (eof-object? next-line)
                        (error 'txt-to-tex "undelimited inline Latex"))
                      (unless (equal? "END-INLINE-LATEX" next-line)
                        (displayln next-line out-port)
                        (loop)))]
                   [(regexp-match #rx"-*- Mode" line)
                    (void)]
                   [(regexp-match #rx"^ *$" line)
                    (fprintf out-port "\\texttt{~~}\\\\\n")]
                   [else
                    (let ([bold? (regexp-match #rx"^[*]" line)])
                      (fprintf out-port "\\texttt{\\small{}\\hbox{}")
                      (when bold? (fprintf out-port "\\textbf{"))
                      (fprintf out-port "~a" (latex-translate (string->list line)))
                      (when bold? (fprintf out-port "}"))
                      (fprintf out-port "}\\\\\n"))])
                 (loop (+ line-number 1))]))))
        #:exists 'truncate))))

;; latex-translate : (listof char) -> string
(define (latex-translate chars)
  (if (null? chars)
      ""
      (let loop ([table translation-table])
        (match table
          ['()
           (unless (<= (char->integer (car chars)) 127)
             (show-warning (car chars)))
           (string-append (string (car chars)) 
                          (latex-translate (cdr chars)))]
          [(cons (list from to) rest-table)
           (cond [(starts-with? chars from)
                  => (λ (rest-chars)
                       (string-append to (latex-translate rest-chars)))]
                 [else (loop rest-table)])]))))

(define translation-table
  '(["[| " "\\ensuremath{\\llbracket}"]
    [" |]" "\\ensuremath{\\rrbracket}"]
    ["->_G^*" "\\ensuremath{\\rightarrow_G^*}"]
    ["->_G" "\\ensuremath{\\rightarrow_G}"]
    ["-/>_G^*" "\\ensuremath{\\not \\rightarrow_G^*}"]
    ["Lemma" "{\\bf Lemma}"]
    ["Proof" "{\\it Proof}"]
    ["~" "\\~{}"]
    ["^" "{}$^{}${}"]
    ["#" "\\#"]
    ["{" "\\char`\\{"]
    ["}" "\\char`\\}"]
    ["$" "\\char`\\$"]
    ["%" "\\char`\\%"]
    ["&" "\\char`\\&"]
    ["_" "\\char`\\_"]
    ["\\" "\\char`\\\\"]
    [" " "~"]
    ["&" "\\&"]
    ["∈" "\\ensuremath{\\in}"]
    ["⊢" "\\ensuremath{\\vdash}"]
    ["≠" "\\ensuremath{\\neq}"]
    ["=" "\\ensuremath{=}"]
    ["⊔" "\\ensuremath{\\sqcup}"]
    ["⊤" "\\ensuremath{\\top}"]
    ["•" "\\ensuremath{\\bullet}"]
    ["⊆" "\\ensuremath{\\subseteq}"]
    ["≤" "\\ensuremath{\\leq}"]
    ["<" "\\ensuremath{<}"]
    ["∅" "\\ensuremath{\\emptyset}"]))

(define (starts-with? given-chars string)
  (let loop ([expected-chars (string->list string)]
             [given-chars given-chars])
    (match* (expected-chars given-chars)
            [((list) _) given-chars]
            [((cons e es) (cons g gs))
             (if (equal? e g)
                 (loop es gs)
                 #f)]
            [(_ _) #f])))

(define warning-shown '())
(define (show-warning x)
  (unless (member x warning-shown)
    (set! warning-shown (cons x warning-shown))
    (eprintf "WARNING: cannot translate ~a\n" x)))

;; txt-to-tex-name : string -> string
;; converts the name of a file from its .txt to its latex name
(define (txt-to-tex-name file)
  (define-values (base name dir) (split-path file))
  (bytes->path (regexp-replace #rx"\\.txt$" (path->bytes name) #"-txt.tex")))

;; go!
(for-each txt-to-tex txt-files)

