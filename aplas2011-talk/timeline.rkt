#lang racket
(require slideshow)
(provide timeline)

(define-syntax-rule 
  (size exp)
  (parameterize ([current-font-size (floor (* (current-font-size) #e1.5))])
    exp))

(define moments
  (size
   (list (list 2002 (t "First prototype; an op. sem. calculator"))
         (list 2003 #f)
         (list 2004 (hbl-append (t "RTA paper (now feels ") (bt "so") (t " out of date)")))
         (list 2005 #f)
         (list 2006 #f)
         (list 2007 (t "Workshop on Redex (31 come)"))
         (list 2008 (t "Semantics of R6RS in Redex"))
         (list 2009 (t "Redex book published"))
         (list 2010 (t "Racket VM spec in Redex"))
         (list 2011 (t "First external funding for Redex"))
         (list 2011 (t "Started thinking about a compiler ....")))))

(define (timeline)
  (slide
   (apply 
    vl-append
    6
    (for/list ([moment (in-list moments)])
      (define year (t (format "~a" (list-ref moment 0))))
      (define comment (list-ref moment 1))
      (define line (htl-append 20 year (or comment (size (t "x")))))
      (if comment
          line
          (ghost line))))))
