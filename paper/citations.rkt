#lang at-exp racket
(require scribble/manual
         scriblib/autobib)

(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography)

;; ----------------------------------------

(define ACM "ACM ")
(define IEEE "IEEE ")
(define IEEE/ACM "IEEE/ACM ")
(define SIGPLAN "SIGPLAN ")
(define International "Intl. ")
(define Conference "Conf. ")
(define Symposium "Symp. ")
(define Workshop "Wksp. ")
(define _Workshop " Wksp.")
(define Journal "J. ")
(define European "Euro. ")
(define Software "Soft. ")
(define Engineering "Eng. ")
(define _Engineering "Eng.")
(define Computational "Comput. ")
(define Transactions "Trans. ")

(define icfp (string-append ACM International Conference "Functional Programming"))
(define pldi (string-append ACM Conference "Programming Language Design and Implementation"))
(define popl (string-append ACM Symposium "Principles of Programming Languages"))
(define lfp "Lisp and Functional Programming")
(define lsc "Lisp and Symbolic Computation")
(define lncs "Lecture Notes in Computer Science")
(define scheme-workshop "Scheme and Functional Programming")
(define sigplan-notices (string-append ACM "SIGPLAN Notices"))
(define esec (string-append European Software Engineering Conference))
(define fse (string-append ACM Symposium "Foundations of " Software Engineering))
(define esec/fse (string-append "Joint " esec " and " fse))
(define ase-ieee/acm (string-append IEEE/ACM International Conference "Automated " Software Engineering))
(define ase-ieee (string-append IEEE International Conference "Automated " Software Engineering))
(define icse (string-append International Conference Software Engineering))
(define icse-companion (string-append International Conference Software _Engineering ": Companion Volume"))
(define issta (string-append International Symposium Software "Testing and Analysis"))
(define tacas (string-append International Conference "Tools and Algorithms for the Construction and Analysis of Systems"))
(define tap (string-append International Conference "Tests and Proofs"))
(define taic-part (string-append "Testing: Academia & Industry Conference -- Practice & Research Techniques"))
(define soqua (string-append International Workshop Software "Quality"))
(define haskell (string-append ACM SIGPLAN "Haskell " _Workshop))
(define padl (string-append International Workshop "Practical Aspects of Declarative Languages"))
(define cav (string-append International Conference "Computer Aided Verification"))
(define ccs (string-append ACM Conference "Computer and Communications Security"))
(define omcp (string-append International Conference "Objects, Models, Components, Patterns"))
(define esop (string-append European Symposium "Programming"))
(define cade (string-append International Conference "Automated Deduction"))
(define iclp (string-append International Conference "Logic Programming"))
(define rta (string-append International Conference "Rewriting Techniques and Applications"))
(define tphols (string-append International Conference "Theorem Proving in Higher Order Logics"))
(define sefm (string-append IEEE International Conference "Software Engineering and Formal Methods"))
(define sde (string-append Software Engineering Symposium "Practical Software Development Environments"))
(define ppdp (string-append International Conference "Principles and Practice of Declarative Programming"))
(define oopsla (string-append ACM Conference "Object-Oriented Programming, Systems, Languages, and Applications"))
(define acl2 (string-append International Workshop "ACL2 Theorem Prover and its Applications"))
(define itp (string-append International Conference "Interactive Theorem Proving"))

(define pepm (string-append ACM SIGPLAN Workshop "Partial Evaluation and Program Manipulation"))
(define iwpt "International Conference on Parsing Technology")
(define sofsem "Seminar on Current Trends in Theory and Practice of Informatics")

(define ibm-sys "IBM Systems Journal")
(define jfp (string-append Journal "Functional Programming"))
(define scp "Science of Computer Programming")
(define spe "Software -- Practice & Experience")
(define cacm "Communications of the ACM")
(define tcs "Theoretical Computer Science")
(define tocl (string-append ACM Transactions Computational "Logic"))

;; ----------------------------------------


(define icfp2007-fyff
  (make-bib
   #:author (authors "Matthew Flatt" "Gang Yu" "Robert Bruce Findler" "Matthias Felleisen")
   #:title "Adding Delimited and Composable Control to a Production Programming Environment"
   #:location (proceedings-location icfp #:pages '(165 176))
   #:date "2007"))

(define redex
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

(define redex-parti (in-bib redex ", part I"))

(define sfp2009-kf
  (make-bib
   #:author (authors "Casey Klein" "Robert Bruce Findler")
   #:title "Randomized Testing in PLT Redex"
   #:location (proceedings-location scheme-workshop #:pages '(26 36))
   #:date "2009"))

(define QuickCheck
  (make-bib
   #:author (authors "Koen Claessen" "John Hughes")
   #:title "QuickCheck: A Lightweight Tool for Random Testing of Haskell Programs"
   #:location (proceedings-location icfp #:pages '(268 279))
   #:date "2000"))

(define esop2001-cff
  (make-bib
   #:author (authors "John Clements" "Matthew Flatt" "Matthias Felleisen")
   #:title "Modeling an Algebraic Stepper"
   #:location (proceedings-location esop #:pages '(320 334))
   #:date 2001))

(define Hanford
  (make-bib
   #:author (authors "Kenneth V. Hanford")
   #:title "Automatic Generation of Test Cases"
   #:location (journal-location ibm-sys
                                #:volume 9
                                #:number "4"
                                #:pages '(244 257))
   #:date "1970"))

(define Racket-VM
  (make-bib
   #:author (authors "Casey Klein" "Matthew Flatt" "Robert Bruce Findler")
   #:title "The Racket Virtual Machine and Randomized Testing"
   #:url "http://plt.eecs.northwestern.edu/racket-machine/"
   #:date 2011))

(define icfp2005-pcmkf
  (make-bib
   #:author (authors "Greg Pettyjohn" "John Clements" "Joe Marshall" "Shriram Krishnamurthi" "Matthias Felleisen")
   #:title "Continuations from Generalized Stack Inspection"
   #:location (proceedings-location icfp #:pages '(216 227))
   #:date "2005"))

(define hughes-arrows
  (make-bib
   #:author (authors "John Hughes")
   #:title "Generalizing Monads to Arrows"
   #:location (journal-location scp
                                #:volume 37
                                #:number "1--3"
                                #:pages '(67 111))
   #:date "2000"))

(define pldi1993-fsdf
  (make-bib
   #:author (authors "Cormac Flanagan" "Amr Sabry" "Bruce F. Duba" "Matthias Felleisen")
   #:title "The Essence of Compiling with Continuations"
   #:location (proceedings-location icfp #:pages '(237 247))
   #:date "1993"))

(define CML
  (make-bib
   #:author "John H. Reppy"
   #:title "Concurrent Programming in ML"
   #:is-book? #t
   #:location (book-location #:publisher "Cambridge University Press")
   #:date "1999"))

(define norell-dissertation
  (make-bib
   #:author "Ulf Norell"
   #:title "Towards a Practical Programming Language Based on Dependent Type Theory"
   #:location (dissertation-location #:institution "Chalmers University of Technology"
                                     #:degree "PhD")
   #:date "2007"))

(define Concurrent-Haskell
  (make-bib
   #:author (authors "Simon Peyton Jones" "Andrew Gordon" "Sigbjorn Finne")
   #:title "Concurrent Haskell"
   #:location (proceedings-location popl #:pages '(295 308))
   #:date "1996"))

(define fuzz-unix
  (make-bib
   #:author (authors "Barton P. Miller" "Lars Fredriksen" "Bryan So")
   #:title "An Empirical Study of the Reliability of UNIX Utilities"
   #:location (journal-location cacm
                                #:volume 33
                                #:number "12"
                                #:pages '(32 44))
   #:date "1990"))

(define coq
  (make-bib
   #:author (org-author-name "The Coq Development Team")
   #:title "The Coq Proof Assistant Reference Manual"
   #:location "Version 8.3"
   #:url "http://coq.inria.fr/"
   #:date "2010"))

(define twelf
  (make-bib
   #:author (authors "Frank Pfenning" "Carsten Schürmann")
   #:title "System description: Twelf---A Meta-Logical Framework for Deductive Systems"
   #:location (proceedings-location cade #:pages '(202 206))
   #:date "1999"))

(define ott
  (make-bib
   #:author (authors "Peter Sewell" "Francesco Zappa Nardelli" "Scott Owens" 
                     "Gilles Peskine" "Thomas Ridge" "Susmit Sarkar" "Rok Strniša")
   #:title "Ott: Effective Tool Support for the Working Semanticist"
   #:location (journal-location jfp
                                #:volume 20
                                #:number "1"
                                #:pages '(71 122))
   #:date "2010"))

(define αML
  (make-bib
   #:author "Matthew R. Lakin"
   #:title "An Executable Meta-Language for Inductive Definitions with Binders"
   #:location (dissertation-location #:institution "University of Cambridge"
                                     #:degree "PhD")
   #:date "2010"))

(define αProlog
  (make-bib
   #:author (authors "James Cheney" "Christian Urban")
   #:title "αProlog: A Logic Programming Language with Names, Binding, and α-Equivalence"
   #:location (proceedings-location iclp
                                    #:series lncs
                                    #:volume 3132
                                    #:pages '(269 283))
   #:date "2004"))

(define rta2004-mfff
  (make-bib
   #:author (authors "Jacob Matthews" "Robert Bruce Findler" "Matthew Flatt" "Matthias Felleisen")
   #:title "A Visual Environment for Developing Context-Sensitive Term Rewriting Systems"
   #:location (proceedings-location rta
                                    #:series lncs
                                    #:volume 3091
                                    #:pages '(301 311))
   #:date "2004"))

(define DrScheme
  (make-bib 
   #:title "DrScheme: A Programming Environment for Scheme"
   #:author (authors "Robert Bruce Findler" "John Clements" "Cormac Flanagan" 
                     "Matthew Flatt" "Shriram Krishnamurthi"  "Paul Steckler"
                     "Matthias Felleisen")
   #:date "2002"
   #:location (journal-location jfp
                                #:volume 12
                                #:number "2"
                                #:pages '(159 182))))

(define PLaneT-cite
  (make-bib 
   #:title "Component Deployment with PLaneT: You Want it Where?"
   #:author "Jacob Matthews"
   #:date "2006"
   #:location (proceedings-location scheme-workshop)))


(define R6RS
  (make-bib
   #:title "Revised [6] Report on the Algorithmic Language Scheme"
   #:date "2007"
   #:author (authors "Michael Sperber"
                     "R. Kent Dybvig"
                     "Matthew Flatt"
                     "Anton van Straaten"
                     "Richard Kelsey"
                     "William Clinger"
                     "Jonathan Rees"
                     "Robert Bruce Findler"
                     "Jacob Matthews")
   #:location (book-location #:publisher "Cambridge University Press")))

(define PoplMark
  (make-bib
   #:author (authors "Brian E. Aydemir" "Aaron Bohannon" "Matthew Fairbairn"
                     "J. Nathan Foster" "Benjamin C. Pierce" "Peter Sewell"
                     "Dimitrios Vytiniotis" "Geoffrey Washburn" "Stephanie Weirich"
                     "Steve Zdancewic")
   #:title "Mechanized Metatheory for the Masses: The PoplMark Challenge"
   #:location (proceedings-location tphols
                                    #:series lncs
                                    #:volume 3603
                                    #:pages '(50 65))
   #:date "2005"))

(define isabelle
  (make-bib
   #:title "Isabelle/HOL---A Proof Assistant for Higher-Order Logic"
   #:date "2011"
   #:author (authors "Tobias Nipkow" "Lawrence C. Paulson" "Markus Wenzel")
   #:location (book-location #:publisher "Springer Verlag")))

(define HOL
  (make-bib
   #:author (authors "Konrad Slind" "Michael Norrish")
   #:title "A Brief Overview of HOL4"
   #:location (proceedings-location tphols
                                    #:series lncs
                                    #:volume 5170
                                    #:pages '(28 32))
   #:date "2008"))

(define isabelle-quickcheck
  (make-bib 
   #:title "Random Testing in Isabelle/HOL"
   #:author (authors "Stefan Berghofer" "Tobias Nipkow")
   #:date "2004"
   #:location (proceedings-location sefm)))

(define (INRIA-location no)
  (techrpt-location #:institution "INRIA" 
                    #:number (format "Research Report No. ~a" no)))

(define mentor
  (make-bib 
   #:title "Programming Environments Based on Structured Editors: The Mentor Experience"
   #:author (authors "Véronique Donzeau-Gouge" "Gérard Huet" "Gilles Kahn" "Bernard Lang")
   #:date "1980"
   #:location (INRIA-location 26)))

(define centaur
  (make-bib 
   #:title "Centaur: The System"
   #:author (authors "Patrick Borras" "Dominique Clément" "Thierry Despeyroux"
                     "Janet Incerpi" "Gilles Kahn" "Bernard Lang" "Valérie Pascual")
   #:date "1988"
   #:location (proceedings-location sde #:pages '(11 24))))

(define typol
  (make-bib 
   #:title "Executable Specification of Static Semantics"
   #:author (authors "Thierry Despeyroux")
   #:date "1984"
   #:location (INRIA-location 295)))

(define FNC-2
  (make-bib 
   #:title "Design, Implementation and Evaluation of the FNC-2 Attribute Grammar System"
   #:author (authors "Martin Jourdan" "Didier Parigot" "Catherine Julié"
                     "Olivier Durin" "Carole Le Bellec")
   #:date "1990"
   #:location (proceedings-location pldi #:pages '(209 222))))

(define minotaur
  (make-bib 
   #:title "Integrating Natural Semantics and Attribute Grammars:The Minotaur System"
   #:author (authors "Isabelle Attali" "Didier Parigot")
   #:date "1994"
   #:location (INRIA-location 2339)))

(define αProlog-test
  (make-bib 
   #:title "Mechanized Metatheory Model-Checking"
   #:author (authors "James Cheney" "Alberto Momigliano")
   #:date "2007"
   #:location (proceedings-location ppdp #:pages '(75 86))))

(define roberson-test
  (make-bib 
   #:title "Efficient Software Model Checking of Soundness of Type Systems"
   #:author (authors "Michael Roberson" "Melanie Harries" "Chandrasekhar Boyapati")
   #:date "2008"
   #:location (proceedings-location oopsla #:pages '(493 504))))

(define hardin-AAMP7G
  (make-bib 
   #:title "A Robust Machine Code Proof Framework for Highly Secure Applications"
   #:author (authors "David S. Hardin" "Eric W. Smith" "William D. Young")
   #:date "2006"
   #:location (proceedings-location acl2 #:pages '(11 20))))

(define fox-ARM
  (make-bib
   #:author (authors "Anthony Fox")
   #:title "Formal Specification and Verification of ARM6"
   #:location (proceedings-location tphols
                                    #:series lncs
                                    #:volume 2758
                                    #:pages '(25 40))
   #:date "2003"))

(define fox-myreen-ARM
  (make-bib
   #:author (authors "Anthony Fox" "Magnus O. Myreen")
   #:title "A Trustworthy Monadic Formalization of the ARMv7 Instruction Set Architecture "
   #:location (proceedings-location itp
                                    #:series lncs
                                    #:volume 6172
                                    #:pages '(243 258))
   #:date "2010"))

(define sarkar-x86
  (make-bib
   #:author (authors "Susmit Sarkar" "Peter Sewell" "Francesco Zappa Nardelli"
                     "Scott Owens" "Tom Ridge" "Thomas Braibant"
                     "Magnus O. Myreen" "Jade Alglave")
   #:title "The Semantics of x86-CC Multiprocessor Machine Code"
   #:location (proceedings-location popl #:pages '(379 391))
   #:date "2009"))

(define racket-VM
  (make-bib
   #:author (authors "Casey Klein" "Matthew Flatt" "Robert Bruce Findler")
   #:title "The Racket Virtual Machine and Randomized Testing"
   #:url "http://plt.eecs.northwestern.edu/racket-machine/"
   #:date "2011"))

(define maude2
  (make-bib
   #:author (authors "Manuel Clavel" "Francisco Durán" 
                     "Steven Eker" "Patrick Lincoln"
                     "Narciso Martí-Oliet" "José Meseguer"
                     "Carolyn Talcott")
   #:title "Maude 2.0 System"
   #:date 2003
   #:location (proceedings-location rta 
                                    #:pages '(76 87)
                                    #:series lncs
                                    #:volume 2706)))

(define LF-algorithms
  (make-bib
   #:author (authors "Robert Harper" "Frank Pfenning")
   #:title "On Equivalence and Canonical Forms in the LF Type Theory"
   #:location (journal-location tocl
                                #:volume 6
                                #:number "1"
                                #:pages '(61 101))
   #:date "2005"))

(define λzap
  (make-bib
   #:author (authors "David Walker" "Lester Mackey" "Jay Ligatti"
                     "George A. Reis" "David I. August")
   #:title "Static Typing for a Faulty Lambda Calculus"
   #:location (proceedings-location icfp #:pages '(38 49))
   #:date "2006"))

(define weber-dissertation
  (make-bib
   #:author "Tjark Weber"
   #:title "SAT-based Finite Model Generation for Higher-Order Logic"
   #:location (dissertation-location #:institution "Technische Universität München"
                                     #:degree "PhD")
   #:date "2008"))

(define nitpick
  (make-bib
   #:author (authors "Jasmin Christian Blanchette" "Tobias Nipkow")
   #:title "Nitpick: A Counterexample Generator for Higher-Order Logic Based on a Relation Model Finder"
   #:location (proceedings-location itp
                                    #:series lncs
                                    #:volume 6172
                                    #:pages '(131 146))
   #:date "2010"))

(define PLT-TR-2010-1
  (make-bib
   #:author (authors "Matthew Flatt" "PLT")
   #:title "Reference: Racket"
   #:location (techrpt-location #:institution "PLT Inc."
                                #:number "PLT-TR-2010-1")
   #:date "2010"
   #:url "http://racket-lang.org/tr1/"))

(define scheme2005-mf
  (make-bib
   #:title "An Operational Semantics for R5RS Scheme"
   #:author (authors "Jacob Matthews" "Robert Bruce Findler")
   #:location (proceedings-location scheme-workshop)
   #:date 2005))

(define felleisen-hieb
  (make-bib
   #:author (authors "Matthias Felleisen" "Robert Hieb")
   #:title "The Revised Report on the Syntactic Theories of Squential Control and State"
   #:location (journal-location tcs)
   #:date "1991"))

(define context-sensitive-rewriting-fundamentals
  (make-bib
   #:author "Salvador Lucas"
   #:title "Fundamentals of Context-sensitive rewriting"
   #:location (proceedings-location sofsem)
   #:date "1995"))

(define cbn-calculus
  (make-bib
   #:author (authors "Zena M. Ariola" "Matthias Felleisen")
   #:title "The Call-by-Need Lambda-Calculus"
   #:location (journal-location jfp
                                #:volume 7
                                #:number 3
                                #:pages '(265 301))
   #:date "1997"))
  
(define berendrecht 
  (make-bib 
    #:author "H. P. Barendregt"
    #:title "The Lambda Calculus: Its Syntax and Semantics"
    #:location (book-location #:publisher "North Holland")
    #:date 1984))

(define racket-vm
  (make-bib
    #:title "The Racket Virtual Machine and Randomized Testing"
    #:author (authors "Casey Klein" "Matthew Flatt" "Robert Bruce Findler")
    #:url "http://plt.eecs.northwestern.edu/racket-machine/"
    #:date "to appear"))

(define example-of-using-contexts-with-explicit-deompose-and-plug
  (make-bib
   #:title "An Operational Foundation for Delimited Continuations in the CPS Hierarchy"
   #:author (authors "Mal Gorzata Biernacka"
                     "Dariusz Biernacki"
                     "Olivier Danvy")
   #:location (journal-location "Logical Methods in Computer Science"
                                #:volume 1)
   
   #:date 2005))

(define kuno-cacm65
  (make-bib
   #:title "The Predictive Analyzer and a Path Elimination Technique"
   #:author "Susumu Kuno"
   #:location (journal-location cacm
                                #:volume 8
                                #:number "7"
                                #:pages '(453 462))
   #:date 1965))

(define frost-iwpt07
  (make-bib
   #:title "Modular and Efficient Top-Down Parsing for Ambiguous Left-Recursive Grammars"
   #:author (authors "Richard A. Frost"
                     "Rahmatullah Hafiz"
                     "Paul C. Callaghan")
   #:location (proceedings-location iwpt #:pages '(109 120))
   #:date 2007))
(define frost-iwpt07-sec3 (in-bib frost-iwpt07 ", section 3"))

(define warth-pepm08
  (make-bib
   #:title "Packrat Parsers Can Support Left Recursion"
   #:author (authors "Alessandro Warth"
                     "James R. Douglass"
                     "Todd Millstein")
   #:location (proceedings-location pepm #:pages '(103 110))
   #:date 2008))