main.pdf: DOES_NOT_EXIST core-algorithm-correctness-txt.tex
	raco make main.scrbl
	scribble ++extra floatflt.sty ++style correctness-statement.tex ++extra core-algorithm-correctness-txt.tex --pdf main.scrbl

aplas2011-kmjf.pdf: DOES_NOT_EXIST core-algorithm-correctness-txt.tex
	raco make main.scrbl
	env EXTENDED_VERSION=yes scribble ++extra floatflt.sty ++style correctness-statement.tex ++extra core-algorithm-correctness-txt.tex --pdf --dest-name aplas2011-kmjf.pdf main.scrbl

tex: DOES_NOT_EXIST core-algorithm-correctness-txt.tex
	raco make main.scrbl
	scribble ++extra floatflt.sty ++style correctness-statement.tex ++extra core-algorithm-correctness-txt.tex --latex --dest scratch main.scrbl
	(cd scratch ; pdflatex main | grep floatflt)

core-algorithm-correctness-txt.tex: core-algorithm-correctness.txt txt-to-tex.rkt
	racket txt-to-tex.rkt core-algorithm-correctness.txt 

DOES_NOT_EXIST:
