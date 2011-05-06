main.pdf: DOES_NOT_EXIST 
	scribble --latex --prefix eptcs-prefix.tex ++style style.tex main.scrbl
	pdflatex main.tex
	pdflatex main.tex
	rm -f main.aux main.log main.out main.pag main.tex texput.log

DOES_NOT_EXIST:
