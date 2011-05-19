main.pdf: DOES_NOT_EXIST 
	raco make main.scrbl
	scribble ++extra floatflt.sty --pdf main.scrbl

tex: DOES_NOT_EXIST
	raco make main.scrbl
	scribble ++extra floatflt.sty --latex --dest scratch main.scrbl
	(cd scratch ; pdflatex main | grep floatflt)

DOES_NOT_EXIST:
