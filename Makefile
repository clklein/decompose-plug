dist: DOES_NOT_EXIST
	rm -rf dist
	mkdir dist
	mkdir dist/aplas-semantics
	cd aplas2011; make main.pdf
	cp aplas2011/main.pdf dist/aplas2011-kmjf-revised.pdf
	git clone . submitted
	cd submitted; git reset --hard f54f5187f7b45e33e6d6305ebaff6ca9e81e92be
	mv submitted/sem-sem dist/aplas-semantics
	mv submitted/2-models dist/aplas-semantics
	mv submitted/run-tests.rkt dist/aplas-semantics
	cp dist-src/README dist/aplas-semantics
	rm -rf submitted
	cd dist; tar czf aplas-semantics.tar.gz aplas-semantics
	rm -rf dist/aplas-semantics
	cp aplas2011/final.pdf dist/aplas2011-kmjf.pdf
	cp dist-src/index.html dist

DOES_NOT_EXIST:
