dist: DOES_NOT_EXIST
	rm -rf dist
	mkdir dist
	mkdir dist/aplas-semantics
	cd aplas11; make main.pdf; make aplas2011-kmjf.pdf
	cp aplas11/main.pdf dist/aplas2011-kmjf-revised-short.pdf
	cp aplas11/aplas2011-kmjf.pdf dist/aplas2011-kmjf-revised-long.pdf
	git clone . submitted
	cd submitted; git reset --hard f54f5187f7b45e33e6d6305ebaff6ca9e81e92be
	mv submitted/sem-sem dist/aplas-semantics
	mv submitted/2-models dist/aplas-semantics
	mv submitted/run-tests.rkt dist/aplas-semantics
	rm -rf submitted
	cd dist; tar czf aplas-semantics.tar.gz aplas-semantics
	rm -rf dist/aplas-semantics
	cp aplas11/final.pdf dist/aplas2011-kmjf.pdf
	cp dist-src/index.html dist
	cp dist-src/README dist

DOES_NOT_EXIST:
