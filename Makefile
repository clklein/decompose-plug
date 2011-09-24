dist: DOES_NOT_EXIST
	rm -rf dist
	mkdir dist
	cp aplas2011/final.pdf dist/aplas2011-kmjf.pdf
	cp aplas2011/final-extended.pdf dist/aplas2011-kmjf-extended.pdf
	git clone . submitted
	cd submitted; git reset --hard f54f5187f7b45e33e6d6305ebaff6ca9e81e92be
	mkdir dist/aplas-semantics
	mv submitted/sem-sem dist/aplas-semantics
	mv submitted/2-models dist/aplas-semantics
	mv submitted/run-tests.rkt dist/aplas-semantics
	cp dist-src/README dist/aplas-semantics
	rm -rf submitted
	cd dist; tar czf aplas-semantics.tar.gz aplas-semantics
	rm -rf dist/aplas-semantics
	cp dist-src/index.html dist

DOES_NOT_EXIST:
