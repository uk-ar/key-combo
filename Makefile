EMACS=emacs

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l test/run-tests.el
