EMACS ?= emacs
CASK ?= cask

all: test

test: clean-elc
	${MAKE} unit
	${MAKE} file-test
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

unit:
	${CASK} exec ert-runner test/flycheck-dmd-dub-pure-test.el

file-test:
	${CASK} exec ert-runner test/flycheck-dmd-dub-file-test.el

compile:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile flycheck-dmd-dub.el

clean-elc:
	rm -f flycheck-dmd-dub.elc

.PHONY:	all test unit
