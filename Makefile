EMACS ?= emacs
CASK ?= cask

.PHONY: all
all: test

.PHONY: test
test: clean-elc
	${MAKE} unit
	${MAKE} file-test
	${MAKE} compile
	${MAKE} unit
	${MAKE} clean-elc

.PHONY: unit
unit:
	${CASK} exec ert-runner test/flycheck-dmd-dub-pure-test.el

.PHONY: file-test
file-test:
	${CASK} exec ert-runner test/flycheck-dmd-dub-file-test.el

.PHONY: compile
compile: flycheck-dmd-dub.elc

flycheck-dmd-dub.elc:
	${CASK} exec ${EMACS} -Q -batch -f batch-byte-compile flycheck-dmd-dub.el

.PHONY: clean-elc
clean-elc:
	rm -f *.elc
