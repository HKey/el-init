EMACS ?= emacs
CASK  ?= cask
MAKE  ?= make

.PHONY: clean compile test full-test coverage

clean:
	${CASK} clean-elc

compile:
	${EMACS} --version
	${CASK} exec ${EMACS} -Q -batch -L . -eval \
	"(progn \
	   (when (version<= \"24.3\" emacs-version) \
	     (setq byte-compile-error-on-warn t)) \
	   (batch-byte-compile))" el-init.el

test:
	${CASK} exec ert-runner

full-test:
	${MAKE} clean
	${MAKE} test
	${MAKE} compile
	${MAKE} test

# do not compile when using undercover.el
coverage:
	${MAKE} clean
	${MAKE} test
