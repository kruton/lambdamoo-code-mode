.PHONY: test check

EMACS ?= emacs
EMACSFLAGS ?= -Q

test:
	$(EMACS) $(EMACSFLAGS) --batch -L . -L test -l test/lambdamoo-code-mode-test.el -f ert-run-tests-batch-and-exit

check:
	$(EMACS) $(EMACSFLAGS) --batch -L . --eval '(progn (require (quote bytecomp)) (let ((byte-compile-error-on-warn t) (byte-compile-dest-file-function (lambda (_) "/tmp/lambdamoo-code-mode.elc"))) (byte-compile-file "lambdamoo-code-mode.el")))'
