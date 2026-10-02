EMACS ?= emacs
BATCH = $(EMACS) --batch -Q --eval '(progn (require (quote package)) (package-initialize) (setq load-prefer-newer t))' -L .

.PHONY: check test compile clean

check: test compile

test:
	$(BATCH) -l test/agent-shell-sessions-tests.el -f ert-run-tests-batch-and-exit

compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' -f batch-byte-compile agent-shell-sessions.el

clean:
	rm -f agent-shell-sessions.elc
