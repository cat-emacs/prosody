EMACS ?= emacs
LISP := prosody.el prosody-use-package.el prosody-nerd-icons.el

.PHONY: lint build test

lint:
	$(EMACS) -Q --batch -L . \
		--eval "(dolist (file '(\"prosody.el\" \"prosody-use-package.el\" \"prosody-nerd-icons.el\" \"test/prosody-test.el\")) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens)))"

build:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $(LISP)

test:
	$(EMACS) -Q --batch -L . \
		-l test/prosody-test.el \
		-f ert-run-tests-batch-and-exit
