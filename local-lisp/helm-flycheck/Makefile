.PHONY : test

EMACS ?= emacs
CASK ?= cask
SRC ?= helm-flycheck.el

LOADPATH = -L .

ELPA_DIR = $(shell EMACS=$(EMACS) $(CASK) package-directory)

test: test-compile

clean-elpa:
	rm -rf .cask

clean-elc:
	cask clean-elc

clean: clean-elpa clean-elc

print-deps:
	${EMACS} --version
	@echo CASK=${CASK}

test-compile: elpa
	$(CASK) exec $(EMACS) -batch -Q $(LOADPATH) -eval "(progn (setq byte-compile-error-on-warn t) (batch-byte-compile))" $(SRC)

travis-ci: print-deps test

elpa: $(ELPA_DIR)

$(ELPA_DIR): Cask
	$(CASK) install
	touch $@

