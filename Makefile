all:
	emacs --batch -f batch-byte-compile lisp/init_preface.el
	emacs --batch -f batch-byte-compile theme/nightsblue-theme.el
	emacs --batch -l lisp/init_preface.el -f batch-byte-compile init.el
