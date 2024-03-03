all:
	#emacs --batch -f batch-byte-compile lisp/*.el
	emacs --batch -f batch-byte-compile lisp/theme/*.el
	emacs --batch -f batch-byte-compile init.el
