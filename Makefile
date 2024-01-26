all:
	emacs --batch -l init.d/init_preface.el -f batch-byte-compile init.el
