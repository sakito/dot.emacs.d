.PHONY: all
all:
	#emacs --batch -f batch-byte-compile lisp/*.el
	#emacs --batch -f batch-byte-compile lisp/theme/*.el
	emacs --batch -f batch-byte-compile init.el



.PHONY: dry_sync
dry_sync:
	rsync -n -av --delete --exclude-from=./ignore_list.txt ./ ${HOME}/src/github.com/sakito/dot.emacs.d

.PHONY: sync
sync:
	rsync -av --delete --exclude-from=./ignore_list.txt ./ ${HOME}/src/github.com/sakito/dot.emacs.d
