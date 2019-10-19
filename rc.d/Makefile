# 初回セットアップ用スクリプト

CWD=$(shell pwd)

all:
	@echo HOME = ${HOME}
	@echo CWD = ${CWD}

dotfiles:
	ln -sf ${CWD}/hgignore ${HOME}/.hgignore
	ln -sf ${CWD}/hgrc ${HOME}/.hgrc
	ln -sf ${CWD}/gitconfig ${HOME}/.gitconfig
	ln -sf ${CWD}/gitignore ${HOME}/.config/git/ignore
	ln -sf ${CWD}/pip ${HOME}/.pip
	ln -sf ${CWD}/zlogin ${HOME}/.zlogin
	ln -sf ${CWD}/pythonrc.py ${HOME}/.pythonrc.py
	ln -sf ${CWD}/tmux.conf ${HOME}/.tmux.conf
	ln -sf ${CWD}/zshrc ${HOME}/.zshrc

brewinstall:
	brew install coreutils findutils
	brew install readline
	brew install tmux reattach-to-user-namespace
	brew install zsh
	brew install wget
	brew install autoconf automake
	brew install texinfo
	brew install git
	brew install gauche go

#portinstall:
#	sudo port install nkf wget lftp libxml2 libxslt w3m apg
#	sudo port install coreutils findutils
#	sudo port install zsh-devel +mp_completion +pcre +examples +doc
#	sudo port install screen
#	sudo port install tmux +screen
#	sudo port install gawk gsed
#	sudo port install yasm
#	sudo port install scala29 ocaml smlnj
#	sudo port install gauche slib
#	sudo port install ccl lisp-hyperspec
#	sudo port install subversion +bash_completion +mod_dav_svn +tools +unicode_path
#	sudo port install subversion-python27bindings
#	sudo port install readline
#	sudo port install jpeg
#	sudo port install lcms
#	sudo port install rlwrap
#	sudo port install webp

# no test sorry
pipinstall:
	curl -O https://raw.github.com/pypa/virtualenv/develop/virtualenv.py
	python virtualenv.py --distribute -p /usr/local/bin/python ${HOME}/local
	source ${HOME}/local/bin/activate
	pip install readline
	pip -r requirements.txt

# end
