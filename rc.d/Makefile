# 初回セットアップ用スクリプト

CWD=$(shell pwd)

all:
	@echo HOME = ${HOME}
	@echo CWD = ${CWD}

dotfiles:
	ln -s ${CWD}/hgignore ${HOME}/.hgignore
	ln -s ${CWD}/pip ${HOME}/.pip
	ln -s ${CWD}/screenrc ${HOME}/.screenrc
	ln -s ${CWD}/zlogin ${HOME}/.zlogin
	ln -s ${CWD}/hgrc ${HOME}/.hgrc
	ln -s ${CWD}/pythonrc.py ${HOME}/.pythonrc.py
	ln -s ${CWD}/tmux.conf ${HOME}/.tmux.conf
	ln -s ${CWD}/zshrc ${HOME}/.zshrc
	ln -s ${CWD}/ccl-init.lisp ${HOME}/.ccl-init.lisp
	[ -d ${HOME}/.buildout ] || mkdir ${HOME}/.buildout
	cp ${CWD}/buildout/default.cfg ${HOME}/.buildout
	echo "eggs-directory = $HOME/var/eggs" >> ${HOME}/.buildout/default.cfg
	echo "download-cache = $HOME/var/cache" >> ${HOME}/.buildout/default.cfg

portinstall:
	sudo port install nkf wget lftp libxml2 libxslt w3m apg
	sudo port install coreuitls findutils
	sudo port install zsh-devel +mp_completion +pcre +examples +doc
	sudo port install screen
	sudo port install tmux +screen
	sudo port install gawk gsed
	sudo port install yasm
	sudo port install scala29 ocaml smlnj
	sudo port install gauche slib
	sudo port install ccl lisp-hyperspec
	sudo port install subversion +bash_completion +mod_dav_svn +tools +unicode_path
	sudo port install subversion-python27bindings
	sudo port install readline
	sudo port install jpeg
	sudo port install lcms
	sudo port install rlwrap

# no test sorry
pipinstall:
	curl -O https://raw.github.com/pypa/virtualenv/develop/virtualenv.py
	python virtualenv.py --distribute -p /usr/local/bin/python ${HOME}/local
	source ${HOME}/local/bin/activate
	easy_install readline
	echo "/opt/local/lib/svn-python2.7" >  ${HOME}/local/lib/python2.7/site-packages/svn-python.pth
	pip -r requirements.txt

