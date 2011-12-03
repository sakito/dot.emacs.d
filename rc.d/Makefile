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

portinstall:
	sudo port install nkf wget lftp findutils libxml2 libxslt w3m apg
	sudo port install zsh-devel +mp_completion +pcre +examples +doc
	sudo port install screen
	sudo port install tmux +screen
	sudo port install gawk gsed
	sudo port install yasm
	sudo port install scala29 ocaml smlnj
	sudo port install gauche ccl lisp-hyperspec
	sudo port install subversion +bash_completion +mod_dav_svn +tools +unicode_path
	sudo port install readline
	sudo port install jpeg
	sudo port install lcms

# no test sorry
pipinstall:
	sudo easy_install -UZ pip
	hash -r
	sudo pip install virtualenv
	hash -r
	virtualenv --no-site-packages -p /usr/local/bin/python ${HOME}/local
	hash -r
	pip install readline
	pip install mercurial
	echo "/opt/subversion/lib/svn-python" >  ~/local/lib/python2.7/site-packages/svn-python.pth
	pip install hgsubversion
	pip install hg-git
	pip install docutils
	pip install Pygments
	pip install nose
	pip install Sphinx
	pip install ipython
	pip install Cython
	pip install lxml
	pip install chardet
	pip install web.py
	pip install SQLAlchemy
	pip install numpy
	pip install blockdiag


