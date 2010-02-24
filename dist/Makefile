#  Cocoa Emacs (Emacs 23) を極力 Emacs 22 の時と同じ動作にさせるための Makefile です

## Comannds Variables
TAR = tar xvzf
CURL = curl -O

## Emacs Variables
EMACS_VERSION=23.1.92
EMACS_SRC = emacs-${EMACS_VERSION}
USER-EMACS-DIRECTORY=${HOME}/.emacs.d
LISPDIR=${USER-EMACS-DIRECTORY}/lisp
VERSION_SPECIFIC_LISPDIR=${USER-EMACS-DIRECTORY}/lisp
INFODIR=${USER-EMACS-DIRECTORY}/share/info
DATADIR=${USER-EMACS-DIRECTORY}/share/skk

##
APEL_SRC=apel-10.7
SKK_SRC=ddskk-14.0.91
SKK_CFG=SKK-CFG
HOWM_SRC=howm-1.3.8
AUCTeX_SRC=auctex-11.86


all:
	@echo "[ src ]"
	@echo "patch"
	@echo "compile"
	@echo "install"
	@echo "installlib"

# ソース類の取得
src:
	$(CURL) ftp://alpha.gnu.org/gnu/emacs/pretest/${EMACS_SRC}.tar.gz ; \
	${TAR} ${EMACS_SRC}.tar.gz

# パッチの取得
patch: src
	$(CURL) http://jaist.dl.sourceforge.jp/macemacsjp/45818/inline_patch-${EMACS_VERSION}-b1.tar.gz ;\
	${TAR} inline_patch-${EMACS_VERSION}-b1.tar.gz ; \
	$(CURL) http://sakito.jp/shared/emacspatches/${EMACS_SRC}-mac-1.97.tar.gz ; \
	${TAR} ${EMACS_SRC}-mac-1.97.tar.gz ; \
	cd ${EMACS_SRC} ; \
	patch -p 0 < ../${EMACS_SRC}-mac-1.97/patch-mac ; \
	cp -r ../${EMACS_SRC}-mac-1.97/mac mac ; \
	cp ../${EMACS_SRC}-mac-1.97/src/* src ; \
	cp ../${EMACS_SRC}-mac-1.97/lisp/term/mac-win.el lisp/term ; \
	cp nextstep/Cocoa/Emacs.base/Contents/Resources/Emacs.icns mac/Emacs.app/Contents/Resources/Emacs.icns ; \
	patch -p 0 < ../inline_patch-${EMACS_VERSION}-b1/emacs-inline.patch ; \
	patch -p 1 < ../disable-fink.patch ; \
	patch -p 1 < ../disable-ns-self-contained.patch

#  コンパイル
compile: patch
	cd ${EMACS_SRC} ; \
	./configure --with-mac --without-x ; \
	make bootstrap

# インストール
install: compile
	cd ${EMACS_SRC} ; \
	sudo make install ; \
	@echo "sudo cp -R mac/Emacs.app /Application"

# lisp
installlib: apelinstall skkinstall auctexinstall howminstall w3minstall

# apel
apelinstall:
	${CURL} http://kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/${APEL_SRC}.tar.gz ; \
	${TAR} ${APEL_SRC}.tar.gz; \
	cd ${APEL_SRC}; \
	make LISPDIR=${LISPDIR} VERSION_SPECIFIC_LISPDIR=${VERSION_SPECIFIC_LISPDIR} INFODIR=${INFODIR} ; \
	make install ${LISPDIR} VERSION_SPECIFIC_LISPDIR=${VERSION_SPECIFIC_LISPDIR} INFODIR=${INFODIR}

# skk
skkinstall:
	${CURL} http://openlab.ring.gr.jp/skk/maintrunk/${SKK_SRC}.tar.gz ; \
	${TAR} ${SKK_SRC}.tar.gz ; \
	${CURL} http://openlab.jp/skk/dic/SKK-JISYO.L.gz ; \
	gunzip SKK-JISYO.L.gz ; \
	cd ${SKK_SRC} ; \
	mv ../SKK-JISYO.L dict ; \
	echo "(add-to-list 'load-path \"$(LISPDIR)/apel\")" >> $(SKK_CFG); \
	echo "(add-to-list 'load-path \"$(LISPDIR)/emu\")" >> $(SKK_CFG); \
	echo '(setq APEL_DIR "$(LISPDIR)/apel")' >> $(SKK_CFG); \
	echo '(setq EMU_DIR "$(LISPDIR)/emu")' >> $(SKK_CFG); \
	echo '(setq SKK_DATADIR "$(DATADIR)")' >> $(SKK_CFG); \
	echo '(setq SKK_INFODIR "$(INFODIR)")' >> $(SKK_CFG); \
	echo '(setq SKK_LISPDIR "$(LISPDIR)")' >> $(SKK_CFG); \
	echo '(setq SKK_SET_JISYO t)' >> $(SKK_CFG); \
	make LISPDIR=${LISPDIR} VERSION_SPECIFIC_LISPDIR=${VERSION_SPECIFIC_LISPDIR} INFODIR=${INFODIR} ; \
	make install ${LISPDIR} VERSION_SPECIFIC_LISPDIR=${VERSION_SPECIFIC_LISPDIR} INFODIR=${INFODIR}

# howm
howminstall:
	${CURL} http://howm.sourceforge.jp/a/${HOWM_SRC}.tar.gz; \
	${TAR} ${HOWM_SRC}.tar.gz; \
	cd ${HOWM_SRC}; \
	./configure --with-lispdir=${LISPDIR} --with-docdir=$HOME/.emacs.d/share/howm --with-extdir=$HOME/.emacs.d/bin; \
	make; \
	make install

# AUCTeX
auctexinstall:
	${CURL} http://ftp.gnu.org/pub/gnu/auctex/${AUCTeX_SRC}.tar.gz ; \
	${TAR} ${AUCTeX_SRC}.tar.gz ; \
	cd ${AUCTeX_SRC} ; \
	./configure --prefix=${USER-EMACS-DIRECTORY} --with-lispdir=${LISPDIR} ; \
	make ; \
	make install

# emacs-w3m
w3minstall:
	cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m ; \
	cd emacs-w3m ; \
	autoconf ; \
	./configure --with-lispdir=~/.emacs.d/lisp/w3m --datarootdir=~/.emacs.d/share --with-icondir=~/.emacs.d/share/icon ; \
	make ; \
	make install ; \
	make install-icons

