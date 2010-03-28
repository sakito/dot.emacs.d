# etags の タグファイルを生成するためのいいかげんなMakefile

all:
	@echo "etags"

pyetags:
	#find /Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/ -name "*.py" -not -name '_test.py' | xargs etags -f python.TAGS --append -l python
	find /Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/ -name "*.py" -not -name '_test.py' | xargs etags -f python.TAGS --append --regex='/^[ \t]+def[ \t]+\([a-zA-Z_0-9]+\)/\1/'

objcetags:
	s="\t "; \
	S="[$$s]*"; \
	w="_a-zA-Z0-9"; \
	CN="[A-Z][$$w]*"; \
	NM="[$$w][$$w]*"; \
	SDK="/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS3.1.2.sdk/System/Library/Frameworks"; \
	find $$SDK -name "*.h" | xargs etags -a --declarations -r "/$$S[-+]$$S(\($$S$$NM\)\{1,3\}$$S\**$$S)?$$S\($$NM\)$$S[:;]/\2/" -f frm.tags;\
	sed "/^@class/d" frm.tags > objc.TAGS

haskelletags:
	VER=6.12.1; \
	SRC=ghc-$${VER}-src.tar.bz2; \
	if [ ! -f $${SRC} ]; \
	then curl -O http://www.haskell.org/ghc/dist/$${VER}/$${SRC}; \
	fi; \
	tar xvfj $${SRC}; \
	find . -name \*.*hs | xargs hasktags -e; \
	mv TAGS haskell.TAGS

# otags を利用した方が良いかもしれない
ocamletags:
	find "/usr/local/lib/ocaml" -name "*.ml*" | sort -r | xargs \
	etags "--regex=/let[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/let[ \t]+rec[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/and[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/type[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/exception[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/val[ \t]+\([^ \t]+\)/\1/" \
	"--regex=/module[ \t]+\([^ \t]+\)/\1/" -f ocaml.TAGS