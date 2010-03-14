# etags の タグファイルを生成するためのいいかげんなMakefile

all:
	@echo "etags"

pyetags:
	find /Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/ -name "*.py" | xargs etags -f python.TAGS -l python

objcetags:
	s="\t ";\
	S="[$$s]*";\
	w="_a-zA-Z0-9";\
	CN="[A-Z][$$w]*";\
	NM="[$$w][$$w]*";\
	SDK="/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS3.1.2.sdk/System/Library/Frameworks";\
	find $$SDK -name "*.h" | xargs etags -a --declarations -r "/$$S[-+]$$S(\($$S$$NM\)\{1,3\}$$S\**$$S)?$$S\($$NM\)$$S[:;]/\2/" -f frm.tags;\
	sed "/^@class/d" frm.tags > objc.TAGS

#haskelletags:
#	find "/Library/Frameworks/GHC.framework/Versions/Current/usr/lib/" -name \*.hi | xargs hasktags -e

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