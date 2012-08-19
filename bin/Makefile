.PHONY: all
all:
	@echo "make lint"

.PHONY: lint
lint:
	cython --embed lintrunner.py
	gcc -O3 -c lintrunner.c -I${HOME}/local/include/python2.7
	gcc -o lintrunner.exe lintrunner.o  -L${HOME}/local/lib -framework Python

