#!/bin/bash

#find $1 -iname *.py | xargs -n1 ~/.emacs.d/bin/lintrunner.sh
pylint "$1"
pycodestyle --ignore=E221,E701,E202 --repeat "$1"
true
