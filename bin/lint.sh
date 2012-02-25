#!/bin/bash

find $1 -iname *.py | xargs -n1 ~/.emacs.d/bin/lintrunner.sh
