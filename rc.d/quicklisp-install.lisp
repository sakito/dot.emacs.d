(load "http://beta.quicklisp.org/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload '(
                :quicklisp-slime-helper
                :linedit
                :clsql
                :cl-fad
                :series
                ))

(ql:add-to-init-file)