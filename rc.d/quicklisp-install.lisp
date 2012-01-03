(load "http://beta.quicklisp.org/quicklisp.lisp")
(quicklisp-quickstart:install)

(ql:quickload '(
                :quicklisp-slime-helper
                :linedit
                :clsql
                :cl-fad
                :series
                :cl-csv
                ;; ln -s /usr/local/mysql/lib/libmysqlclient.dylib ~/.ccl/quicklisp/dists/quicklisp/software/clsql-xxxx-git/db-mysql
                :clsql-mysql
                :hunchentoot
                :hunchentoot-test
                ))

(ql:add-to-init-file)