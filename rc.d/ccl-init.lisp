#|
Clozure CL 用の設定ファイル
この設定ファイルは以下の作業を実施した後に設定しています

sudo port install ccl
curl -O http://beta.quicklisp.org/quicklisp.lisp
ccl64 --load quicklisp.lisp
(quicklisp-quickstart:install)
;; 動作確認
(ql:system-apropos "term")
;; 設定ファイルに設定を追加
(ql:add-to-init-file)
(quit)

;; 何かインストールする場合は (ql:quickload :cl-ppcre) とかでインストールする

;; 定期的に (ql:update-all-dists) すること

ln -s ~/.emacs.d/rc.d/ccl-init.lips ~/.ccl-init.lips

;; quicklispを利用してライブライリをインストールしている
ccl load ~/.emacs.d/rc.d/quicklisp-install.lisp --eval '(ccl:quit)'

|#

;;; 文字コードの設定
(setf ccl:*default-external-format*
      (ccl:make-external-format :character-encoding :utf-8
                                :line-termination :unix)
      ccl:*default-file-character-encoding* :utf-8
      ccl:*default-socket-character-encoding* :utf-8)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".ccl/quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;;; ccl-init.lisp ends here
