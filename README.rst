.. -*- restructuredtext -*-

sakito の .emacs.d 以下のファイル を一部公開しています。

検証不十分で動作してないコードがまざることがありますが、ご了承ください。

leaf.el( https://github.com/conao3/leaf.el )を利用しています

リポジトリの構成は以下のようになっています。

dotfile類は https://bitbucket.org/sakito/dot.zsh.d/ にて管理

.. sourcecode:: text

 ~/
  .emacs.d/
    init.el
    lisp
     … パッケージとして取得不可能になっているlisp
    bin
     … Emacs Lisp のみから呼ぶことを想定して作成してあるスクリプト等
    share
     … Emacs Lisp が利用する固定(基本いじくらない)リソース
    etc
     … Emacs Lisp が利用する可変(基本いじる)リソース
    var
     … キャシュファイルやバックアップファイル用
    private
     … 公開しないファイル類など
