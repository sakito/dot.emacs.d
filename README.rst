.. -*- restructuredtext -*-

sakito の .emacs.d 以下のファイル を一部公開しています。

`ソース <http://bitbucket.org/sakito/dot.emacs.d/src>`_ を参照してください。

かなりグダグダなのであまり参考にならない気がしますが、徐々に整理していきます。

`Emacs をビルドするための個人的なパッチ等 <http://bitbucket.org/sakito/macemacspatch>`_  を当てたり、private 以下に大量のコードがあったりして、そのままだと動作しないかもしれません。

実作業を優先して、運用てカバーする傾向があるので、たまに検証不十分で動作してないコードがまざることがありますが、ご了承ください。

リポジトリの構成は以下のようになっています。このようにした経緯に関しては `.emacs.d の利用 <http://www.sakito.com/2009/12/emacsd.html>`_ を参照してください。

.. sourcecode:: text

 ~/
  .emacs.d/
    init.el
    site-start.d
      init-*.el
      … init.el から読みこむ起動設定ファイル
    lisp
     … 他者が配布している Emacs Lisp
    local-lisp
     … 自分で作成した Emacs Lisp
     … なんらかの理由で公式配布のバージョンから修正してある Emacs Lisp
     … 頻繁に更新する Emacs Lisp
    private
     … 公開しないファイル類など
    rc.d
     … zshrc 等の rc ファイルがはいっていて ln -s している。結局 Emacs との連携を考えるのでまとめた
    bin
     … Emacs Lisp のみから呼ぶことを想定して作成してあるスクリプト等
    share
      dtd
      rnc
      icons
      skk
      man
      info
      … Emacs Lisp が利用する固定(基本いじくらない)リソース
    etc
     snippets
     rst
     … Emacs Lisp が利用する可変(基本いじる)リソース
    var
     … キャシュファイルやバックアップファイル用
