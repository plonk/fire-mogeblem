# 端末でファイアーエムブレムっぽいゲーム

- 動作環境  
ubuntu17.10  

- 必要なもの  
 - libncurses5-dev  
(ubuntuなら sudo apt install libncurses5-dev  )  
 - lisp環境  
 roswellかsbcl+quicklisp  

  
 必要なものを用意して  
 ros -l load.lisp  
 か  
 sbcl --load load.lisp  
 すれば動くはず

- キー入力
z:決定
x:キャンセル
↑→↓←(カーソルキー):カーソル移動
