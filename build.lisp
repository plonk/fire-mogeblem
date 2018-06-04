(ql:quickload '(cl-charms  alexandria defenum cffi))

(setf sb-ext:*invoke-debugger-hook*
      (lambda (condition hook)
        (declare (ignore condition hook))
        ;; デバッガが呼ばれたら、単にプログラムを終了する
        ;; recklessly-p に t を指定して、後始末(標準出力ストリームのフラッシュ等)が行われないようにする
        (sb-ext:quit :recklessly-p t)))
  ;;デバッガの文字ずれないようにする
       ;;(charms/ll:endwin)))

(load "test.lisp")

(sb-ext:save-lisp-and-die "fire-mogeblem"
			  :toplevel #'hello-world
			  :save-runtime-options t
			  :executable t)
