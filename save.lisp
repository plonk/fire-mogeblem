;;ロードしたらファイル消す
;;謎の文字列
(defun rand-string ()
  (let ((str))
    (dotimes (i 16)
      (case (random 2)
	(0 (push (code-char (+ 97 (random 26))) str))
	(1 (push (code-char (+ 48 (random 10))) str))))
    (coerce str 'string)))



;;(name job hp maxhp str skill w_lv agi luck def give-exp move weapon rank lv)
(defun make-units-data-list (units)
  (loop for u across units
     collect (list (unit-name u) (unit-job u) (unit-hp u) (unit-maxhp u) (unit-str u)
		   (unit-skill u) (unit-w_lv u) (unit-agi u) (unit-luck u) (unit-def u)
		   (unit-give_exp u) (unit-move u) (unit-team u) (unit-weapon u) (unit-rank u) (unit-lv u))))

;;セーブする
(defun save-suru (game)
  (let* ((stage (game-stage game))
	 (str (rand-string))
	 (units-data (make-units-data-list (game-player_units game)))
	 (lst units-data)
	 (window (charms:make-window 30 6 5 5)))
    (with-open-file (out str :direction :output
			 :if-exists :supersede)
      (format out "(setf *load-units-data* '~s)~%" lst)
      (format out "(setf *load-stage* ~d)" stage))
    (charms:write-string-at-point
     window
     "セーブしました" 1 1)
    (charms:write-string-at-point
     window
     "復活の呪文は" 1 2)
    (charms:write-string-at-point
     window
     (format nil "~a" str) 1 3)
    (refresh-windows window)
    (charms:get-char window)
    (charms:destroy-window window)))

;;謎のコード
(defun my-getstr (window)
  (cffi:with-foreign-string
      (str "")
    (charms/ll:wgetstr (charms::window-pointer window) str)
    (cffi:foreign-string-to-lisp str)))



(defun load-file (str)
  (handler-case
      (load str)
    (file-error ()
      nil)))


;;ロードする
(defun load-suru (game)
  (let ((len (length *load-units-data*)))
    (setf (game-player_units game)
	  (make-array len :initial-contents
		      (loop for u in *load-units-data*
			 collect (apply #'make-unit
					(mapcan #'list
						'(:name :job :hp :maxhp :str :skill
						  :w_lv :agi :luck :def :give_exp :move :team :weapon :rank :lv)
						u))))
	  (game-stage game) *load-stage*)))

;;復活の呪文入力
(defun get-loadstr (game)
  (charms:enable-echoing)
  (charms/ll:curs-set 1)
  (let ((window (charms:make-window 40 5 0 0)))
    (charms:write-string-at-point
     window
     "復活の呪文を入力してください" 1 1)
    (charms:move-cursor window 1 2)
    (let ((loadstr (my-getstr window)))
      (if (load-file loadstr)
	  (progn (charms:write-string-at-point
		  window
		  "ロードしました" 1 3)
		 (load-suru game)
		 (delete-file loadstr)
		 (setf *game-opening* nil
		       *set-init-pos* t))
	  (charms:write-string-at-point
	   window
	   "復活の呪文が間違ってます！！" 1 3))
      (refresh-windows window)
      (charms:disable-echoing)
      (charms/ll:curs-set 0)
      (charms:get-char window)
      (charms:destroy-window window))))

