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

(defun save-suru (game)
  (let* ((stage (game-stage game))
	 (str (rand-string))
	 (units-data (make-units-data-list (game-player_units game)))
	(lst units-data))
    (with-open-file (out str :direction :output
			 :if-exists :supersede)
      (format out "(defparameter *load-units-data* '~s)~%" lst)
      (format out "(defparameter *load-stage* ~d)" stage))))


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
