(ql:quickload '(cl-charms bordeaux-threads alexandria defenum))

(setf sb-ext:*invoke-debugger-hook*  
      (lambda (condition hook) 
        (declare (ignore condition hook))
        ;; デバッガが呼ばれたら、単にプログラムを終了する
        ;; recklessly-p に t を指定して、後始末(標準出力ストリームのフラッシュ等)が行われないようにする
        ;;(sb-ext:quit :recklessly-p t)))
	;;デバッガの文字ずれないようにする
(charms/ll:endwin)))

(load "define.lisp")



;;ユニットのデータ作成
(defun make-unit-data (unit-num x y)
  (let* ((data (aref *units-data* unit-num))
	 (name (aref data 0)) (job (aref data 1)) (hp (aref data 2))
	 (str (aref data 3)) (skill (aref data 4))
	 (w_lv (aref data 5)) (agi (aref data 6)) (luck (aref data 7))
	 (def (aref data 8)) (move (aref data 9))
	 (type (aref data 10)) (weapon (aref data 11)))
    (make-unit :name name :job job :hp hp :maxhp hp :str str :skill skill
	       :w_lv w_lv :agi agi :luck luck :def def :move move :weapon weapon
	       :x x :y y :unit-num unit-num :type type)))

;;地形データと全ユニットデータ作成
(defun make-cells-and-units (map map-no-chara)
  (let ((units-list nil) (cells (make-array (list *map-h* *map-w*)))
	(units nil))
    (loop for y from 0 below *map-h* do
      (loop for x from 0 below *map-w* do
	(let ((c (aref map (+ (* *map-w* y) x))))
	  (if (numberp c)
	      (setf (aref cells y x) c)
	      (let ((unit-num (- (char-code (aref (symbol-name c) 0)) (char-code #\A))))
		(setf (aref cells y x) (aref map-no-chara (+ (* *map-w* y) x)))
		(push (make-unit-data unit-num x y) units-list))))))
    (setf units-list (sort units-list #'< :key #'(lambda (x) (unit-unit-num x))))
    (setf units (make-array (length units-list)
			    :initial-contents units-list))
    (values cells units)))


;;windowの枠
(defun draw-window-border (window
                           &optional
                             (ls #\|) (rs #\|) (ts #\-) (bs #\-)
                             (tl #\+) (tr #\+) (bl #\+) (br #\+))
  (apply #'charms/ll:wborder (charms::window-pointer window)
         (mapcar #'char-code (list ls rs ts bs tl tr bl br))))

(defun draw-window-box (window &optional (verch #\|) (horch #\-))
  (charms/ll:box (charms::window-pointer window) (char-code verch) (char-code horch)))

(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:false)
    (error "Your terminal does not support color."))
  (when (eql (charms/ll:can-change-color) charms/ll:false)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
	t
	(error "start-color error ~s." ret-code))))


(defmacro define-color-pair ((name pair) foreground background)
  `(progn
     (start-color)
     (setf ,name (progn (charms/ll:init-pair ,pair ,foreground ,background)
			(charms/ll:color-pair ,pair)))))



(defun draw-window-background (window color-pair)
  (charms/ll:wbkgd (charms::window-pointer window) color-pair))

(defmacro with-colors ((window color-pair) &body body)
  (let ((winptr (gensym)))
    (alexandria:once-only (color-pair)
      `(let ((,winptr (charms::window-pointer ,window)))
	 (charms/ll:wattron ,winptr ,color-pair)
	 ,@body
	 (charms/ll:wattroff ,winptr ,color-pair)))))

(defun init-color ()
  (define-color-pair (+white/blue+ 1) +white+ +blue+)
  (define-color-pair (+black/red+ 2) +black+ +red+)
  (define-color-pair (+black/white+ 3) +black+ +white+)
  (define-color-pair (+green/black+ 4) +green+ +black+)
  (define-color-pair (+dark_green/green+ 5) +dark_green+ +green+)
  (define-color-pair (+low-yama-f/low-yama-b+ 6) +low-yama-f+ +low-yama-b+)
  (define-color-pair (+high-yama-f/high-yama-b+ 7) +high-yama-f+ +high-yama-b+)
  (define-color-pair (+black/town-b+ 8) +black+ +town-b+)
  (define-color-pair (+black/fort-b+ 9) +black+ +fort-b+)
  (define-color-pair (+black/castle-b+ 10) +black+ +castle-b+)
  (define-color-pair (+black/green+ 11) +black+ +green+)
  (define-color-pair (+black/player-b+ 12) +black+ +player-b+))

;;疑似カーソル移動
(defun cursor-move (cursor x y window)
  (multiple-value-bind (width height)
      (charms:window-dimensions window)
    (if (/= x 0)
	(let* ((x1 (+ (cursor-x cursor) x))
	       (x2 (+ (* x1 2) 1)))
	  (cond
	    ((>= x2 (1- width))
	     (setf (cursor-x cursor) 0))
	    ((<= x2 0)
	     (setf (cursor-x cursor) (- *map-w* 1)))
	    (t (setf (cursor-x cursor) x1))))
	(let ((y1 (+ (cursor-y cursor) y)))
	  (cond
	    ((>= y1 (- height 2))
	     (setf (cursor-y cursor) 0))
	    ((< y1 0)
	     (setf (cursor-y cursor) (- *map-h* 1)))
	    (t (setf (cursor-y cursor) y1)))))))

;;地形データ表示
(defun show-cell-data (cell window)
  (charms:write-string-at-point 
   window
   (format nil "  地形  : ~a" (celldesc-name (aref *celldescs* cell)))
   1 1)
  (charms:write-string-at-point 
   window
   (format nil "防御効果: ~2d%" (celldesc-defence (aref *celldescs* cell)))
   1 2)
  (charms:write-string-at-point 
   window
   (format nil "回復効果: ~a" (if (celldesc-heal (aref *celldescs* cell))
				 "あり" "なし"))
   1 3))

;; x y位置にユニットいたらユニットデータ返す
(defun get-unit (x y units)
  (loop for unit across units do
       (if (and (= (unit-x unit) x) (= (unit-y unit) y))
	   (return-from get-unit unit))))

(defun show-unit-data (unit window)
  (let* ((weapon (aref *weapondescs* (unit-weapon unit)))
	 (w-name (weapondesc-name weapon))
	 (w-dmg (weapondesc-damage weapon))
	 (w-hit (weapondesc-hit weapon))
	 (w-wei (weapondesc-weight weapon))
	 (w-cri (weapondesc-critical weapon))
	 (w-ranmin (weapondesc-rangemin weapon))
	 (w-ranmax (weapondesc-rangemax weapon)))
  (charms:write-string-at-point
   window
   (format nil " 名前 : ~a" (unit-name unit))
   1 1)
  (charms:write-string-at-point
   window
   (format nil "  HP  : ~2d/~2d" (unit-hp unit) (unit-maxhp unit))
   1 2)
  (charms:write-string-at-point
   window
   (format nil "  力  : ~2d" (unit-str unit))
   1 3)
  (charms:write-string-at-point
   window
   (format nil "  技  : ~2d" (unit-skill unit))
   1 4)
  (charms:write-string-at-point
   window
   (format nil "武器Lv: ~2d" (unit-w_lv unit))
   1 5)
  (charms:write-string-at-point
   window
   (format nil "素早さ: ~2d" (unit-agi unit))
   1 6)
  (charms:write-string-at-point
   window
   (format nil " 幸運 : ~2d" (unit-luck unit))
   1 7)
  (charms:write-string-at-point
   window
   (format nil "守備力: ~2d" (unit-def unit))
   1 8)
  (charms:write-string-at-point
   window
   (format nil "移動力: ~2d" (unit-move unit))
   1 9)
  (charms:write-string-at-point
   window
   (format nil " 武器 : ~a" w-name)
   1 10)
  (charms:write-string-at-point
   window
   (format nil "        威力:~2d 重量:~2d 命中:~2d~%         必殺:~2d レンジ:~d〜~d"
	   w-dmg w-wei w-hit w-cri w-ranmin w-ranmax)
   1 11)))

;;地形とユニット描画
(defun show-cell-unit (cells units window window2 unit-win cursor)
  (loop for y from 0 below *map-h* do
       (loop for x from 0 below *map-w* do
	    (let ((cell (aref cells y x)) (unit (get-unit x y units))
		  (color +dark_green/green+) (aa nil))
	      (if unit
		  (let* ((job-num (unit-job unit))
			 (job (aref *jobdescs* job-num))
			 (job-aa (jobdesc-aa job)))
		    (if (= (unit-type unit) +ally+)
			(setf color +black/player-b+)
			(setf color +black/red+))
		    (setf aa job-aa))
		  (progn
		    (case cell
		      (0 (setf color +white/blue+))
		      (1 (setf color +black/green+))
		      (2 (setf color +dark_green/green+))
		      (3 (setf color +low-yama-f/low-yama-b+))
		      (4 (setf color +high-yama-f/high-yama-b+))
		      (5 (setf color +black/town-b+))
		      (6 (setf color +black/fort-b+))
		      (7 (setf color +black/castle-b+))
		      (otherwise (setf color +black/red+)))
		    (setf aa (celldesc-aa (aref *celldescs* cell)))))
	      (if (and (= (cursor-x cursor) x)
		       (= (cursor-y cursor) y))
		  (progn (setf color +black/white+)
			 (show-cell-data cell window2)
			 (if unit
			     (show-unit-data unit unit-win))))
	      (with-colors (window color)
		(charms:write-string-at-point 
		 window
		 aa
		 (+ (* x 2) 1) (1+ y)))))))

  
(defun hello-world ()
  (setf *random-state* (make-random-state t))
  (charms:with-curses ()
    (charms/ll:initscr)
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (init-color)
    (let ((cells nil) (units nil)
	  (cursor (make-cursor :x 0 :y 0))
	  (window (charms:make-window (+ 2 (* 2 *map-w*))
				      (+ 2 *map-h*) 0 0)))
      ;;特殊キーを使う
      (charms/ll:keypad (charms::window-pointer window) 1)
    (loop named hello-world
	  with unit-win = (charms:make-window 34 14
					      0 (+ 2 *map-h*))
	  with window2 = (charms:make-window 18 5 35 (+ 2 *map-h*))
	  do (progn
	       
	       (charms:clear-window window)
	       (charms:clear-window window2)
	       (charms:clear-window unit-win)
	       
	       ;;地形とユニットセット
	       (if (or (null cells) (null units))
		   (setf (values cells units)
			 (make-cells-and-units *map1-chara* *map1-no-chara*)))

	       ;;描画
	       (show-cell-unit cells units window window2 unit-win cursor)
	       
	       (draw-window-box window)
	       (draw-window-box window2)
	       (draw-window-box unit-win)
	       (charms:write-string-at-point
		window
		(format nil "x:~d y:~D" (cursor-x cursor) (cursor-y cursor)) 28 0)
	       ;;(charms:write-string-at-point window "マップ" 28 0)
	       (charms:write-string-at-point window2 "地形データ" 3 0)
	       (charms:write-string-at-point unit-win "ユニットデータ" 10 0)
	       (charms:refresh-window window)
	       (charms:refresh-window window2)
	       (charms:refresh-window unit-win)
	       (let ((c (charms:get-char window)))
		 (cond
		   ((eql c #\q)
		    (return-from hello-world))
		   ((eql c (code-char charms/ll:key_up))
		    (cursor-move cursor 0 -1 window))
		   ((eql c (code-char charms/ll:key_down))
		    (cursor-move cursor 0 1 window))
		   ((eql c (code-char charms/ll:key_right))
		    (cursor-move cursor 1 0 window))
		   ((eql c (code-char charms/ll:key_left))
		    (cursor-move cursor -1 0 window))))
	       (sleep 0.01))))))

(hello-world)
			
