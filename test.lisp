;;TODO ステージ終了ごとにセーブしたい ロードするときの文字列入力を作る
;;bordeaux-threads


(load "define.lisp")
(load "map-data.lisp")
(load "save.lisp")

;;-------------A-star--------------------------------------------------------------------
(defstruct node
  (pos nil)
  (stable 0)
  (id 0)
  (g 0)
  (h 0)
  (f 0)
  (parent nil))

#|
CL-USER 10 > (minimum '((a 1) (b -1) (c -2)) #'< #'second)
(C -2)
|#
(defun minimum (list predicate key)
  (when list
    (let* ((m0 (first list))
           (m1 (funcall key m0)))
      (mapc (lambda (e0 &aux (e1 (funcall key e0)))
              (when (funcall predicate e1 m1)
                (psetf m0 e0 m1 e1)))
            list)
      m0)))

;;マンハッタン距離
(defun manhatan (pos goal)
  (+ (abs (- (car pos) (car goal))) (abs (- (cadr pos) (cadr goal)))))
;;ノードから道順をとり出す
(defun node-pick-pos (end pos-l)
  (if (null (node-parent end))
      pos-l
      (node-pick-pos (node-parent end) (cons (node-pos end) pos-l))))

(defun astar (start goal cells movecost block-cell)
  (let ((open (list (make-node :pos start :g 0 :h (manhatan start goal)
			                         :f (manhatan start goal))))
        (close nil))
    (loop for i from 0 do
      (if (null open)
	        (return (print "hoge")))
      (let ((n (minimum open #'< #'node-f)))
	      (setf open (remove n open :test #'equalp))
	      (push n close)
	      (if (equal (node-pos n) goal)
	          (return (values (node-f n) (node-pick-pos n '()))))
	      (setf (node-g n) (- (node-f n) (node-h n)))
	      (loop for v in '((1 0) (0 1) (-1 0) (0 -1)) do
	        (let ((next (mapcar #'+ (node-pos n) v)))
	          (if (and (>= (1- *map-w*) (car next) 0)
			               (>= (1- *map-h*) (cadr next) 0)
                     (not (find next block-cell :test #'equal)))
		            (let ((m (find next open :test #'equal :key #'node-pos))
                      (dist (aref movecost (aref cells (cadr next) (car next)))))
                  (when (>= dist 0)
                    (if m
              		      (if (> (node-f m) (+ (node-g n) (node-h m) dist))
              			        (setf (node-f m) (+ (node-g n) (node-h m) dist)
              				            (node-parent m) n))
              		      (progn
              			      (setf m (find next close :test #'equal :key #'node-pos))
                    			(if m
                    			    (if (> (node-f m) (+ (node-g n) (node-h m) dist))
                          				(progn
                          				  (setf (node-f m) (+ (node-g n) (node-h m) dist)
                          					(node-parent m) n)
                          				  (setf close (remove m close :test #'equalp))
                          				  (push m open)))
                    			    (progn
                    			      (setf m (make-node))
                    			      (setf (node-pos m) next)
                    			      (setf (node-g m) dist)
                    			      (setf (node-h m) (manhatan next goal))
                    			      (setf (node-f m) (+ (node-g n) (node-h m) (node-g m)))
                    			      (setf (node-parent m) n)
                    			      (push m open))))))))))))))
;;--------------------------------------------------------------------------------

(defmethod unit-jobdesc ((u unit))
  (aref *jobdescs* (unit-job u)))

(defmethod unit-weapondesc ((u unit))
  (aref *weapondescs* (unit-weapon u)))



;;初期位置選択
;;プレイヤーのマップ初期位置設定
(defun set-start-pos-player (c x y game)
  (let* ((num (- (char-code c) 97))
	 (unit (aref (game-player_units game) num)))
    (setf (unit-x unit) x
	  (unit-y unit) y)))




;;レベルによるステータス補正
(defun lv-status (status-l hp+ st+)
  (loop for i from 2 to 10
     do (case i
	  ((2 3 10) ;;hp maxhp give-exp
 	   (incf (nth i status-l) hp+))
	  ((4 5 6 7 9) ;; str skill w_lv agi def
	   (incf (nth i status-l) st+))))
  status-l)

;;ユニットのデータ作成
;;ステージ数=敵のレベル
(defun make-unit-data (unit-c x y lv)
  (let* ((status-l (copy-seq (cdr (assoc unit-c *units-data*))))
	 (hp+ (1- lv)) (st+ (floor lv 2))
	 (data (lv-status status-l hp+ st+)))
    (apply #'make-unit
	   (append (mapcan #'list
			   '(:name :job :hp :maxhp :str :skill
			     :w_lv :agi :luck :def :give_exp :move :team :weapon :rank)
			   data)
		   (list :unit-num unit-c :x x :y y)))))

;;地形データと全ユニットデータ作成
(defun make-cells-and-units (game map map-no-chara)
  (let ((cells (make-array (list *map-h* *map-w*)))
        (units nil))
    (loop for y from 0 below *map-h* do
      (loop for x from 0 below *map-w* do
       (let ((c (aref map (+ (* *map-w* y) x))))
          (cond 
	    ((numberp c) ;;ユニットいない
	     (setf (aref cells y x) c))
	    ;;((>= 103 (char-code c) 97) ;;プレイヤー側ユニット
	    ;; (setf (aref cells y x) (aref map-no-chara (+ (* *map-w* y) x)))
	    ;; (set-start-pos-player c x y game))
	    (t ;;敵ユニット
	     (setf (aref cells y x) (aref map-no-chara (+ (* *map-w* y) x)))
	     (push (make-unit-data c x y (game-stage game)) (game-units_l game)))))))
    ;;敵ユニットと味方ユニット合体
    ;;(setf units-list (append units-list (coerce (player-units game) 'list)))
    (setf units (make-array (length (game-units_l game))
			    :initial-contents (game-units_l game)))
    (setf (game-cells game) cells
          (game-units game) units)))

;;ステージ終了時に生き残ってる味方ユニット
(defun get-alive-ally-units (game)
  (sort (remove-if #'(lambda (u)
	         (or (null (unit-alive? u))
		     (= +enemy+ (unit-team u))))
		   (game-units game))
	#'< :key #'(lambda (u) (unit-job u))))
  

;;色変更できるかチェック
(defun start-color ()
  (when (eql (charms/ll:has-colors) charms/ll:false)
    (error "Your terminal does not support color."))
  (let ((ret-code (charms/ll:start-color)))
    (if (= ret-code 0)
	t
	(error "start-color error ~s." ret-code)))
  (unless (= 256 charms/ll:*COLORS*)
    (when (eql (charms/ll:can-change-color) charms/ll:false)
      (error "Your terminal cannot change colors.")))
  (charms/ll:use-default-colors)
  (charms/ll:curs-set 0))

;;カラーペアを作る
(defmacro define-color-pair ((name pair) foreground background)
  `(progn
     (start-color)
     (setf ,name (progn (charms/ll:init-pair ,pair ,foreground ,background)
                  (charms/ll:color-pair ,pair)))))


;;windowの背景色設定
(defun draw-window-background (window color-pair)
  (charms/ll:wbkgd (charms::window-pointer window) color-pair))

(defmacro with-colors ((window color-pair) &body body)
  (let ((winptr (gensym)))
    (alexandria:once-only (color-pair)
      `(let ((,winptr (charms::window-pointer ,window)))
        (charms/ll:wattron ,winptr ,color-pair)
        ,@body
        (charms/ll:wattroff ,winptr ,color-pair)))))


(defun lookup-color-value (index)
  (cond
   ((<= 16 index 231)
    (let ((offset (- index 16)))
      (multiple-value-bind (red remainder) (truncate offset 36)
          (multiple-value-bind (green blue) (truncate remainder 6)
              (mapcar (lambda (v) (* 200 v)) (list red green blue))))))
   ((<= 232 index 255)
    (let* ((offset (- index 232))
           (v (truncate (* offset 1000) 23)))
      (list v v v)))))

;; 
(defparameter *next-color* 16)

(defun allocate-color (index)
  (if (= 256 charms/ll:*COLORS*)
      index
    (let ((this-color *next-color*))
      (incf *next-color*)
      (destructuring-bind (r g b) (lookup-color-value index)
        (charms/ll:init-color this-color r g b))
      this-color)))

(defun init-colors ()
  (setf +dark_green+  (allocate-color  22))
  (setf +low-yama-f+  (allocate-color 214))
  (setf +low-yama-b+  (allocate-color 166))
  (setf +high-yama-f+ (allocate-color 248))
  (setf +high-yama-b+ (allocate-color 244))
  (setf +town-b+      (allocate-color  51))
  (setf +fort-b+      (allocate-color 201))
  (setf +castle-b+    (allocate-color 226))
  (setf +player-b+    (allocate-color  39))
  (setf +p-move-b+    (allocate-color 141))
  (setf +e-move-b+    (allocate-color 160))
  (setf +atk-b+       (allocate-color 202)))

(defun init-color-pairs ()
  (define-color-pair (+white/blue+ 1) +white+ +blue+)
  (define-color-pair (+black/red+ 2) +black+ +red+)
  (define-color-pair (+black/white+ 3) +black+ +white+)
  (define-color-pair (+green/black+ 4) +green+ +black+)
  (define-color-pair (+dark_green/green+ 5) +dark_green+ +green+)
  (define-color-pair (+low-yama-f/low-yama-b+ 6) +low-yama-f+ +green+)
  (define-color-pair (+high-yama-f/high-yama-b+ 7) +high-yama-f+ +high-yama-b+)
  (define-color-pair (+black/town-b+ 8) +black+ +green+)
  (define-color-pair (+black/fort-b+ 9) +black+ +green+)
  (define-color-pair (+black/castle-b+ 10) +black+ +castle-b+)
  (define-color-pair (+black/green+ 11) +black+ +green+)
  (define-color-pair (+black/player-b+ 12) +black+ +player-b+)
  (define-color-pair (+black/p-move-b+ 13) +black+ +p-move-b+)
  (define-color-pair (+black/e-move-b+ 14) +black+ +e-move-b+)
  (define-color-pair (+black/atk-b+    15) +black+ +atk-b+)
  (define-color-pair (+black/yellow+   16) +black+ +yellow+)
  (define-color-pair (+white/black+    17) +white+ +black+))

;;色作成
(defun init-color ()
  (init-colors)
  (init-color-pairs))

;;疑似カーソル移動
(defun cursor-move (game x y map-win)
  (multiple-value-bind (width height)
      (charms:window-dimensions map-win)
    (if (/= x 0)
       (let* ((x1 (+ (game-cursor_x game) x))
              (x2 (+ (* x1 2) 1)))
          (cond
             ((>= x2 (1- width))
              (setf (game-cursor_x game) 0))
             ((<= x2 0)
              (setf (game-cursor_x game) (- *map-w* 1)))
             (t (setf (game-cursor_x game) x1))))
       (let ((y1 (+ (game-cursor_y game) y)))
          (cond
             ((>= y1 (- height 2))
              (setf (game-cursor_y game) 0))
             ((< y1 0)
              (setf (game-cursor_y game) (- *map-h* 1)))
             (t (setf (game-cursor_y game) y1)))))))

;;地形データ表示
(defun show-cell-data (cell map-win)
  (charms:write-string-at-point
   map-win
   (format nil "  地形  : ~a" (celldesc-name (aref *celldescs* cell)))
   1 1)
  (charms:write-string-at-point
   map-win
   (format nil "防御効果: ~2d%" (celldesc-def (aref *celldescs* cell)))
   1 2)
  (charms:write-string-at-point
   map-win
   (format nil "回復効果: ~a" (if (celldesc-heal (aref *celldescs* cell))
                           "あり" "なし"))
   1 3))

;; x y位置にユニットいたらユニットデータ返す
(defun get-unit (x y units)
  (loop for unit across units do
       (if (and (= (unit-x unit) x) (= (unit-y unit) y)
                (unit-alive? unit))
           (return-from get-unit unit))))

;;うまいことまとめたい
;;lvup = ステータス上昇率 (HP 力 技 武器 速さ 運 守備 魔防)
;;レベルアップしたときの上昇したステータス表示
(defun show-lv-up-status (unit unit-win) 
  (let* ((weapon (unit-weapondesc unit))
         (w-name (weapondesc-name weapon))
         (w-dmg (weapondesc-damage weapon))
         (w-hit (weapondesc-hit weapon))
         (w-wei (weapondesc-weight weapon))
         (w-cri (weapondesc-critical weapon))
         (w-ranmin (weapondesc-rangemin weapon))
         (w-ranmax (weapondesc-rangemax weapon))
	 (lvup (mapcar #'(lambda (x) (>= x (random 100))) (unit-lvup unit))))
    (clear-windows unit-win)
    (charms:write-string-at-point
    unit-win
    (format nil " 名前 : ~a ~a" (unit-name unit) (if (unit-act? unit) "(行動済み)" ""))
    1 1)
   (charms:write-string-at-point
    unit-win
    (format nil "  Lv  : ~2d ↑ +1" (incf (unit-lv unit)))
    1 2)
   (charms:write-string-at-point
    unit-win
    (if (nth 0 lvup)
	(format nil "  HP  : ~2d ↑ +1" (incf (unit-maxhp unit)))
	(format nil "  HP  : ~2d" (unit-maxhp unit)))
    1 3)
   (charms:write-string-at-point
    unit-win
    (if (nth 1 lvup)
	(format nil "  力  : ~2d ↑ +1" (incf (unit-str unit)))
	(format nil "  力  : ~2d" (unit-str unit)))
    1 4)
   (charms:write-string-at-point
    unit-win
    (if (nth 2 lvup)
	(format nil "  技  : ~2d ↑ +1" (incf (unit-skill unit)))
	(format nil "  技  : ~2d" (unit-skill unit)))
    1 5)
   (charms:write-string-at-point
    unit-win
    (if (nth 3 lvup)
	(format nil "武器Lv: ~2d ↑ +1" (incf (unit-w_lv unit)))
	(format nil "武器Lv: ~2d" (unit-w_lv unit)))
    1 6)
   (charms:write-string-at-point
    unit-win
    (if (nth 4 lvup)
	(format nil "素早さ: ~2d ↑ +1" (incf (unit-agi unit)))
	(format nil "素早さ: ~2d" (unit-agi unit)))
    1 7)
   (charms:write-string-at-point
    unit-win
    (if (nth 5 lvup)
	(format nil " 幸運 : ~2d ↑ +1" (incf (unit-luck unit)))
	(format nil " 幸運 : ~2d" (unit-luck unit)))
    1 8)
   (charms:write-string-at-point
    unit-win
    (if (nth 6 lvup)
	(format nil "守備力: ~2d ↑ +1" (incf (unit-def unit)))
	(format nil "守備力: ~2d" (unit-def unit)))
    1 9)
   (charms:write-string-at-point
    unit-win
    (format nil "移動力: ~2d" (unit-move unit))
    1 10)
   (charms:write-string-at-point
    unit-win
    (format nil " 武器 : ~a" w-name)
    1 11)
   (charms:write-string-at-point
    unit-win
    (format nil "        威力:~2d 重量:~2d 命中:~2d~%         必殺:~2d レンジ:~d〜~d"
     w-dmg w-wei w-hit w-cri w-ranmin w-ranmax)
    1 12)
   (draw-window-box unit-win) ;;枠
   (charms:write-string-at-point
    unit-win
    "レベルアップ"
    1 0)
   (refresh-windows unit-win)))

;;ユニットデータを表示
(defun show-unit-data (unit unit-win)
  (let* ((weapon (unit-weapondesc unit))
         (w-name (weapondesc-name weapon))
         (w-dmg (weapondesc-damage weapon))
         (w-hit (weapondesc-hit weapon))
         (w-wei (weapondesc-weight weapon))
         (w-cri (weapondesc-critical weapon))
         (w-ranmin (weapondesc-rangemin weapon))
         (w-ranmax (weapondesc-rangemax weapon)))
   (charms:write-string-at-point
    unit-win
    (format nil " 名前 : ~a ~a" (unit-name unit) (if (unit-act? unit) "(行動済み)" ""))
    1 1)
   (charms:write-string-at-point
    unit-win
    (format nil "  Lv  : ~2d" (unit-lv unit))
    1 2)
   (charms:write-string-at-point
    unit-win
    (format nil "  HP  : ~2d/~2d" (unit-hp unit) (unit-maxhp unit))
    1 3)
   (charms:write-string-at-point
    unit-win
    (format nil "  力  : ~2d" (unit-str unit))
    1 4)
   (charms:write-string-at-point
    unit-win
    (format nil "  技  : ~2d" (unit-skill unit))
    1 5)
   (charms:write-string-at-point
    unit-win
    (format nil "武器Lv: ~2d" (unit-w_lv unit))
    1 6)
   (charms:write-string-at-point
    unit-win
    (format nil "素早さ: ~2d" (unit-agi unit))
    1 7)
   (charms:write-string-at-point
    unit-win
    (format nil " 幸運 : ~2d" (unit-luck unit))
    1 8)
   (charms:write-string-at-point
    unit-win
    (format nil "守備力: ~2d" (unit-def unit))
    1 9)
   (charms:write-string-at-point
    unit-win
    (format nil "移動力: ~2d" (unit-move unit))
    1 10)
   (charms:write-string-at-point
    unit-win
    (format nil " 武器 : ~a" w-name)
    1 11)
   (charms:write-string-at-point
    unit-win
    (format nil "        威力:~2d 重量:~2d 命中:~2d~%         必殺:~2d レンジ:~d〜~d"
     w-dmg w-wei w-hit w-cri w-ranmin w-ranmax)
    1 12)))

;;地形の色取得
(defun get-cell-color (cell can-move)
  (cond
    ((and can-move (= can-move +ally+))
     +black/p-move-b+)
    ((and can-move (= can-move +enemy+))
     +black/e-move-b+)
    (t
     (cond
       ((= cell +cell_sea+)     +white/blue+)
       ((= cell +cell_plane+)   +black/green+)
       ((= cell +cell_forest+)  +dark_green/green+)
       ((= cell +cell_mt+)      +low-yama-f/low-yama-b+)
       ((= cell +cell_high_mt+) +high-yama-f/high-yama-b+)
       ((= cell +cell_town+)    +black/town-b+)
       ((= cell +cell_fort+)    +black/fort-b+)
       ((= cell +cell_castle+)  +black/castle-b+)))))

;;初期位置きめるときの地形描画
(defun show-set-pos (game map-win cell-win unit-win mes-win)
  (clear-windows map-win cell-win unit-win mes-win)
  (loop for y from 0 below *map-h*
     do (loop for x from 0 below *map-w*
	   do (let* ((cell (aref (game-cells game) y x)) (unit (get-unit x y (game-units game)))
		     (color (get-cell-color cell nil))
		     (aa (celldesc-aa (aref *celldescs* cell))))
		(when (and unit (unit-alive? unit))
		  (if (= (unit-team unit) +ally+)
		      (setf color +black/player-b+)
		      (setf color +black/red+))
		  (setf aa (jobdesc-aa (unit-jobdesc unit))))
		(when (aref (game-init_pos_area game) y x)
		  (setf color +black/yellow+))
		(when (and (= (game-cursor_x game) x)
			   (= (game-cursor_y game) y))
		  (setf color +black/white+)
		  (show-cell-data cell cell-win)
		  (when (and unit (unit-alive? unit))
		    (show-unit-data unit unit-win)))
		(with-colors (map-win color)
		  (charms:write-string-at-point
		   map-win aa (+ (* x 2) 1) (1+ y))))))
  (charms:write-string-at-point
    mes-win
    (format nil "配置する場所を選んでく~% ださい~% (黄色ぽいとこ)")
    1 1)
  (draw-windows-box map-win cell-win unit-win mes-win)
  (charms:write-string-at-point
   map-win
   (format nil "ステージ~d" (game-stage game))
   12 0)
  (refresh-windows map-win cell-win unit-win mes-win))
  
;;地形とユニット描画
(defun show-cell-unit (game map-win cell-win unit-win)
  (loop for y from 0 below *map-h*
     do (loop for x from 0 below *map-w*
	   do (let ((cell (aref (game-cells game) y x)) (unit (get-unit x y (game-units game)))
		    (color +dark_green/green+) (aa nil))
		(if (and unit (unit-alive? unit)) ;;生きてるユニットがいる
		    (progn
		      (if (= (unit-team unit) +ally+)
			  (setf color +black/player-b+) ;;プレイヤー側
			  (setf color +black/red+))     ;;敵側
		      (setf aa (jobdesc-aa (unit-jobdesc unit))))
		    (let ((can-move (aref (game-move_area game) y x))) ;;移動可能範囲
		      (setf color (get-cell-color cell can-move))
		      (setf aa (celldesc-aa (aref *celldescs* cell)))))
		(when (aref (game-atk_area game) y x) ;;攻撃可能範囲
		  (setf color +black/atk-b+))
		(when (and (= (game-cursor_x game) x) ;;カーソル位置
			   (= (game-cursor_y game) y))
		  (setf color +black/white+)
		  (show-cell-data cell cell-win)
		  ;;カーソル位置にうにっといたらデータ表示
		  (when (and unit (unit-alive? unit))
		    (show-unit-data unit unit-win)))

		(with-colors (map-win color)
		  (charms:write-string-at-point
		   map-win aa
		   (1+ (* x 2)) (1+ y) ;;枠とか全角文字とか考慮したあれ
		   ))))))

;;敵移動中描画
;;地形とユニット描画
(defun show-map (cells units map-win)
  (loop for y from 0 below *map-h* do
    (loop for x from 0 below *map-w* do
       (let ((cell (aref cells y x)) (unit (get-unit x y units))
             (color +dark_green/green+) (aa nil))
         (if (and unit (unit-alive? unit))
             (let ((job-aa (jobdesc-aa (unit-jobdesc unit))))
                 (if (= (unit-team unit) +ally+)
                     (setf color +black/player-b+)
                     (setf color +black/red+))
                 (setf aa job-aa))
             (setf color (get-cell-color cell nil)
                   aa (celldesc-aa (aref *celldescs* cell))))
         (with-colors (map-win color)
             (charms:write-string-at-point
	      map-win aa (+ (* x 2) 1) (1+ y)))))))

;;ユニットを置ける初期位置エリアを求める
(defun get-init-pos-area (game)
  (let* ((pos (nth (game-stage game) *stage-init-pos*))
	 (d-y (car pos)) (d-x (cadr pos))
	 (can-pos (mapcar #'(lambda (yx) (list (+ d-y (car yx)) (+ d-x (cadr yx)))) *ar-cell*)))
    (setf (game-cursor_x game) d-x
	  (game-cursor_y game) d-y)
    (loop for y from 0 below *map-h*
       do (loop for x from 0 below *map-w*
	     do (when (find (list y x) can-pos :test #'equal)
		  (setf (aref (game-init_pos_area game) y x) t))))))
	      

;;ユニットの移動可能範囲取得
(defun can-move-area (x y move movecost team game)
  (when (and (>= (1- *map-w*) x 0) (>= (1- *map-h*) y 0))
    (let* ((cell (aref (game-cells game) y x))
           (cost (aref movecost cell))
           (unit? (get-unit x y (game-units game))))
      (when (and (>= move cost) (>= cost 0)
                 (or (null unit?)
                     (= team (unit-team unit?))))
        (setf (aref (game-move_area game) y x) team)
        (loop for v in '((0 1) (0 -1) (1 0) (-1 0))
              do
              (can-move-area (+ x (car v)) (+ y (cadr v))
                             (- move cost) movecost team game))))))

;;ユニットの移動可能範囲取得
(defun get-move-area (unit game)
  (let ((move (unit-move unit)) (x (game-cursor_x game))
        (team (unit-team unit)) (y (game-cursor_y game))
        (movecost (jobdesc-movecost (unit-jobdesc unit))))
    (can-move-area x y move movecost team game)))

;;移動可能範囲の初期化
(defun init-move-area (move-area)
  (loop for y from 0 below *map-h* do
       (loop for x from 0 below *map-w* do
        (setf (aref move-area y x) nil))))

;;距離
(defun m-dist (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;;ユニットとユニットの距離
(defun unit-dist (unit1 unit2)
  (+ (abs (- (unit-x unit1) (unit-x unit2)))
     (abs (- (unit-y unit1) (unit-y unit2)))))

;;攻撃可能範囲取得
(defun can-atk-area? (x1 y1 x2 y2 r-min r-max atk-area i)
  (if (or (> 0 x2) (> 0 y2) (>= x2 *map-w*) (>= y2 *map-h*) (> 0 i))
      nil
      (let ((dist (m-dist x1 y1 x2 y2)))
        (if (>= r-max dist r-min)
            (setf (aref atk-area y2 x2) t))
        (can-atk-area? x1 y1 (1+ x2) y2 r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 (1- x2) y2 r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 x2 (1+ y2) r-min r-max atk-area (1- i))
        (can-atk-area? x1 y1 x2 (1- y2) r-min r-max atk-area (1- i)))))

;;攻撃可能範囲取得
(defun get-atk-area (unit atk-area)
  (let* ((weapon (unit-weapondesc unit))
         (r-min (weapondesc-rangemin weapon))
         (r-max (weapondesc-rangemax weapon))
         (x1 (unit-x unit))
         (y1 (unit-y unit)))
    (can-atk-area? x1 y1 x1 y1 r-min r-max atk-area r-max)))

;;攻撃可能範囲に敵がいるか判定
(defun can-atk? (game)
  (loop for u across (game-units game)
        do (if (and (aref (game-atk_area game) (unit-y u) (unit-x u))
                    (unit-alive? u) (/= (unit-team (game-select_unit game)) (unit-team u)))
               (return-from can-atk? t))))

;;ユニット選択フェイズでzキー押したとき
(defun select-unit-p (game)
  (init-move-area (game-move_area game))
  (let ((unit (get-unit (game-cursor_x game) (game-cursor_y game) (game-units game))))
    (if (and unit (unit-alive? unit) (not (unit-act? unit)))
        (let ((team (unit-team unit)))
          (setf (game-select_unit game) unit)
          (if (= team +ally+)
              (setf (game-s_phase game) +select_move+)
              (setf (game-s_phase game) +select_unit+))
          (get-move-area unit game))
        (setf (game-s_phase game) +select_unit+))))

;;移動先選択フェイズでzキー押したとき
(defun select-move-p (game)
  (let* ((x (game-cursor_x game)) (y (game-cursor_y game))
         (unit (get-unit x y (game-units game))))
    (if (and (or (null unit) ;;ユニットがいない場所
                 (equal (game-select_unit game) unit) ;;その場でとどまる
                 (not (unit-alive? unit))) ;;死亡しているユニットの場所
             (aref (game-move_area game) y x))
        (progn
          (setf (unit-x (game-select_unit game)) x
                (unit-y (game-select_unit game)) y)
          (init-move-area (game-move_area game))
          (get-atk-area (game-select_unit game) (game-atk_area game))
          (let ((atk? (can-atk? game)))
            (if atk?
                (setf (game-s_phase game) +select_attack+)
                ;;攻撃できる敵がいないとき
                (progn (init-move-area (game-atk_area game))
                       (setf (unit-act? (game-select_unit game)) t ;;行動済み
                             (game-select_unit game) nil
                             (game-s_phase game) +select_unit+))))))))

;;攻撃メッセージ
(defun get-atk-msg (atk-type)
  (cond
    ((= atk-type +atk_normal+)
     "攻撃")
    ((= atk-type +atk_counter+)
     "反撃")
    ((= atk-type +atk_re+)
     "再攻撃")))

;;攻撃が命中してダメージを与える
(defun give-damage (atk-unit def-unit a-w-dmg a-w-cri atk-win)
  (let* ((a-str (unit-str atk-unit)) (a-luck (unit-luck atk-unit))
         (d-def (unit-def def-unit)) (a-skill (unit-skill atk-unit))
         (a-atk (+ a-str a-w-dmg))
         (hissatsu? (+ a-w-cri (floor (+ a-skill a-luck) 2)))
         (dmg (max 0 (- a-atk d-def))) (msg-y 2))
    (when (> hissatsu? (random 100))
      (setf dmg (* dmg 3))
      (charms:write-string-at-point
        atk-win "必殺の一撃！" 1 (prog1 msg-y (incf msg-y))))
    (charms:write-string-at-point
      atk-win (format nil "~aに~dのダメージを与えた" (unit-name def-unit) dmg) 1 msg-y)
    (charms:get-char atk-win)
    (decf (unit-hp def-unit) dmg)
    (min 20 dmg))) ;;経験値用

;;命中しなかったメッセージ
(defun miss-msg (atk-win)
  (charms:write-string-at-point
    atk-win "ミス！！！！" 1 2)
  (charms:get-char atk-win))

;;HPが０になったときのメッセージ
(defun dead-msg (dead-unit alive-unit atk-win)
  (charms:write-string-at-point
    atk-win
    (if (= (unit-team dead-unit) +ally+)
        (format nil "~a は ~a に倒された。" (unit-name dead-unit) (unit-name alive-unit))
        (format nil "~a は ~a を倒した。" (unit-name alive-unit) (unit-name dead-unit)))
    1 4)
  (charms:get-char atk-win))

(defun celldesc-yx (cells y x)
  (aref *celldescs* (aref cells y x)))

;;攻撃処理
(defun attack! (atk-unit def-unit cells atk-type atk-win)
  (let* ((a-skill (unit-skill atk-unit)) (kari-exp 0)
         (a-agi (unit-agi atk-unit)) (d-agi (unit-agi def-unit))
         (a-weapon (unit-weapondesc atk-unit))
         (a-w-dmg (weapondesc-damage a-weapon)) (a-w-hit (weapondesc-hit a-weapon))
         (a-w-wei (weapondesc-weight a-weapon)) (a-w-cri (weapondesc-critical a-weapon))
         (d-weapon (unit-weapondesc def-unit))
         (d-w-wei (weapondesc-weight d-weapon))
         (d-r-min (weapondesc-rangemin d-weapon))
         (d-r-max (weapondesc-rangemax d-weapon))
         (dist (unit-dist atk-unit def-unit))
         (a-speed (- a-agi a-w-wei)) (d-speed (- d-agi d-w-wei))
         (cell (celldesc-yx cells (unit-y def-unit) (unit-x def-unit)))
         (cell-def (celldesc-def cell))
         (atk-hit (+ a-w-hit a-skill)) (def-dodge (+ cell-def (- d-agi d-w-wei)))
         (hit? (- atk-hit def-dodge)) (atk-msg (get-atk-msg atk-type)))
    (charms:clear-window atk-win)
    (charms:write-string-at-point
      atk-win (format nil "~aの~a" (unit-name atk-unit) atk-msg) 1 1)
    (draw-window-box atk-win)
    (charms:get-char atk-win)
    ;;命中したらダメージ
   (if (> hit? (random 100))
       (setf kari-exp (give-damage atk-unit def-unit a-w-dmg a-w-cri atk-win))
       (miss-msg atk-win))
   ;;反撃
   (when (and (> (unit-hp def-unit) 0) (= atk-type +atk_normal+)
              (>= d-r-max dist d-r-min))
     (attack! def-unit atk-unit cells +atk_counter+ atk-win))
   ;;再攻撃
   (when (and (> a-speed d-speed) (= atk-type +atk_normal+)
              (> (unit-hp def-unit) 0) (> (unit-hp atk-unit) 0))
     (attack! atk-unit def-unit cells +atk_re+ atk-win))
   ;;死亡判定&経験値取得
   (when (= atk-type +atk_normal+)
     (cond
       ((>= 0 (unit-hp def-unit)) ;;def側を倒した
        (when (= (unit-rank def-unit) +leader+)
          (setf *game-over?* t))
	(if (= (unit-rank def-unit) +boss+) ;;bossだったら+10経験値
	    (incf (unit-exp atk-unit) (+ 10 (unit-give_exp def-unit)))
	    (incf (unit-exp atk-unit) (unit-give_exp def-unit)))
        (setf (unit-alive? def-unit) nil) ;;死亡
        (dead-msg def-unit atk-unit atk-win))
       ((>= 0 (unit-hp atk-unit)) ;;atk側が倒された
        (when (= (unit-rank atk-unit) +leader+)
          (setf *game-over?* t))
        (setf (unit-alive? atk-unit) nil)
        (dead-msg atk-unit def-unit atk-win))
       (t ;;敵を倒せなかった(ダメージ与えただけ)
	(incf (unit-exp atk-unit) kari-exp))))

   (charms:refresh-window atk-win)))

;;レベルアップ処理
(defun lv-up (unit unit-win)
  (when (>= (unit-exp unit) 100)
    (show-lv-up-status unit unit-win) ;;上昇したステータス表示
    (charms:get-char unit-win)
    (setf (unit-exp unit) (- (unit-exp unit) 100))))

;;攻撃先選択フェイズでzキー押したとき
(defun select-atk-p (game unit-win atk-win)
  (let* ((x (game-cursor_x game)) (y (game-cursor_y game))
         (atk-unit (game-select_unit game))
         (def-unit (get-unit x y (game-units game))))
    (when (and def-unit (aref (game-atk_area game) y x)
               (/= (unit-team atk-unit) (unit-team def-unit)))
      (attack! atk-unit def-unit (game-cells game) +atk_normal+ atk-win)
      (lv-up atk-unit unit-win) ;;レベルアップ処理
      (setf (game-s_phase game) +select_unit+
            (unit-act? atk-unit) t
            (game-select_unit game) nil)
      (init-move-area (game-atk_area game)))))

;;全員行動済みかチェック
(defun turn-end? (units team)
  (every #'unit-act?
        (remove-if (lambda (u) (or (/= team (unit-team u))
                                   (null (unit-alive? u)))) units)))

;;行動済み初期化
(defun init-act (units team)
  (loop for u across units
        when (= team (unit-team u))
        do
        (setf (unit-act? u) nil)))

;;unitに一番近いキャラ
(defun near-chara (unit units r-min)
  (first
    (sort (remove-if (lambda (u) (or (not (unit-alive? u))
                                     (= (unit-team u) (unit-team unit))
				     (> r-min (unit-dist unit u)))) ;;最小射程以下の敵消す
                     (coerce units 'list))
     #'<
     :key (lambda (u)
            (m-dist (unit-x unit) (unit-y unit) (unit-x u) (unit-y u))))))

;;敵が動いたら画面描画
(defun show-enemy-move (units cells map-win sleep-time)
  (charms:clear-window map-win)
  (show-map cells units map-win)
  (draw-window-box map-win)
  (charms:write-string-at-point
    map-win "敵のターン" 28 0)
  (charms:refresh-window map-win)
  (sleep sleep-time))


;;指定した地形の(x y)座標を返す
(defun get-cell-pos (cell cells)
  (loop for y from 0 below *map-h* do
    (loop for x from 0 below *map-w* do
      (when (= (aref cells y x) cell)
        (return-from get-cell-pos (list x y))))))


;;ユニットの移動可能範囲をリストで返す
;;ユニットの移動可能範囲取得
(defun can-move-list (xy units move movecost cells team m-list)
  (if (or (> 0 (car xy)) (> 0 (cadr xy)) (>= (car xy) *map-w*) (>= (cadr xy) *map-h*))
      m-list
      (let* ((cell (aref cells (cadr xy) (car xy)))
             (cost (aref movecost cell))
             (unit? (get-unit (car xy) (cadr xy) units)))
        (if (or (> cost move) (= cost -1)
                (and unit? (/= (unit-team unit?) team)))
            m-list
            (progn
              (when (and (null unit?)
                         (not (find xy m-list :test #'equal)))
                (push xy m-list))
              (loop for v in '((0 1) (0 -1) (1 0) (-1 0)) do
                (let ((xy1 (mapcar #'+ xy v)))
                  (setf m-list (can-move-list xy1 units (- move cost) movecost cells team m-list))))
              m-list)))))

;;ユニットの移動可能範囲取得
(defun get-move-list (unit units cells)
  (let* ((move (unit-move unit))
         (x (unit-x unit)) (y (unit-y unit))
         (team (unit-team unit))
         (movecost (jobdesc-movecost (unit-jobdesc unit))))
    (can-move-list (list x y) units move movecost cells team '())))

;;相手ユニットがいて移動できないcellリストを返す
(defun get-block-cell (unit units)
  (mapcar #'(lambda (x) (list (unit-x x) (unit-y x)))
              (remove-if #'(lambda (x) (or (null (unit-alive? x))
                                           (= (unit-team x) (unit-team unit))))
                             (coerce units 'list))))


;;目標に一番近くなる移動可能な場所までの道を返す
(defun all-astar (start goal unit units cells movecost block-cell)
  (let* ((sin-goal (first (sort (get-move-list unit units cells)
                                #'<
                                :key #'(lambda (x) (astar goal x cells movecost block-cell)))))
        (cost 0) (paths nil))
    (setf (values cost paths)
          (astar start sin-goal cells movecost block-cell))
    paths))

;;画面クリア
(defun gamen-clear (&rest window)
 (dolist (win window)
   (charms:clear-window win)))

;;画面リフレッシュ
(defun gamen-refresh (&rest window)
 (dolist (win window)
   (charms:refresh-window win)))

;;a-starで移動ルート探索(城行き)
(defun astar-to-castle (unit cells units movecost map-win)
  (let* ((start (list (unit-x unit) (unit-y unit)))
         (castle (get-cell-pos +cell_castle+ cells))
         (block-cell (get-block-cell unit units))
         (paths (all-astar start castle unit units cells movecost block-cell)))
    (when (not (equal start castle))
      (loop for path in paths do
        (setf (unit-x unit) (car path)
              (unit-y unit) (cadr path))
        ;;敵が一歩動いたら画面描画
        (show-enemy-move units cells map-win 0.15)))))

;;a-starで移動ルート探索
(defun astar-move (unit target cells units r-min r-max movecost map-win)
  (let* ((start (list (unit-x unit) (unit-y unit)))
         (goal  (list (unit-x target) (unit-y target)))
         (block-cell (get-block-cell unit units)) ;;敵ユニットの場所
         (paths (all-astar start goal unit units cells movecost block-cell)))
    (loop for path in paths
       do
	 (let ((unit-oru? (get-unit (car path) (cadr path) units)))
	   (setf (unit-x unit) (car path)
		 (unit-y unit) (cadr path))
	   (setf target (near-chara unit units r-min)) ;;一歩動いたらターゲット再設定
	   ;;敵が一歩動いたら画面描画
	   (show-enemy-move units cells map-win 0.15)
	   ;;次の移動先のマスにユニットがいないand攻撃範囲に敵がいたら移動やめて攻撃にうつる
	   (when (and (>= r-max (unit-dist unit target) r-min)
		      (null unit-oru?))
	     (return))
	   ))))

;;敵の移動
(defun enemy-move (unit target r-min r-max units cells map-win)
  ;;(init-move-area move-area)
  ;;(get-move-area (unit-x unit) (unit-y unit) unit units cells move-area)
  (let ((movecost (jobdesc-movecost (unit-jobdesc unit))))
    (if (= (unit-rank unit) +boss+) ;;ボスは城に向かう
        (astar-to-castle unit cells units movecost map-win)
        (astar-move unit target cells units r-min r-max movecost map-win))))

;;敵の攻撃
(defun enemy-attack (atk-unit def-unit cells atk-win)
  (attack! atk-unit def-unit cells +atk_normal+ atk-win)
  (erase-window atk-win))

;;敵の行動 攻撃範囲に相手ユニットがいたら攻撃する
(defun enemy-act (units cells atk-win map-win)
  (let ((wait-time 0.05)) 
    (loop while (null *game-over?*)
       for u across units
       when (and (>= (unit-team u) +enemy+)
		 (unit-alive? u))
       do
	 (gamen-clear map-win)
	 (show-enemy-move units cells map-win wait-time)
	 (let* ((weapon (unit-weapondesc u))
		(r-min (weapondesc-rangemin weapon))
		(r-max (weapondesc-rangemax weapon))
		(target (near-chara u units r-min))
		(dist (unit-dist u target)))
	   (if (>= r-max dist r-min) ;;攻撃範囲に相手がいる
	       (enemy-attack u target cells atk-win)
	       (progn ;;移動後に攻撃範囲に相手がいたら攻撃
		 (enemy-move u target r-min r-max units cells map-win)
		 (setf target (near-chara u units r-min))
		 (when (>= r-max (unit-dist u target) r-min)
		   (enemy-attack u target cells atk-win)))))
	 (gamen-refresh map-win))))

;;回復地形効果
(defun cell-heal (units cells)
  (loop for u across units
    do
    (when (and (celldesc-heal (celldesc-yx cells (unit-y u) (unit-x u)))
               (> (unit-maxhp u) (unit-hp u)))
      (setf (unit-hp u)
            (min (+ (unit-hp u) (floor (unit-maxhp u) 10))
                 (unit-maxhp u))))))

;;ゲーム状態初期化
(defun init-game (game)
  (setf (game-cursor_x game)              0
        (game-cursor_y game)              0
        (game-cells game)               nil
        (game-units game)               nil
        (game-select_unit game)         nil	
        (game-turn game)           +p_turn+
        (game-s_phase game)   +select_unit+
	*game-clear*                    nil
	*game-over?*                    nil
	*game-opening*                  nil)
  (init-move-area (game-move_area game))
  (init-move-area (game-atk_area game)))

;;game opening
(defun game-opening-message (game map-win)
  (clear-windows map-win)
  (charms:write-string-at-point map-win "ファイアーモゲブレム" 15 2)
  (charms:write-string-at-point map-win "s:スタート" 15 4)
  (charms:write-string-at-point map-win "q:終わる" 15 5)
  ;; (charms:write-string-at-point map-win "w:セーブ" 15 6)
  ;; (charms:write-string-at-point map-win "l:ロード" 15 7)
  (gamen-refresh map-win)
  (let ((c (charms:get-char map-win)))
    (cond
      ((eql c #\q)
       (setf *game-play* nil))
      ;; ((eql c #\w)
      ;;  (save-suru game))
      ;; ((eql c #\l)
      ;;  (erase-window map-win)
      ;;  (get-loadstr game))
      ((eql c #\s)
       (init-game game)))))

;;ゲームクリアメッセージ
;;game opening
(defun game-clear-message (game map-win)
  (charms:write-string-at-point map-win "クリアしたよ！" 15 2)
  (charms:write-string-at-point map-win "r:もう一度やる" 15 5)
  (charms:write-string-at-point map-win "q:終わる" 15 6)
  (gamen-refresh map-win)
  (let ((c (charms:get-char map-win)))
    (cond
      ((eql c #\q)
       (setf *game-play* nil))
      ((eql c #\r)
       (init-game game)))))

;; game-over
(defparameter *gameobera* '(
"                                                    __ __ "
" _____ _____ _____ _____    _____ _____ _____ _____|  |  |"
"|   __|  _  |     |   __|  |     |  |  |   __| __  |  |  |"
"|  |  |     | | | |   __|  |  |  |  |  |   __|    -|__|__|"
"|_____|__|__|_|_|_|_____|  |_____|\\___/|_____|__|__|__|__|"))

(defun game-over-message (game map-win)
  (let ((len (length *gameobera*)))
    (dotimes (i len)
      (charms:write-string-at-point map-win (nth i *gameobera*) 0 (1+ i)))
    (charms:write-string-at-point map-win "q:終わる" 15 (+ len 2))
    (charms:write-string-at-point map-win "r:再挑戦" 15 (+ len 3))
    (gamen-refresh map-win)
    (let ((c (charms:get-char map-win)))
      (cond
        ((eql c #\q)
         (setf *game-play* nil))
        ((eql c #\r)
         (init-game game))))))

;;画面のタイトル表示
(defun draw-windows-title (game map-win cell-win unit-win mes-win)
  (charms:write-string-at-point map-win
				(format nil "ステージ~d" (game-stage game)) 26 0)
  (charms:write-string-at-point cell-win "地形データ" 3 0)
  (charms:write-string-at-point unit-win "ユニットデータ" 10 0)
  (charms:write-string-at-point mes-win  "メッセージ" 6 0))

;;マップに配置するキャラ表示 
(defun show-set-units (game c-win cursor mes-win)
  (clear-windows c-win mes-win)
   (loop for u across (game-player_units game)
	 for y from 1 
      do (let ((color +white/black+))
	   (when (= y (1+ cursor))
	     (setf color +black/white+))
	   (with-colors (c-win color)
	     (charms:write-string-at-point
	      c-win
	      (format nil "~A" (unit-name u))
	      1 y))))
   (charms:write-string-at-point
    mes-win
    (format nil "配置するユニットを選ん~% でください")
    1 1)
   (draw-windows-box c-win mes-win)
   (charms:write-string-at-point c-win "出撃可能ユニット" 2 0)
   (charms:write-string-at-point mes-win "メッセージ" 6 0)
   (refresh-windows c-win mes-win)
   (let ((c (charms:get-char c-win))
	 (len (length (game-player_units game))))
	 ;;(len (length (game-player_units game))))
     (cond
       ((eql c #\z) ;;決定
	(aref (game-player_units game) cursor))
       ((eql c #\q) ;;ゲーム終了
	nil)
       ((eql c (code-char charms/ll:key_up))
        (if (> 0 (1- cursor))
	    (show-set-units game c-win (1- len) mes-win)
	    (show-set-units game c-win (1- cursor) mes-win)))
       ((eql c (code-char charms/ll:key_down))
	(if (>= (1+ cursor) len)
	    (show-set-units game c-win 0 mes-win)
	    (show-set-units game c-win (1+ cursor) mes-win)))
       (t
	(show-set-units game c-win cursor mes-win)))))


;;ユニットをマップに置く
(defun set-unit-map (unit game map-win cell-win unit-win mes-win)
  (show-set-pos game map-win cell-win unit-win mes-win) ;;マップ表示
  (let ((c (charms:get-char map-win))
	(x (game-cursor_x game))
	(y (game-cursor_y game)))
    (cond
      ((eql c #\z)
       (if (and (null (get-unit x y (game-units game))) ;;ユニットがいない
		(aref (game-init_pos_area game) y x)) ;;初期配置エリア内
	   (progn
	     (setf (unit-x unit) x
		   (unit-y unit) y) ;;初期位置設定
	     (push unit (game-units_l game)) ;;全ユニットリストに追加
	     (setf (game-units game) ;;全ユニットデータ上書き
		   (make-array (length (game-units_l game)) ;;リストから配列にする
			       :initial-contents (game-units_l game)))
	     (setf (game-player_units game) ;;配置したユニットを削除
		   (remove (unit-name unit) (game-player_units game)
			   :test #'equal :key #'(lambda (x) (unit-name x)))))
	   ;;おけなかったらループ
	   (set-unit-map unit game map-win cell-win unit-win mes-win)))
      ((eql c #\q)
       (setf *game-play* nil))
      ((eql c (code-char charms/ll:key_up))
       (cursor-move game 0 -1 map-win)
       (set-unit-map unit game map-win cell-win unit-win mes-win))
      ((eql c (code-char charms/ll:key_down))
       (cursor-move game 0 1 map-win)
       (set-unit-map unit game map-win cell-win unit-win mes-win))
      ((eql c (code-char charms/ll:key_right))
       (cursor-move game 1 0 map-win)
       (set-unit-map unit game map-win cell-win unit-win mes-win))
      ((eql c (code-char charms/ll:key_left))
       (cursor-move game -1 0 map-win)
       (set-unit-map unit game map-win cell-win unit-win mes-win))
      (t (set-unit-map unit game map-win cell-win unit-win mes-win)))))

;;初期位置決める
(defun set-units-pos (game map-win cell-win unit-win mes-win)
  (let ((c-win (charms:make-window 20 10 0 (+ 2 *map-h*)))
        (num (min 7 (length (game-player_units game)))) ;;初期配置数
	(unit nil))
    (charms/ll:keypad (charms::window-pointer c-win) 1) ;;カーソルキー使えるようにする
    (loop for i from 0 below num
	 while *game-play*
       do
	 (show-set-pos game map-win cell-win unit-win mes-win) 
	 (setf unit (show-set-units game c-win 0 mes-win)) ;;配置するユニットを取り出す
	 (if unit ;; qキー押されるとゲーム終了
	     (set-unit-map unit game map-win cell-win unit-win mes-win)
	     (setf *game-play* nil)))
    (setf *set-init-pos* nil)))

;;キー入力
(defun key-down-event (game map-win unit-win atk-win)
  (let ((c (charms:get-char map-win)))
    (cond
      ((eql c #\r)
       (init-act (game-units game) +ally+)
       (setf (game-turn game) +e_turn+))
      ((eql c #\q)
       (setf *game-play* nil))
      ((eql c #\x)
       (cond
	 ;;移動したいユニットを選ぶフェイズ
         ((= (game-s_phase game) +select_unit+) 
          (init-move-area (game-move_area game)))
	 ;;選択したユニットを移動させるフェイズ
         ((= (game-s_phase game) +select_move+)
          (init-move-area (game-move_area game))
          (setf (game-s_phase game) +select_unit+
                (game-select_unit game) nil))
	 ;;移動したあとに敵を攻撃するフェイズ
         ((= (game-s_phase game) +select_attack+)
          (init-move-area (game-atk_area game))
          (setf (game-s_phase game) +select_unit+
                (unit-act? (game-select_unit game)) t
                (game-select_unit game) nil))))
      ((eql c #\z)
       (cond
	 ;;移動したいユニットを選ぶフェイズ
         ((= (game-s_phase game) +select_unit+)
          (select-unit-p game))
	 ;;選択したユニットを移動させるフェイズ
         ((= (game-s_phase game) +select_move+)
          (select-move-p game))
	 ;;移動したあとに敵を攻撃するフェイズ
         ((= (game-s_phase game) +select_attack+)
          (select-atk-p game unit-win atk-win)
          (gamen-clear atk-win))))
      ((eql c (code-char charms/ll:key_up))
       (cursor-move game 0 -1 map-win))
      ((eql c (code-char charms/ll:key_down))
       (cursor-move game 0 1 map-win))
      ((eql c (code-char charms/ll:key_right))
       (cursor-move game 1 0 map-win))
      ((eql c (code-char charms/ll:key_left))
       (cursor-move game -1 0 map-win)))))

;;ターンチェンジ
(defun check-turn-change (game map-win cell-win unit-win atk-win mes-win)
  (cond
    ;;プレイヤーうニットがすべて行動済みならターンチェンジ
    ((and (= (game-turn game) +p_turn+)
          (turn-end? (game-units game) +ally+))
     (init-act (game-units game) +ally+) ;;未行動状態に
     (setf (game-turn game) +e_turn+) ;;ターンチェンジ
     (cell-heal (game-units game) (game-cells game))) ;;地形による回復
    ((= (game-turn game) +e_turn+)
     (erase-window cell-win unit-win mes-win)
     (enemy-act (game-units game) (game-cells game) atk-win map-win) ;;全敵の行動
     (setf (game-turn game) +p_turn+) ;;敵がすべて行動したのでターンチェンジ
     (cell-heal (game-units game) (game-cells game)))))

;;クリアチェック
(defun check-stage-clear (game)
  (let ((leader (find +leader+ (game-units game) :key #'unit-rank))
        (castle (get-cell-pos +cell_castle+ (game-cells game))))
    (when (equal (list (unit-x leader) (unit-y leader)) castle)
      (if (= (game-stage game) 5) ;;ステージ５クリアしたら終わり
	  (setf *game-clear* t)
	  (setf *stage-clear* t)))))

;;メッセージウィンドウ 幅 半角24 (枠で2つかう)
(defun show-mes-win (game mes-win)
  (cond
    ((= (game-s_phase game) +select_unit+)
     (charms:write-string-at-point
      mes-win
      (format nil "移動したいユニットを選~% んでください") 1 1))
    ((= (game-s_phase game) +select_move+)
     (charms:write-string-at-point
       mes-win "移動先を選んでください" 1 1))
    ((= (game-s_phase game) +select_attack+)
     (charms:write-string-at-point
      mes-win
      (format nil "攻撃したい相手を選んで~%ください") 1 1))))

;;全ユニット回復
(defun heal-all-units (game)
  (loop for u across (game-player_units game)
     do
       (setf (unit-hp u) (unit-maxhp u))))

;;セーブするか次に行くか save-f セーブフラグセーブは一回だけできる
(defun save-or-next-stage (game save-f)
  (setf save-f nil)
  (let ((window (charms:make-window 30 6 5 5)))
    (charms:write-string-at-point
     window
     "n:次のステージへ" 1 1)
    (charms:write-string-at-point
     window
     "q:終わる" 1 2)
    (when save-f
      (charms:write-string-at-point
       window
       "s:セーブ" 1 3))
    (refresh-windows window)
    (let ((c (charms:get-char window)))
      (charms:destroy-window window)
      (cond
	((eql c #\n)
	 (setf *set-init-pos* t ;;ステージ開始初期位置設定フラグ
	       *stage-clear* nil)) ;;ステージクリアフラグ消す
	((and save-f (eql c #\s))
	 (save-suru game)
	 (save-or-next-stage game nil))
	((eql c #\q)
	 (setf *game-play* nil))
	(t
	 (save-or-next-stage game save-f))))))

;;ステージデータ・セット
(defun set-stage (game)
  (setf (game-player_units game) (get-alive-ally-units game) ;;生き残った味方ユニット
	(game-units game) nil
	(game-units_l game) nil)
  (init-move-area (game-init_pos_area game)) ;;初期位置初期化
  (heal-all-units game) ;;HP回復
  (incf (game-stage game)) ;;次のステージ
  (save-or-next-stage game t))
  

(defun hello-world ()
  (setf *random-state* (make-random-state t))
  (charms:with-curses ()
    (charms/ll:initscr)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms/ll:curs-set 0) ;;カーソル消す
    (start-color)
    (init-color)
    (let ((game (make-game :stage 1 :s_phase +select_unit+ :turn +p_turn+
			   :player_units *defo-player-units*
                           :atk_area (make-array (list *map-h* *map-w*) :initial-element nil)
                           :move_area (make-array (list *map-h* *map-w*) :initial-element nil)
			   :init_pos_area (make-array (list *map-h* *map-w*) :initial-element nil)))
          (map-win (charms:make-window (+ 2 (* 2 *map-w*)) ;;マップウィンドウ
					   (+ 2 *map-h*) 0 0)))
             ;;特殊キーを使う
      (charms/ll:keypad (charms::window-pointer map-win) 1)
     (loop named hello-world
	while *game-play*
	;;with test-win = (charms:make-window 52 3 0 (+ 2 *map-h* 15))
	with unit-win = (charms:make-window 34 15 0 (+ 2 *map-h*)) ;;ユニットデータウィンドウ
	with atk-win = (charms:make-window 36 8 0 (+ 2 *map-h*))   ;;攻撃メッセージウィンドウ
	with cell-win = (charms:make-window 18 5 35 (+ 2 *map-h*)) ;;地形データウィンドウ
	with mes-win = (charms:make-window 24 6 35 (+ 6 (+ 2 *map-h*))) ;;なんかメッセージウィンドウ
	do
            (cond
              (*game-opening* ;;オープニング
	       (game-opening-message game map-win))
	      (*stage-clear*
	       (erase-window map-win unit-win atk-win cell-win mes-win)
	       (set-stage game))
              (*game-clear* ;;ゲームクリア
                (erase-window map-win unit-win atk-win cell-win mes-win)
                (game-clear-message game map-win))
              (*game-over?* ;;ゲームオーバー
                (erase-window map-win unit-win atk-win cell-win mes-win)
                (game-over-message game map-win))
	      (*set-init-pos* ;;ステージ開始
	       ;;地形とユニットセット
	       (let ((mapu-e (nth (game-stage game) *all-enemy-map*))
		     (mapu-no (nth (game-stage game) *all-no-unit-map*)))
		 (make-cells-and-units game mapu-e mapu-no) ;;マップと敵データ作成
		 (get-init-pos-area game) ;;初期配置可能な場所取得
		 (set-units-pos game map-win cell-win unit-win mes-win) ;;ユニット配置
		 (init-act (game-units game) +ally+))) ;;未行動状態に
              (t ;;ゲーム進行中
                (gamen-clear map-win cell-win unit-win mes-win)
                ;;描画
                (show-cell-unit game map-win cell-win unit-win)
                ;;メッセージウィンドウ
                (show-mes-win game mes-win)
                ;;ウィンドウの枠表示
                (draw-windows-box map-win cell-win unit-win mes-win)
		(draw-windows-title game map-win cell-win unit-win mes-win)
                (gamen-refresh map-win cell-win unit-win mes-win)
                ;;キー入力
                (when (= (game-turn game) +p_turn+)
                  (key-down-event game map-win unit-win atk-win))
                ;;ステージクリアチェック
                (check-stage-clear game)
                ;;ターンチェンジチェック
                (check-turn-change game map-win cell-win unit-win atk-win mes-win)

                (gamen-refresh map-win cell-win unit-win atk-win mes-win)

                (sleep 0.01)))))))

