;;TODO 行動済み処理

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

;;ユニットのデータ作成
(defun make-unit-data (unit-c x y)
  (let ((data (cdr (assoc unit-c *units-data*))))
    (apply #'make-unit
      (append (mapcan #'list
                '(:name :job :hp :maxhp :str :skill :w_lv :agi :luck :def :move :team :weapon :rank)
                data)
              (list :unit-num unit-c :x x :y y)))))

;;地形データと全ユニットデータ作成
(defun make-cells-and-units (game map map-no-chara)
  (let ((units-list nil) (cells (make-array (list *map-h* *map-w*)))
        (units nil))
    (loop for y from 0 below *map-h* do
      (loop for x from 0 below *map-w* do
       (let ((c (aref map (+ (* *map-w* y) x))))
          (if (numberp c)
              (setf (aref cells y x) c)
              (progn
                (setf (aref cells y x) (aref map-no-chara (+ (* *map-w* y) x)))
                (push (make-unit-data c x y) units-list))))))
    ;;(setf units-list (sort units-list #'< :key #'(lambda (x) (unit-unit-num x))))
    (setf units (make-array (length units-list)
                 :initial-contents units-list))
    (setf (game-cells game) cells
          (game-units game) units)))


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

;;色作成
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
  (define-color-pair (+black/player-b+ 12) +black+ +player-b+)
  (define-color-pair (+black/p-move-b+ 13) +black+ +p-move-b+)
  (define-color-pair (+black/e-move-b+ 14) +black+ +e-move-b+)
  (define-color-pair (+black/atk-b+    15) +black+ +atk-b+))

;;疑似カーソル移動
(defun cursor-move (game x y window)
  (multiple-value-bind (width height)
      (charms:window-dimensions window)
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
(defun show-cell-data (cell window)
  (charms:write-string-at-point
   window
   (format nil "  地形  : ~a" (celldesc-name (aref *celldescs* cell)))
   1 1)
  (charms:write-string-at-point
   window
   (format nil "防御効果: ~2d%" (celldesc-def (aref *celldescs* cell)))
   1 2)
  (charms:write-string-at-point
   window
   (format nil "回復効果: ~a" (if (celldesc-heal (aref *celldescs* cell))
                           "あり" "なし"))
   1 3))

;; x y位置にユニットいたらユニットデータ返す
(defun get-unit (x y units)
  (loop for unit across units do
       (if (and (= (unit-x unit) x) (= (unit-y unit) y)
                (unit-alive? unit))
           (return-from get-unit unit))))

;;ユニットデータを表示
(defun show-unit-data (unit window)
  (let* ((weapon (unit-weapondesc unit))
         (w-name (weapondesc-name weapon))
         (w-dmg (weapondesc-damage weapon))
         (w-hit (weapondesc-hit weapon))
         (w-wei (weapondesc-weight weapon))
         (w-cri (weapondesc-critical weapon))
         (w-ranmin (weapondesc-rangemin weapon))
         (w-ranmax (weapondesc-rangemax weapon)))
   (charms:write-string-at-point
    window
    (format nil " 名前 : ~a ~a" (unit-name unit) (if (unit-act? unit) "(行動済み)" ""))
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

;;地形とユニット描画
(defun show-cell-unit (game window window2 unit-win)
  (loop for y from 0 below *map-h* do
       (loop for x from 0 below *map-w* do
        (let ((cell (aref (game-cells game) y x)) (unit (get-unit x y (game-units game)))
              (color +dark_green/green+) (aa nil))
           (if (and unit (unit-alive? unit))
               (progn
                 (if (= (unit-team unit) +ally+)
                     (setf color +black/player-b+)
                     (setf color +black/red+))
                 (setf aa (jobdesc-aa (unit-jobdesc unit))))
               (let ((can-move (aref (game-move_area game) y x)))
                 (setf color (get-cell-color cell can-move))
                 (setf aa (celldesc-aa (aref *celldescs* cell)))))
           (when (aref (game-atk_area game) y x)
             (setf color +black/atk-b+))
           (when (and (= (game-cursor_x game) x)
                      (= (game-cursor_y game) y))
               (setf color +black/white+)
               (show-cell-data cell window2)
               (when (and unit (unit-alive? unit))
                 (show-unit-data unit unit-win)))

           (with-colors (window color)
            (charms:write-string-at-point
               window aa (+ (* x 2) 1) (1+ y)))))))

;;敵移動中描画
;;地形とユニット描画
(defun show-map (cells units window)
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
         (with-colors (window color)
             (charms:write-string-at-point
                window aa (+ (* x 2) 1) (1+ y)))))))

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
         (dmg (- a-atk d-def)) (msg-y 2))
    (when (> hissatsu? (random 100))
      (setf dmg (* dmg 3))
      (charms:write-string-at-point
        atk-win "必殺の一撃！" 1 (prog1 msg-y (incf msg-y))))
    (charms:write-string-at-point
      atk-win (format nil "~aに~dのダメージを与えた" (unit-name def-unit) dmg) 1 msg-y)
    (charms:get-char atk-win)
    (decf (unit-hp def-unit) dmg)))

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
  (let* ((a-skill (unit-skill atk-unit))
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
       (give-damage atk-unit def-unit a-w-dmg a-w-cri atk-win)
       (miss-msg atk-win))
   ;;反撃
   (when (and (> (unit-hp def-unit) 0) (= atk-type +atk_normal+)
              (>= d-r-max dist d-r-min))
     (attack! def-unit atk-unit cells +atk_counter+ atk-win))
   ;;再攻撃
   (when (and (> a-speed d-speed) (= atk-type +atk_normal+)
              (> (unit-hp def-unit) 0) (> (unit-hp atk-unit) 0))
     (attack! atk-unit def-unit cells +atk_re+ atk-win))
   ;;死亡判定
   (when (= atk-type +atk_normal+)
     (cond
       ((>= 0 (unit-hp def-unit))
        (when (= (unit-rank def-unit) +leader+)
          (setf *game-over?* t))
        (setf (unit-alive? def-unit) nil)
        (dead-msg def-unit atk-unit atk-win))
       ((>= 0 (unit-hp atk-unit))
        (when (= (unit-rank atk-unit) +leader+)
          (setf *game-over?* t))
        (setf (unit-alive? atk-unit) nil)
        (dead-msg atk-unit def-unit atk-win))))

   (charms:refresh-window atk-win)))

;;攻撃先選択フェイズでzキー押したとき
(defun select-atk-p (game atk-win)
  (let* ((x (game-cursor_x game)) (y (game-cursor_y game))
         (atk-unit (game-select_unit game))
         (def-unit (get-unit x y (game-units game))))
    (when (and def-unit (aref (game-atk_area game) y x)
               (/= (unit-team atk-unit) (unit-team def-unit)))
      (attack! atk-unit def-unit (game-cells game) +atk_normal+ atk-win)
      (setf (game-s_phase game) +select_unit+
            (unit-act? atk-unit) t
            (game-select_unit game) nil)
      (init-move-area (game-atk_area game)))))

;;全員行動済みかチェック
(defun turn-end? (units team)
  (every #'unit-act?
        (remove-if-not (lambda (u) (= team (unit-team u))) units)))

;;行動済み初期化
(defun init-act (units team)
  (loop for u across units
        when (= team (unit-team u))
        do
        (setf (unit-act? u) nil)))

;;unitに一番近いキャラ
(defun near-chara (unit units)
  (first
    (sort (remove-if (lambda (u) (or (not (unit-alive? u))
                                     (= (unit-team u) (unit-team unit))))
                     (coerce units 'list))
     #'<
     :key (lambda (u)
            (m-dist (unit-x unit) (unit-y unit) (unit-x u) (unit-y u))))))

;;敵が動いたら画面描画
(defun show-enemy-move (units cells window)
  (charms:clear-window window)
  (show-map cells units window)
  (draw-window-box window)
  (charms:write-string-at-point
    window "敵のターン" 28 0)
  (charms:refresh-window window)
  (sleep 0.15))


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
  (let* ((block-cell2 (remove goal block-cell :test #'equal))
         (sin-goal (first (sort (get-move-list unit units cells)
                                #'<
                                :key #'(lambda (x) (astar x goal cells movecost block-cell2)))))
        (cost 0) (paths nil))
    (setf (values cost paths)
          (astar start sin-goal cells movecost
                 (remove sin-goal block-cell :test #'equal)))
    paths))


;;a-starで移動ルート探索(城行き)
(defun astar-to-castle (unit cells units movecost window)
  (let* ((start (list (unit-x unit) (unit-y unit)))
         (castle (get-cell-pos +cell_castle+ cells))
         (block-cell (get-block-cell unit units))
         (paths (all-astar start castle unit units cells movecost block-cell)))
    (when (not (equal start castle))
      (loop for path in paths do
        (setf (unit-x unit) (car path)
              (unit-y unit) (cadr path))
        ;;敵が一歩動いたら画面描画
        (show-enemy-move units cells window)))))

;;a-starで移動ルート探索
(defun astar-move (unit target cells units r-max movecost window)
  (let* ((start (list (unit-x unit) (unit-y unit)))
         (goal  (list (unit-x target) (unit-y target)))
         (block-cell (get-block-cell unit units))
         (paths (all-astar start goal unit units cells movecost block-cell)))
    (loop for path in paths do
      (setf (unit-x unit) (car path)
            (unit-y unit) (cadr path))
      ;;敵が一歩動いたら画面描画
      (show-enemy-move units cells window)
      (when (and (>= r-max (unit-dist unit target))
                 (null (get-unit (unit-x unit) (unit-y unit) units)))
        (return)))))

;;敵の移動
(defun enemy-move (unit target r-max units cells window)
  ;;(init-move-area move-area)
  ;;(get-move-area (unit-x unit) (unit-y unit) unit units cells move-area)
  (let ((movecost (jobdesc-movecost (unit-jobdesc unit))))
    (if (= (unit-rank unit) +boss+)
        (astar-to-castle unit cells units movecost window)
        (astar-move unit target cells units r-max movecost window))))
        ;;(tekito-move 0 0 unit units target r-max move-area movecost move cells window))))

;;敵の攻撃
(defun enemy-attack (atk-unit def-unit cells atk-win)
  (attack! atk-unit def-unit cells +atk_normal+ atk-win)
  (charms:clear-window atk-win)
  (charms:refresh-window atk-win))

;;敵の行動 攻撃範囲に相手ユニットがいたら攻撃する
(defun enemy-act (units cells atk-win window)
  (loop while (null *game-over?*)
        for u across units
        when (and (>= (unit-team u) +enemy+)
                  (unit-alive? u))
        do
        (let* ((weapon (unit-weapondesc u))
               (target (near-chara u units))
               (r-min (weapondesc-rangemin weapon))
               (r-max (weapondesc-rangemax weapon))
               (dist (unit-dist u target)))
          (if (>= r-max dist r-min) ;;攻撃範囲に相手がいる
              (enemy-attack u target cells atk-win)
              (progn ;;移動後に攻撃範囲に相手がいたら攻撃
                (enemy-move u target r-max units cells window)
                (setf target (near-chara u units))
                (when (>= r-max (unit-dist u target) r-min)
                  (enemy-attack u target cells atk-win)))))))

;;回復地形効果
(defun cell-heal (units cells)
  (loop for u across units
    do
    (when (and (celldesc-heal (celldesc-yx cells (unit-y u) (unit-x u)))
               (> (unit-maxhp u) (unit-hp u)))
      (setf (unit-hp u)
            (min (+ (unit-hp u) (floor (unit-maxhp u) 10))
                 (unit-maxhp u))))))

;;画面クリア
(defun gamen-clear (&rest window)
 (dolist (win window)
   (charms:clear-window win)))

;;画面リフレッシュ
(defun gamen-refresh (&rest window)
 (dolist (win window)
   (charms:refresh-window win)))

;;ゲーム状態初期化
(defun init-game (game)
  (setf (game-cursor_x game)              0
        (game-cursor_y game)              0
        (game-cells game)               nil
        (game-units game)               nil
        (game-select_unit game)         nil
        (game-turn game)           +p_turn+
        (game-s_phase game)   +select_unit+
        (game-move_area game) (make-array (list *map-h* *map-w*) :initial-element nil)
        (game-atk_area game)  (make-array (list *map-h* *map-w*) :initial-element nil)))

;; game-over
(defparameter *gameobera* '(
"                                                    __ __ "
" _____ _____ _____ _____    _____ _____ _____ _____|  |  |"
"|   __|  _  |     |   __|  |     |  |  |   __| __  |  |  |"
"|  |  |     | | | |   __|  |  |  |  |  |   __|    -|__|__|"
"|_____|__|__|_|_|_|_____|  |_____|\\___/|_____|__|__|__|__|"))

(defun game-over-message (game window)
  (let ((len (length *gameobera*)))
    (dotimes (i len)
      (charms:write-string-at-point window (nth i *gameobera*) 0 (1+ i)))
    (charms:write-string-at-point window "q:終わる" 15 (+ len 2))
    (charms:write-string-at-point window "r:再挑戦" 15 (+ len 3))
    (gamen-refresh window)
    (let ((c (charms:get-char window)))
      (cond
        ((eql c #\q)
         (setf *game-play* nil))
        ((eql c #\r)
         (setf *game-over?* nil)
         (init-game game))))))

;;画面の枠表示
(defun draw-windows (window window2 unit-win)
  (draw-window-box window)
  (draw-window-box window2)
  (draw-window-box unit-win)

  ;;(charms:write-string-at-point
   ;;window
  ;;(format nil "x:~d y:~D" (cursor-x cursor) (cursor-y cursor)) 28 0)
   ;;(format nil "turn:~d" turn) 28 0)
  (charms:write-string-at-point window "マップ" 28 0)
  (charms:write-string-at-point window2 "地形データ" 3 0)
  (charms:write-string-at-point unit-win "ユニットデータ" 10 0))

;;キー入力
(defun key-down-event (game window atk-win)
  (let ((c (charms:get-char window)))
    (cond
      ((eql c #\r)
       (init-act (game-units game) +ally+)
       (setf (game-turn game) +e_turn+))
      ((eql c #\q)
       (setf *game-play* nil))
      ((eql c #\x)
       (cond
         ((= (game-s_phase game) +select_unit+)
          (init-move-area (game-move_area game)))
         ((= (game-s_phase game) +select_move+)
          (init-move-area (game-move_area game))
          (setf (game-s_phase game) +select_unit+
                (game-select_unit game) nil))
         ((= (game-s_phase game) +select_attack+)
          (init-move-area (game-atk_area game))
          (setf (game-s_phase game) +select_unit+
                (unit-act? (game-select_unit game)) t
                (game-select_unit game) nil))))
      ((eql c #\z)
       (cond
         ((= (game-s_phase game) +select_unit+)
          (select-unit-p game))
         ((= (game-s_phase game) +select_move+)
          (select-move-p game))
         ((= (game-s_phase game) +select_attack+)
          (select-atk-p game atk-win))))
      ((eql c (code-char charms/ll:key_up))
       (cursor-move game 0 -1 window))
      ((eql c (code-char charms/ll:key_down))
       (cursor-move game 0 1 window))
      ((eql c (code-char charms/ll:key_right))
       (cursor-move game 1 0 window))
      ((eql c (code-char charms/ll:key_left))
       (cursor-move game -1 0 window)))))

;;ターンチェンジ
(defun check-turn-change (game window atk-win)
  (cond
    ((and (= (game-turn game) +p_turn+)
          (turn-end? (game-units game) +ally+))
     (init-act (game-units game) +ally+)
     (setf (game-turn game) +e_turn+)
     (cell-heal (game-units game) (game-cells game)))
    ((= (game-turn game) +e_turn+)
     (enemy-act (game-units game) (game-cells game) atk-win window)
     (setf (game-turn game) +p_turn+)
     (cell-heal (game-units game) (game-cells game)))))

(defun hello-world ()
  (setf *random-state* (make-random-state t))
  (charms:with-curses ()
    (charms/ll:initscr)
    (charms:disable-echoing)
    (charms:enable-raw-input)
    (start-color)
    (init-color)
    (let ((game (make-game :s_phase +select_unit+ :turn +p_turn+
                           :atk_area (make-array (list *map-h* *map-w*) :initial-element nil)
                           :move_area (make-array (list *map-h* *map-w*) :initial-element nil)))
          (window (charms:make-window (+ 2 (* 2 *map-w*))
                                      (+ 2 *map-h*) 0 0)))
             ;;特殊キーを使う
      (charms/ll:keypad (charms::window-pointer window) 1)
     (loop named hello-world
           while *game-play*
           ;;with game-over-win = (charms:make-window 80 15 0 0)
           with unit-win = (charms:make-window 34 14 0 (+ 2 *map-h*))
           with atk-win = (charms:make-window 36 8 0 (+ 2 *map-h*))
           with window2 = (charms:make-window 18 5 35 (+ 2 *map-h*))
           do

            (when *game-over?*
              (gamen-clear window window2 unit-win atk-win)
              (gamen-refresh window window2 unit-win atk-win)
              (game-over-message game window))
            (when (null *game-over?*)
              (gamen-clear window window2 unit-win)
              ;;地形とユニットセット
              (if (or (null (game-cells game)) (null (game-units game)))
                  (make-cells-and-units game *map1-chara* *map1-no-chara*))

              ;;描画
              (show-cell-unit game window window2 unit-win)
              ;;ウィンドウの枠表示
              (draw-windows window window2 unit-win)
              (gamen-refresh window window2 unit-win)
              ;;キー入力
              (when (= (game-turn game) +p_turn+)
                (key-down-event game window atk-win))

              ;;ターンチェンジチェック
              (check-turn-change game window atk-win)

              (gamen-refresh window window2 unit-win)

              (sleep 0.01))))))
