(defparameter *map-w* 30)
(defparameter *map-h* 13)
(defparameter *game-over?* nil)
(defparameter *game-play* t)
(defparameter *game-opening* t)
(defparameter *stage-clear* nil)
(defparameter *game-clear* nil)
(defparameter *set-init-pos* t)
(defparameter *load-units-data* nil)
(defparameter *load-stage* 0)

(defstruct game
  (cursor_x 0)
  (cursor_y 0)
  (cells nil)
  (units nil)
  (units_l nil)
  (player_units nil)
  (select_unit nil)
  (turn 0)
  (move_area nil)
  (atk_area nil)
  (init_pos_area nil) ;;初期位置エリア
  (s_phase 0)
  (stage 1))

(defstruct windows
  (map nil)
  (unit nil)
  (atk nil)
  (mes nil)
  (cell nil))


;;周囲のマス
(defparameter *ar-cell* '((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1) (0 0)))

;;f:文字色 b:背景色
(defconstant +black+   charms/ll:COLOR_BLACK)
(defconstant +red+     charms/ll:COLOR_RED)
(defconstant +green+   charms/ll:COLOR_GREEN)
(defconstant +yellow+  charms/ll:COLOR_YELLOW)
(defconstant +blue+    charms/ll:COLOR_BLUE)
(defconstant +magenta+ charms/ll:COLOR_MAGENTA)
(defconstant +cyan+    charms/ll:COLOR_CYAN)
(defconstant +white+   charms/ll:COLOR_WHITE)
(defconstant +dark_green+   22)
(defconstant +low-yama-f+  214)
(defconstant +low-yama-b+  166)
(defconstant +high-yama-f+ 248)
(defconstant +high-yama-b+ 244)
(defconstant +town-b+       51)
(defconstant +fort-b+      201)
(defconstant +castle-b+    226)
(defconstant +player-b+     39)
(defconstant +p-move-b+    141)
(defconstant +e-move-b+    160)
(defconstant +atk-b+       202)

(defparameter +white/blue+ nil)
(defparameter +black/red+ nil)
(defparameter +black/white+ nil)
(defparameter +black/green+ nil)
(defparameter +green/black+ nil)
(defparameter +dark_green/green+ nil)
(defparameter +low-yama-f/low-yama-b+ nil)
(defparameter +high-yama-f/high-yama-b+ nil)
(defparameter +black/town-b+ nil)
(defparameter +black/fort-b+ nil)
(defparameter +black/castle-b+ nil)
(defparameter +black/player-b+ nil)
(defparameter +black/p-move-b+ nil)
(defparameter +black/e-move-b+ nil)
(defparameter +black/atk-b+    nil)
(defparameter +black/yellow+   nil)
(defparameter +white/black+    nil)

;;画面をクリア
(defun clear-windows (&rest window)
  (dolist (win window)
    (charms:clear-window win)))

;;画面描画
(defun refresh-windows (&rest window)
  (dolist (win window)
    (charms:refresh-window win)))

;;ウィンドウ消す
(defun erase-window (&rest window)
  (dolist (win window)
    (charms:clear-window win)
    (charms:refresh-window win)))


;;windowの枠
(defun draw-window-border (window
                           &optional
                             (ls #\|) (rs #\|) (ts #\-) (bs #\-)
                             (tl #\+) (tr #\+) (bl #\+) (br #\+))
  (apply #'charms/ll:wborder (charms::window-pointer window)
         (mapcar #'char-code (list ls rs ts bs tl tr bl br))))

(defun draw-window-box (window &optional (verch #\|) (horch #\-))
  (charms/ll:box (charms::window-pointer window) (char-code verch) (char-code horch)))

;;枠表示
(defun draw-windows-box (&rest window)
  (dolist (win window)
    (draw-window-box win)))

(defenum:defenum phase-num
    (+select_unit+ +select_move+ +select_attack+ +select_max+))

(defenum:defenum atk-type
  (+atk_normal+ +atk_counter+ +atk_re+))

(defenum:defenum turn-num
  (+p_turn+ +e_turn+ +turn_max+))

(defenum:defenum unit-rank2
  (+common+ +leader+ +boss+))
;;地形データ
(defstruct celldesc
  (name  nil)
  (aa    nil)
  (def     0)
  (heal  nil))

;;武器データ
(defstruct weapondesc
  (name   nil)
  (damage   0)
  (weight   0)
  (hit      0)
  (critical 0)
  (rangeMin 0)
  (rangeMax 0))

;;ジョブデータ
(defstruct jobdesc
  (name nil)
  (aa   nil)
  (give_exp 0) ;;倒されたときに相手に与える経験値
  (movecost nil))

;;ユニットデータ
(defstruct unit
  (name nil) (job 0) (hp 0) (maxhp 0) (str 0) (skill 0) (give_exp 0)
  (w_lv 0) (agi 0) (luck 0) (def 0) (move 0) (weapon 0) (exp 0) (lv 1)
  (x 0) (y 0) (unit-num 0) (team 0) (alive? t) (act? nil) (rank 0)
  (lvup nil))



(defstruct cursor
  (x 0)
  (y 0))



;;武器
(defenum:defenum buki
    (+w_iron_sword+ +w_rapier+ +w_spear+ +w_silver_spear+ +w_hand_spear+
		    +w_bow+ +w_steal_bow+ +w_cross_bow+ +w_ax+ +w_steal_ax+
		    +w_silver_sword+ +w_max+))

;;武器データ配列
(defparameter *weapondescs*
  (make-array +w_max+ :initial-contents
        (list (make-weapondesc :name "鉄の剣" :damage 5 :weight 2
                               :hit 100 :critical 0 :rangemin 1
                               :rangemax 1)
              (make-weapondesc :name "レイピア" :damage 5 :weight 1
                    :hit 100 :critical 10 :rangemin 1
                    :rangemax 1)
              (make-weapondesc :name "やり" :damage 8 :weight 6
                    :hit 80 :critical 0 :rangemin 1
                    :rangemax 1)
              (make-weapondesc :name "銀の槍" :damage 12 :weight 7
                    :hit 80 :critical 0 :rangemin 1
                    :rangemax 1)
              (make-weapondesc :name "てやり" :damage 7 :weight 6
                    :hit 70 :critical 0 :rangemin 1
                    :rangemax 2)
              (make-weapondesc :name "ゆみ" :damage 4 :weight 1
                    :hit 90 :critical 0 :rangemin 2
                    :rangemax 2)
              (make-weapondesc :name "鋼の弓" :damage 7 :weight 3
                    :hit 80 :critical 0 :rangemin 2
                    :rangemax 2)
              (make-weapondesc :name "ボウガン" :damage 5 :weight 2
                    :hit 100 :critical 20 :rangemin 2
                    :rangemax 2)
              (make-weapondesc :name "おの" :damage 7 :weight 7
                    :hit 80 :critical 0 :rangemin 1
                    :rangemax 1)
              (make-weapondesc :name "鋼の斧" :damage 9 :weight 9
                    :hit 70 :critical 0 :rangemin 1
                    :rangemax 1)
	      (make-weapondesc :name "銀の剣" :damage 12 :weight 3
                    :hit 100 :critical 0 :rangemin 1
                    :rangemax 1)
	      )))

;;地形
(defenum:defenum cell
    (+cell_sea+ +cell_plane+ +cell_forest+ +cell_mt+ +cell_high_mt+
     +cell_town+ +cell_fort+ +cell_castle+ +cell_max+))

;;地形データ配列
(defparameter *celldescs*
  (make-array +cell_max+ :initial-contents
    (list (make-celldesc :name "海"   :aa "〜" :def 30)
          (make-celldesc :name "草原" :aa "．" :def 5)
          (make-celldesc :name "林"   :aa "林" :def 15)
          (make-celldesc :name "山"   :aa "山" :def 25)
          (make-celldesc :name "高山" :aa "山" :def 0)
          (make-celldesc :name "町"   :aa "町" :def 0)
          (make-celldesc :name "砦"   :aa "砦" :def 20 :heal t)
          (make-celldesc :name "城"   :aa "城" :def 30 :heal t))))

(defenum:defenum team
    (+ally+ +enemy+ +type_max+))

;;ジョブ
(defenum:defenum job
    (+job_lord+ +job_paradin+ +job_s_knight+ +job_a_knight+ +job_archer+
		+job_p_knight+ +job_pirate+ +job_hunter+ +job_thief+ +job_bandit+
		+job_d_knight+ +job_shogun+ +job_mercenary+ +job_yusha+ +job_max+))

;;ジョブデータ配列
(defparameter *jobdescs*
  ;;movecost= (海 草原 林 山 高山 町 砦 城)
  (make-array +job_max+ :initial-contents
        (list (make-jobdesc :name "ロード" :aa "君" :give_exp 0
                            :movecost #(-1 1 2 4 -1 1 2 2))
              (make-jobdesc :name "パラディン" :aa "聖" :give_exp 44
                :movecost #(-1 1 3 6 -1 1 2 2))
              (make-jobdesc :name "Sナイト" :aa "騎" :give_exp 30
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "Aナイト" :aa "重" :give_exp 32
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "アーチャー" :aa "射" :give_exp 28
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "Pナイト" :aa "天" :give_exp 36
                :movecost #(1 1 1 1 1 1 1 1))
              (make-jobdesc :name "海賊" :aa "海" :give_exp 24
                :movecost #(2 1 2 4 -1 1 2 2))
              (make-jobdesc :name "ハンター" :aa "狩" :give_exp 26
                :movecost #(-1 1 2 3 -1 1 2 2))
              (make-jobdesc :name "盗賊" :aa "盗" :give_exp 100 ;;40
			    :movecost #(-1 1 2 4 -1 1 2 2))
	      (make-jobdesc :name "山賊" :aa "さ" :give_exp 24
			    :movecost #(-1 1 2 1 2 1 2 2))
	      (make-jobdesc :name "Dナイト" :aa "竜" :give_exp 44
			    :movecost #(1 1 1 1 1 1 1 1))
	      (make-jobdesc :name "将軍" :aa "将" :give_exp 50
			    :movecost #(-1 1 2 4 -1 1 2 2))
	      (make-jobdesc :name "傭兵" :aa "傭" :give_exp 50
			    :movecost #(-1 1 2 3 -1 1 1 2))
	      (make-jobdesc :name "勇者" :aa "勇" :give_exp 50
			    :movecost #(-1 1 2 3 -1 1 1 2))
	      )))

(defparameter *units-data* ;;A~Gも敵データにしてよい
  ;;       name job hp maxhp str skill w_lv agi luck def give-exp move weapon rank
  `((A . ("プロピアキャスター" ,+job_pirate+   24 24 7  3  7  8  0  6 24  6 ,+enemy+ ,+w_steal_ax+ ,+boss+)) ;;ステージ１ボス
    (B . ("もび太"     ,+job_bandit+   27 27 8  3  7  8  0  6 26  6 ,+enemy+ ,+w_steal_ax+ ,+boss+)) ;;ステージ２ボス
    (C . ("ハツネツA"  ,+job_shogun+   28 28 9  1 3  4  0  14 50  5 ,+enemy+ ,+w_silver_spear+ ,+boss+)) ;;ステージ３ボス
    (D . ("リスパー"   ,+job_paradin+  27 27 8  7 10 11  0  9 44 10 ,+enemy+ ,+w_rapier+ ,+boss+)) ;;ステージ4ボス
    (E . ("モーゲ皇帝" ,+job_yusha+    30 30 8 14 10 14  0 10 46  7 ,+enemy+ ,+w_silver_sword+ ,+boss+)) ;;ステージ5ボス
    (F . ("ゴードン"   ,+job_archer+   16 16 5  1  5  4  4  6 28  5 ,+ally+ ,+w_cross_bow+ ,+common+))
    (G . ("シーダ"     ,+job_p_knight+ 16 16 3  6  7 12  9  7 36  8 ,+ally+ ,+w_iron_sword+ ,+common+))
    (H . ("ペカ民兵"   ,+job_hunter+   18 18 6  1  5  5  0  3 26  6 ,+enemy+ ,+w_bow+ ,+common+))
    (I . ("ペカ民兵"   ,+job_hunter+   18 18 6  1  5  5  0  3 26  6 ,+enemy+ ,+w_bow+ ,+common+))
    (J . ("ペカ民兵"   ,+job_thief+    16 16 3  1  2  9  0  2 40  7 ,+enemy+ ,+w_iron_sword+ ,+common+))
    (K . ("ペカ民兵"   ,+job_pirate+   18 18 5  1  5  6  0  4 24  6 ,+enemy+ ,+w_ax+ ,+common+))
    (L . ("ペカ民兵"   ,+job_bandit+   20 20 5  1  5  5  0  3 26  6 ,+enemy+ ,+w_ax+ ,+common+))
    (M . ("ペカ民兵"  ,+job_mercenary+ 16 16 4  8  8 10  0  5 28  7 ,+enemy+ ,+w_iron_sword+ ,+common+))
    (N . ("ペカ民兵"   ,+job_d_knight+ 22 22 9  3 10  6  0 14 44 10 ,+enemy+ ,+w_ax+ ,+common+))
    (O . ("ペカ民兵"   ,+job_s_knight+ 16 16 5  2  8  6  0  7 30  9 ,+enemy+ ,+w_spear+ ,+common+))
    (P . ("ペカ民兵"   ,+job_a_knight+ 16 16 5  1  7  3  0 11 32  5 ,+enemy+ ,+w_iron_sword+ ,+common+))))

;;lvup = ステータス上昇率 (HP 力 技 武器 速さ 運 守備 魔防)
(defparameter *defo-player-units*
  (make-array 7 :initial-contents
        (list (make-unit :name "もげぞう" :job +job_lord+ :hp 18 :maxhp 18
                         :str 50 :skill 3 :w_lv 5 :agi 7 :luck 7 :def 7
			 :lvup '(90 50 40 30 50 70 20 0)
                         :move 7 :weapon +w_rapier+ :team +ally+ :rank +leader+)
              (make-unit :name "ジェイガン" :job +job_paradin+ :hp 20 :maxhp 20
			 :str 7 :skill 10 :w_lv 10 :agi 8 :luck 1 :def 9
			 :lvup '(10 10 10 0 10 0 0 0)
			 :move 10 :weapon +w_iron_sword+ :team +ally+ :rank +common+)
              (make-unit :name "カイン" :job +job_s_knight+ :hp 18 :maxhp 18
			 :lvup '(90 30 60 60 60 50 20 0)
			 :str 7 :skill 5 :w_lv 5 :agi 6 :luck 3 :def 7
			 :move 9 :weapon +w_spear+ :team +ally+ :rank +common+)
              (make-unit :name "アベル" :job +job_s_knight+ :hp 18 :maxhp 18
			 :lvup '(70 40 50 70 50 40 20 0)
			 :str 6 :skill 7 :w_lv 6 :agi 7 :luck 2 :def 7
			 :move 9 :weapon +w_hand_spear+ :team +ally+ :rank +common+)
              (make-unit :name "ドーガ" :job +job_a_knight+ :hp 18 :maxhp 18
			 :lvup '(60 20 40 20 40 20 10 0)
			 :str 7 :skill 3 :w_lv 4 :agi 3 :luck 1 :def 11
			 :move 5 :weapon +w_iron_sword+ :team +ally+ :rank +common+)
              (make-unit :name "ゴードン" :job +job_archer+ :hp 16 :maxhp 16
			 :lvup '(40 30 30 50 30 40 10 0)
			 :str 5 :skill 1 :w_lv 5 :agi 4 :luck 4 :def 6
			 :move 5 :weapon +w_cross_bow+ :team +ally+ :rank +common+)
              (make-unit :name "シーダ" :job +job_p_knight+ :hp 16 :maxhp 16
			 :lvup '(50 20 70 80 90 70 20 0)
			 :str 3 :skill 6 :w_lv 7 :agi 12 :luck 9 :def 7
			 :move 8 :weapon +w_iron_sword+ :team +ally+ :rank +common+))))

#|
(defparameter *units*
  (make-array 11 :initial-contents
        (list (make-unit :name "もげぞう" :job +job_lord+ :hp 18 :maxhp 18
                         :str 5 :skill 3 :w_lv 5 :agi 7 :luck 7 :def 7
                         :move 7 :weapon +w_rapier+)
              (make-unit :name "ジェイガン" :job +job_paradin+ :hp 20 :maxhp 20
                     :str 7 :skill 10 :w_lv 10 :agi 8 :luck 1 :def 9
                     :move 10 :weapon +w_iron_sword+)
              (make-unit :name "カイン" :job +job_s_knight+ :hp 18 :maxhp 18
                     :str 7 :skill 5 :w_lv 5 :agi 6 :luck 3 :def 7
                     :move 9 :weapon +w_spear+)
              (make-unit :name "アベル" :job +job_s_knight+ :hp 18 :maxhp 18
                     :str 6 :skill 7 :w_lv 6 :agi 7 :luck 2 :def 7
                     :move 9 :weapon +w_hand_spear+)
              (make-unit :name "ドーガ" :job +job_a_knight+ :hp 18 :maxhp 18
                     :str 7 :skill 3 :w_lv 4 :agi 3 :luck 1 :def 11
                     :move 5 :weapon +w_iron_sword+)
              (make-unit :name "ゴードン" :job +job_archer+ :hp 16 :maxhp 16
                     :str 5 :skill 1 :w_lv 5 :agi 4 :luck 4 :def 6
                     :move 5 :weapon +w_cross_bow+)
              (make-unit :name "シーダ" :job +job_p_knight+ :hp 16 :maxhp 16
                     :str 3 :skill 6 :w_lv 7 :agi 12 :luck 9 :def 7
                     :move 8 :weapon +w_iron_sword+)
              (make-unit :name "ガザック" :job +job_pirate+ :hp 24 :maxhp 24
                     :str 7 :skill 3 :w_lv 7 :agi 8 :luck 0 :def 6
                     :move 6 :weapon +w_steal_ax+)
              (make-unit :name "ガルダ兵" :job +job_hunter+ :hp 18 :maxhp 18
                     :str 6 :skill 1 :w_lv 5 :agi 5 :luck 0 :def 3
                     :move 6 :weapon +w_bow+)
              (make-unit :name "ガルダ兵" :job +job_thief+ :hp 16 :maxhp 16
                     :str 3 :skill 1 :w_lv 2 :agi 9 :luck 0 :def 2
                     :move 7 :weapon +w_iron_sword+)
              (make-unit :name "ガルダ兵" :job +job_pirate+ :hp 18 :maxhp 18
                     :str 5 :skill 1 :w_lv 5 :agi 6 :luck 0 :def 4
                     :move 6 :weapon +w_ax+))))

|#
;;debug
(defparameter *units* nil)
(defparameter *cells* nil)
;;(setf (values *cells* *units*)
;;      (make-cells-and-units *map1-chara* *map1-no-chara*))
