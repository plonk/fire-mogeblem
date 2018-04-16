(defparameter *map-w* 30)
(defparameter *map-h* 13)
(defparameter *game-over?* nil)
(defparameter *game-play* t)
(defparameter *game-opening* t)
(defparameter *game-clear* nil)

(defstruct game
  (cursor_x 0)
  (cursor_y 0)
  (cells nil)
  (units nil)
  (select_unit nil)
  (turn 0)
  (move_area nil)
  (atk_area nil)
  (s_phase 0))

(defparameter *map1-chara*
  (make-array (* *map-h* *map-w*) :initial-contents
    '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
      0 0 0 0 k k k 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0
      0 0 0 1 h 1 2 4 0 0 0 0 0 0 0 0 0 0 0 5 5 5 e 1 2 5 2 1 0 0
      0 0 1 1 1 1 1 4 4 0 0 0 0 0 0 6 1 1 1 1 1 1 1 1 f 1 5 1 1 0
      0 1 1 1 7 1 k 4 4 4 0 0 0 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 0
      0 1 k 1 1 1 2 4 4 4 4 4 4 1 k 1 1 1 1 1 0 0 0 b a g 1 1 1 0
      0 1 1 1 k k 1 2 4 4 4 2 1 1 1 2 1 1 1 0 0 0 0 1 6 1 1 1 1 0
      0 1 1 5 1 1 i 1 3 3 2 1 1 1 1 1 1 0 0 0 0 1 c d 1 1 1 1 0 0
      0 1 1 1 1 1 1 3 1 1 1 k 1 k 1 2 2 0 0 0 1 1 1 1 1 1 1 0 0 0
      0 0 1 1 1 k 1 1 6 1 1 1 1 1 2 2 0 0 0 2 1 j 1 1 2 1 0 0 0 0
      0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 1 1 2 1 1 1 0 0 0 0
      0 0 0 0 1 5 5 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0
      0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(defparameter *map1-no-chara*
  (make-array (* *map-h* *map-w*) :initial-contents
       '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
         0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 2 0 0 0 0
         0 0 0 1 1 1 2 4 0 0 0 0 0 0 0 0 0 0 0 5 5 5 1 1 2 5 2 1 0 0
         0 0 1 1 1 1 1 4 4 0 0 0 0 0 0 6 1 1 1 1 1 1 1 1 1 1 5 1 1 0
         0 1 1 1 7 1 1 4 4 4 0 0 0 0 1 1 1 1 1 2 1 1 1 1 1 1 1 1 1 0
         0 1 1 1 1 1 2 4 4 4 4 4 4 1 1 1 1 1 1 1 0 0 0 1 1 1 1 1 1 0
         0 1 1 1 1 1 1 2 4 4 4 2 1 1 1 2 1 1 1 0 0 0 0 1 6 1 1 1 1 0
         0 1 1 5 1 1 1 1 3 3 2 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 0 0
         0 1 1 1 1 1 1 3 1 1 1 1 1 1 1 2 2 0 0 0 1 1 1 1 1 1 1 0 0 0
         0 0 1 1 1 1 1 1 6 1 1 1 1 1 2 2 0 0 0 2 1 5 1 1 2 1 0 0 0 0
         0 0 0 1 1 1 1 1 1 1 1 2 1 1 0 0 0 0 0 0 1 1 2 1 1 1 0 0 0 0
         0 0 0 0 1 5 5 0 0 0 0 1 1 0 0 0 0 0 0 0 0 0 0 0 2 2 0 0 0 0
         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

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

(defenum:defenum phase-num
    (+select_unit+ +select_move+ +select_attack+ +select_max+))

(defenum:defenum atk-type
  (+atk_normal+ +atk_counter+ +atk_re+))

(defenum:defenum turn-num
  (+p_turn+ +e_turn+ +turn_max+))

(defenum:defenum unit-rank
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
  (movecost nil))

;;ユニットデータ
(defstruct unit
  (name nil) (job 0) (hp 0) (maxhp 0) (str 0) (skill 0)
  (w_lv 0) (agi 0) (luck 0) (def 0) (move 0) (weapon 0)
  (x 0) (y 0) (unit-num 0) (team 0) (alive? t) (act? nil) (rank 0))



(defstruct cursor
  (x 0)
  (y 0))

;;ジョブ
(defenum:defenum job
    (+job_lord+ +job_paradin+ +job_s_knight+ +job_a_knight+ +job_archer+
     +job_p_knight+ +job_pirate+ +job_hunter+ +job_thief+ +job_max+))

;;ジョブデータ配列
(defparameter *jobdescs*
  ;;movecost= (海 草原 林 山 高山 町 砦 城)
  (make-array +job_max+ :initial-contents
        (list (make-jobdesc :name "ロード" :aa "君"
                            :movecost #(-1 1 2 4 -1 1 2 2))
              (make-jobdesc :name "パラディン" :aa "聖"
                :movecost #(-1 1 3 6 -1 1 2 2))
              (make-jobdesc :name "Sナイト" :aa "騎"
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "Aナイト" :aa "重"
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "アーチャー" :aa "射"
                :movecost #(-1 1 3 -1 -1 1 2 2))
              (make-jobdesc :name "Pナイト" :aa "天"
                :movecost #(1 1 1 1 1 1 1 1))
              (make-jobdesc :name "海賊" :aa "海"
                :movecost #(2 1 2 4 -1 1 2 2))
              (make-jobdesc :name "ハンター" :aa "狩"
                :movecost #(-1 1 2 3 -1 1 2 2))
              (make-jobdesc :name "盗賊" :aa "盗"
                :movecost #(-1 1 2 4 -1 1 2 2)))))

;;武器
(defenum:defenum buki
    (+w_iron_sword+ +w_rapier+ +w_spear+ +w_silver_spear+ +w_hand_spear+
     +w_bow+ +w_steal_bow+ +w_cross_bow+ +w_ax+ +w_steal_ax+ +w_max+))

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
                    :rangemax 1))))

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

(defparameter *units-data*
  ;;       name job hp maxhp str skill w_lv agi luck def move weapon rank
  `((A . ("もげぞう"   ,+job_lord+     18 18 5  3  5  7  7  7  7 ,+ally+ ,+w_rapier+ ,+leader+))
    (B . ("ジェイガン"  ,+job_paradin+  20 20 7 10 10  8  1  9 10 ,+ally+ ,+w_iron_sword+ ,+common+))
    (C . ("カイン"     ,+job_s_knight+ 18 18 7  5  5  6  3  7  9 ,+ally+ ,+w_spear+ ,+common+))
    (D . ("アベル"     ,+job_s_knight+ 18 18 6  7  6  7  2  7  9 ,+ally+ ,+w_hand_spear+ ,+common+))
    (E . ("ドーガ"     ,+job_a_knight+ 18 18 7  3  4  3  1 11  5 ,+ally+ ,+w_iron_sword+ ,+common+))
    (F . ("ゴードン"    ,+job_archer+   16 16 5  1  5  4  4  6  5 ,+ally+ ,+w_cross_bow+ ,+common+))
    (G . ("シーダ"     ,+job_p_knight+ 16 16 3  6  7 12  9  7  8 ,+ally+ ,+w_iron_sword+ ,+common+))
    (H . ("ガザック"    ,+job_pirate+   24 24 7  3  7  8  0  6  6 ,+enemy+ ,+w_steal_ax+ ,+boss+))
    (I . ("ガルダ兵"   ,+job_hunter+   18 18 6  1  5  5  0  3  6 ,+enemy+ ,+w_bow+ ,+common+))
    (J . ("ガルダ兵"   ,+job_thief+    16 16 3  1  2  9  0  2  7 ,+enemy+ ,+w_iron_sword+ ,+common+))
    (K . ("ガルダ兵"   ,+job_pirate+   18 18 5  1  5  6  0  4  6 ,+enemy+ ,+w_ax+ ,+common+))))
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
