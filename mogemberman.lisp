;;trivial-gamekitの関数の外でai読み込みしないとだめらしい
(ql:quickload '(:trivial-gamekit :cl-bodge :jonathan :cl-ppcre))

(defpackage moge
  (:use :cl :trivial-gamekit))

(in-package moge)

(defparameter *window-w* 800)
(defparameter *window-h* 600)
(defparameter *tate* 12) ;;13
(defparameter *yoko* 16) ;;17
(defvar *white* (vec4 1 1 1 1))
(defvar *black* (vec4 0 0 0 1))
(defvar *red*   (vec4 1 0 0 1))
(defvar *green* (vec4 0 1 0 1))
(defvar *blue*  (vec4 0 0 1 1))
(defvar *yellow* (vec4 1 1 0 1))
(defvar *game* nil)
(defvar *player* nil)
(defvar *keystate* nil)
(defparameter *gamespeed-list* (list 60 30 15 7 1))
(defparameter *gamespeed-count* 0)
(defparameter *draw-gamespeed-list* (list "通常" "2倍速" "4倍速" "8倍速" "高速"))
(defparameter *frame* 0)
(defparameter *gamespeed* 60)

(defparameter *falling-wall*
  '((1 11) (2 11) (3 11) (4 11) (5 11) (6 11) (7 11) (8 11) (9 11)
    (10 11) (11 11) (12 11) (13 11) (14 11) (15 11) (15 10) (15 9) (15 8)
    (15 7) (15 6) (15 5) (15 4) (15 3) (15 2) (15 1) (14 1) (13 1) (12 1)
    (11 1) (10 1) (9 1) (8 1) (7 1) (6 1) (5 1) (4 1) (3 1) (2 1) (1 1)
    (1 2) (1 3) (1 4) (1 5) (1 6) (1 7) (1 8) (1 9) (1 10)
    (2 10) (3 10) (4 10) (5 10) (6 10) (7 10) (8 10) (9 10) (10 10)
    (11 10) (12 10) (13 10) (14 10)
    (14 9) (14 8) (14 7) (14 6) (14 5) (14 4) (14 3) (14 2)
    (13 2) (12 2) (11 2) (10 2) (9 2) (8 2) (7 2) (6 2) (5 2) (4 2) (3 2) (2 2)
    (2 3) (2 4) (2 5) (2 6) (2 7) (2 8) (2 9)
    (3 9) (4 9) (5 9) (6 9) (7 9) (8 9) (9 9) (10 9) (11 9) (12 9) (13 9)
    (13 8) (13 7) (13 6) (13 5) (13 4) (13 3)
    (12 3) (11 3) (10 3) (9 3) (8 3) (7 3) (6 3) (5 3) (4 3) (3 3)
    (3 4) (3 5) (3 6) (3 7) (3 8)
    ))


(defclass game ()
  ((walls    :initarg :walls    :initform nil :accessor walls)
   (blocks   :initarg :blocks   :initform nil :accessor blocks)
   (players  :initarg :players  :initform nil :accessor players)
   (bombs    :initarg :bombs    :initform nil :accessor bombs)
   (items    :initarg :items    :initform nil :accessor items)
   (fires    :initarg :fires    :initform nil :accessor fires)
   (turn     :initarg :turn     :initform 1   :accessor turn)
   (winner   :initarg :winner   :initform nil :accessor winner)
   (state    :initarg :state    :initform t   :accessor state)
   (control  :initarg :control  :initform nil :accessor control)
   ))

(defclass player ()
  ((pos          :initarg :pos          :initform nil :accessor pos)
   (id           :initarg :id           :initform 0   :accessor id)
   (img          :initarg :img          :initform 0   :accessor img)
   (isalive      :initarg :isalive      :initform t   :accessor isalive)
   (name         :initarg :name         :initform nil :accessor name)
   (playing      :initarg :playing      :initform nil :accessor playing)
   (kick         :initarg :kick         :initform nil :accessor kick)
   (action       :initarg :action       :initform nil :accessor action)
   (power        :initarg :power        :initform 2   :accessor power)
   (actmove      :initarg :actmove      :initform "STAY"   :accessor actmove)
   (actbomb      :initarg :actbomb      :initform "false"   :accessor actbomb)
   (setbomblimit :initarg :setbomblimit :initform 2   :accessor setbomblimit)
   (setbombcount :initarg :setbombcount :initform 0   :accessor setbombcount)))

(defclass bomb ()
   ((pos     :initarg :pos    :initform nil :accessor pos)
    (timer   :initarg :timer  :initform 0   :accessor timer)
    (id   :initarg :id  :initform 0   :accessor id)
    (kicked  :initarg :kicked :initform nil :accessor kicked)
    (power   :initarg :power  :initform t   :accessor power)))

(defclass fire ()
   ((pos   :initarg :pos  :initform nil :accessor pos)
    (img   :initarg :img  :initform 0   :accessor img)))

(defclass item ()
   ((pos     :initarg :pos   :initform nil :accessor pos)
    (name    :initarg :name  :initform nil   :accessor name)
    (img   :initarg :img  :initform 0   :accessor img)))

(defclass com (player)
  ((ai-stream     :initarg :ai-stream    :initform nil :accessor ai-stream)
   (name     :initarg :name    :initform nil :accessor name)
   (proc     :initarg :proc    :initform nil :accessor proc)
   (atama     :initarg :atama    :initform nil :accessor atama)))

(defclass keystate ()
  ((up     :initarg :up    :initform nil   :accessor up)
   (down   :initarg :down  :initform nil   :accessor down)
   (right  :initarg :right :initform nil   :accessor right)
   (left   :initarg :left  :initform nil   :accessor left)
   (lshift :initarg :lshift  :initform nil   :accessor lshift)
   (spc    :initarg :spc   :initform nil   :accessor spc)
   (rkey   :initarg :rkey   :initform nil   :accessor rkey)
   (zkey   :initarg :zkey  :initform nil   :accessor zkey)))

(gamekit:register-resource-package :keyword "img/")
(define-image :wall "wall.png")
(define-image :block "block.png")
(define-image :p1 "player1.png")
(define-image :p2 "player2.png")
(define-image :p3 "player3.png")
(define-image :p4 "player4.png")
(define-image :bomb "bomb.png")
(define-image :fire "fire.png")
(define-image :fireup "fireup.png")
(define-image :bombup "bombup.png")
(define-image :kick   "kick.png")
(define-font :mplus "mplus-1mn-regular.ttf")

(defparameter *status-font* nil)
(defparameter *gamespeed-font* nil)
(defparameter *winner-font* nil)

(defun set-font ()
  (setf *status-font* (make-font :mplus 28)
	*gamespeed-font*  (make-font :mplus 32)
	*winner-font* (make-font :mplus 46)))

(gamekit:defgame mogemberman () ()
  (:viewport-width *window-w*)
  (:viewport-height *window-h*)
  (:viewport-title "モゲンバーマン"))



;;文字幅取得
(defun moge-char-width (char)
    (if (<= #x20 (char-code char) #x7e)
        1
	2))
;;string全体の文字幅
(defun string-width (string)
  (apply #'+ (map 'list #'moge-char-width string)))
;;最低n幅もったstring作成
(defun minimum-column (n string)
  (let ((pad (- n (string-width string))))
    (if (> pad 0)
	(concatenate 'string string (make-string pad :initial-element #\ ))
        string)))


(defun chomp (line)
  (let ((last-char-pos
          (position-if (lambda (c) (and (not (equal c #\Return)) (not (equal c #\Linefeed))))
                       line :from-end t)))
    (if last-char-pos
        (subseq line 0 (1+ last-char-pos))
        "")))


(define-condition handshake-error (error) ())
;;ai.txtからai起動するコマンドを読み込む
;;*ai* ストリーム？
(defun load-ai (id pos img command-line)
  (let* ((hoge (ppcre:split #\space command-line))
	 (atama nil)
	 (com (make-instance 'com :pos pos :id id :img img)))
    (setf (proc com) (sb-ext:run-program
		      (car hoge) (cdr hoge)
		      :input :stream
		      :output :stream
		      :wait nil
		      :search t))
    (setf (ai-stream com) (make-two-way-stream (sb-ext:process-output (proc com))
					       (sb-ext:process-input (proc com))))
    (handler-case
     (setf (name com) (read-line (ai-stream com)))
     (end-of-file (c)
                  (format t "~A~%" c)
                  (error 'handshake-error)))
    (when (equal (name com) "")
      (format t "AIの名前が空です。~%")
      (error 'handshake-error))
    (setf atama (char (name com) 0))
    (cond
      ((= 2 (moge-char-width atama))
       (setf (atama com) (format nil "~c" atama)))
      ((= 1 (moge-char-width atama))
       (setf (atama com) (format nil "~c" (code-char (+ 65248 (char-code atama))))))
      (t
       (setf (atama com) "主")))
    (format (ai-stream com) "~d~%" id)
    (finish-output (ai-stream com))
    (setf (players *game*) (append (players *game*) (list com)))))

;;使用するai読み込む3体以下なら操作キャラを追加
(defun get-ai-from-txt ()
  (with-open-file (in "ai.txt")
    (loop :for line = (read-line in nil)
       :for pos :in (list (list 1 11) (list 15 11) (list 1 1) (list 15 1))
       :for img :in (list :p1 :p2 :p3 :p4)
       :for id :from 0 :to 3
       :while line
       :do (print id)
	 (load-ai id pos img (chomp (format nil "~a" line)))))
  ;;aiが3人以下なら操作キャラを追加
  (let ((len (length (players *game*)))
	(p nil))
    (when (> 4 len)
      (case len
	(1 (setf p (make-instance 'player :pos (list 15 11) :id 2 :img :p2)))
	(2 (setf p (make-instance 'player :pos (list 1 1) :id 3 :img :p3)))
	(3 (setf p (make-instance 'player :pos (list 15 1) :id 4 :img :p4))))
      (setf (playing p) t
	    (name p) "あなた"
	    (control *game*) t)
      (setf (players *game*) (append (players *game*) (list p))))))







(defun create-field ()
  (loop :for y :from 0 :to *tate*
     :do (loop :for x :from 0 :to *yoko*
	    :do (cond
		  ((or (= y 0) (= x 0)
		       (= y *tate*) (= x *yoko*)
		       (and (evenp x) (evenp y)))
		   (push (list x y) (walls *game*)))
		  ((and (not (or  (and (= x 1) (= y 1))
				  (and (= x 2) (= y 1))
				  (and (= x 1) (= y 2))
				  (and (= x (1- *yoko*)) (= y 1))
				  (and (= x (- *yoko* 2)) (= y 1))
				  (and (= x (1- *yoko*)) (= y 2))
				  (and (= x 1) (= y (1- *tate*)))
				  (and (= x 2) (= y (1- *tate*)))
				  (and (= x 1) (= y (- *tate* 2)))
				  (and (= x (1- *yoko*)) (= y (1- *tate*)))
				  (and (= x (- *yoko* 2)) (= y (1- *tate*)))
				  (and (= x (1- *yoko*)) (= y (- *tate* 2)))))
			(>= (random 7) 1))
		   (push (list x y) (blocks *game*)))))))


;;データ初期化
(defun init-players-data ()
  (loop :for p :in (players *game*)
     :for pos :in (list (list 1 11) (list 15 11) (list 1 1) (list 15 1))
     :do (setf (pos p) pos
	       (power p) 2
	       (setbomblimit p) 2
	       (setbombcount p) 0
	       (kick p) nil
	       (isalive p) t
	       (actmove p) "STAY"
	       (actbomb p) "false")))

;;データ初期化
(defun init-data ()
  (setf (walls *game*) nil
	(blocks *game*) nil
	(bombs *game*) nil
	(fires *game*) nil
	(items *game*) nil
	(turn *game*) 1
	(winner *game*) nil
	(state *game*) t
	*gamespeed* 60
	*gamespeed-count* 0
	*frame* 0)
  (init-players-data)
  (create-field ))


(defun draw-background ()
  (draw-rect (vec2 0 0) *window-w* *window-h* :fill-paint *green*))


(defun draw-field ()
  (with-slots (walls blocks) *game*
    (loop :for w :in walls
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :wall))
    (loop :for w :in blocks
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :block))))


(defun draw-gameinfo ()
  (draw-text (format nil "ゲーム速度:~a (左shiftで変更)"
		     (nth *gamespeed-count* *draw-gamespeed-list*))
	     (vec2 240 550) :font *gamespeed-font* )
  (draw-text "rキーで再戦(初期化)" (vec2 240 510) :font *gamespeed-font*)
  (when (winner *game*)
    (if (eq (winner *game*) :draw)
	(draw-text "引き分け！" (vec2 240 430) :font *winner-font*)
	(draw-text (format nil "~a の勝ちです！"  (winner *game*))
		   (vec2 240 430) :font *winner-font*))))


;;�X�e�[�^�X�\��
(defun draw-com-status (p status-y)
  (let ((k (if (kick p) "あり" "なし")))
    (draw-rect (vec2 550 (- status-y 50)) 240 90 :stroke-paint *black*)
    (draw-image (vec2 550 status-y) (img p))
    (draw-text (format nil "名前:~a" (name p)) (vec2 590 (+ status-y 20)) :font *status-font*)
    (draw-text (format nil "ボム:~d" (setbomblimit p)) (vec2 590 (- status-y 0)) :font *status-font*)
    (draw-text (format nil "火力:~d" (power p)) (vec2 590 (- status-y 20)) :font *status-font*)
    (draw-text (format nil "キック:~a" k) (vec2 590 (- status-y 40)) :font *status-font*)
    ))

(defun draw-player (p status-y)
  (when (isalive p)
    (draw-com-status p status-y)
    (let* ((x (car (pos p)))
	   (y (cadr (pos p))))
      (draw-image (vec2 (* x 32) (* y 32)) (img p)))))

(defun draw-players ()
  (loop :for p :in (players *game*)
     :for status-y :from 380 :downto 0 :by 100
     :do (draw-player p status-y)))



(defun draw-bomb (b)
  (let ((x (car (pos b)))
	(y (cadr (pos b))))
    (draw-image (vec2 (* x 32) (* y 32)) :bomb)))

(defun draw-bombs ()
  (loop :for bomb :in (bombs *game*)
       :do (draw-bomb bomb)))


(defun draw-fire (pos origin)
  (draw-image (vec2 (* (car pos) 32) (* (cadr pos) 32)) :fire
	      :origin origin :width 32 :height 32))

(defun draw-fires ()
  (loop :for fire :in (fires *game*)
     :do (draw-fire (pos fire) (img fire))))


(defun draw-item (item)
  (draw-image (vec2 (* (car (pos item)) 32) (* (cadr (pos item)) 32))
	      (img item)))

(defun draw-items ()
  (loop :for item :in (items *game*)
       :do (draw-item item)))


(defun update-player (p)
  (with-slots (up down left right zkey) *keystate*
    (when (and zkey
	       (> (setbomblimit p) (setbombcount p)))
      (setf (actbomb p) "true")
      (incf (setbombcount p)))
    (cond
      (up    (setf (actmove p) "UP"))
      (down  (setf (actmove p) "DOWN"))
      (right (setf (actmove p) "RIGHT"))
      (left  (setf (actmove p) "LEFT"))
      (t (setf (actmove p) "STOP")))))


;;アイテム追加
(defun put-item (pos)
  (case (random 7)
    ((0 1 2)
     (push (make-instance 'item :pos pos :name "力" :img :fireup)
	   (items *game*)))
    ((3 4 5)
     (push (make-instance 'item :pos pos :name "弾" :img :bombup)
	   (items *game*)))
    (6
     (push (make-instance 'item :pos pos :name "蹴" :img :kick)
	   (items *game*)))))

;;ボムが爆発したらボム置いた数減らす
(defun decf-bomb-count (b)
  (let ((p (find (id b) (players *game*) :key #'id)))
    (decf (setbombcount p))))

;;;火起こし
(defun create-fire (b)
  (let ((x+flag t) (x-flag t)
        (y+flag t) (y-flag t))
    (decf-bomb-count b)
    (labels ((check-push-fire (pos)
	             (cond
            		 ((find pos (walls *game*) :test #'equal)
            		   nil)
            		 ((find pos (blocks *game*) :test #'equal)
            		  (setf (blocks *game*) (remove pos (blocks *game*) :test #'equal))
            		  (when (>= (random 10) 8)
            		    (put-item pos))
            		  nil)
            		 (t
            		  t))))
      (push (make-instance 'fire :pos (copy-list (pos b)) :img (vec2 0 64))
	          (fires *game*))
      (loop :for power :from 1 :to (power b)
	          :do (let ((x+ (list (+ (car (pos b)) power) (cadr (pos b))))
              		   (x- (list (- (car (pos b)) power) (cadr (pos b))))
              		   (y+ (list (car (pos b)) (+ (cadr (pos b)) power)))
              		   (y- (list (car (pos b)) (- (cadr (pos b)) power))))
        	       (if (and x+flag
              			     (check-push-fire x+))
              		   (push (make-instance 'fire :pos x+
              					                 :img (if (= power (power b)) (vec2 32 0) (vec2 0 0)))
		                       (fires *game*))
              		   (setf x+flag nil))
        	       (if (and x-flag
        			(check-push-fire x-))
        		   (push (make-instance 'fire :pos x-
        					:img (if (= power (power b)) (vec2 64 0) (vec2 0 0)))
        			 (fires *game*))
        		   (setf x-flag nil))
        	       (if (and y+flag
        			(check-push-fire y+))
        		   (push (make-instance 'fire :pos y+
        					:img (if (= power (power b)) (vec2 32 32) (vec2 0 32)))
        			 (fires *game*))
        		   (setf y+flag nil))
        	       (if (and y-flag
        			(check-push-fire y-))
        		   (push (make-instance 'fire :pos y-
        					:img (if (= power (power b)) (vec2 64 32) (vec2 0 32)))
        			 (fires *game*))
        		   (setf y-flag nil)))))))

;;壁とブロックとボムとの当たり判定
(defun collision-blocks-walls-boms (pos)
  (or (find pos (blocks *game*) :test #'equal)
      (find pos (walls *game*) :test #'equal)
      (find pos (bombs *game*) :key #'pos :test #'equal)))


(defun update-bomb (b)
  (decf (timer b))
  (cond
    ((= (timer b) 0)
     (create-fire b)
     (setf (bombs *game*)
	         (remove b (bombs *game*) :test #'equal)))
    ((kicked b)
     (let ((temp (copy-list (pos b))))
       (cond
      	 ((string= (kicked b) "UP")    (incf (cadr (pos b))))
      	 ((string= (kicked b) "DOWN")  (decf (cadr (pos b))))
      	 ((string= (kicked b) "RIGHT") (incf (car (pos b))))
      	 ((string= (kicked b) "LEFT")  (decf (car (pos b)))))
       (when (or (find (pos b) (blocks *game*) :test #'equal)
		             (find (pos b) (walls *game*) :test #'equal)
	               (find (pos b) (remove b (bombs *game*) :test #'equal) :key #'pos :test #'equal))
      	 (setf (pos b) temp
      	       (kicked b) nil))))))

(defun update-bombs ()
  (loop :for bomb :in (bombs *game*)
     :do (update-bomb bomb)))


;;-----------------------------------------------------------------
(defun create-player-data (p)
  (append (list :id (id p))
	  (list :name (name p))
	  (list :pos (list :x (car (pos p)) :y (cadr (pos p))))
	  (list :power (power p))
	  (list :setBombLimit (setbomblimit p))
	  (list :isAlive (isalive p))
	  (list :setBombCount (setbombcount p))))

(defun create-bomb-data (b)
  (append (list :pos (list :x (car (pos b)) :y (cadr (pos b))))
	  (list :timer (timer b))
	  (list :power (power b))))

(defun create-item-data (b)
  (append (list :pos (list :x (car (pos b)) :y (cadr (pos b))))
	  (list :name (name b))))

(defun create-send-data ()
  (append (list :turn (turn *game*))
	  (list :walls (walls *game*))
	  (list :blocks (blocks *game*))
	  (list :players (mapcar #'create-player-data (players *game*)))
	  (list :bombs (mapcar #'create-bomb-data (bombs *game*)))
	  (list :items (mapcar #'create-item-data (items *game*)))
	  (list :fires (mapcar #'pos (fires *game*)))))

(defun send-data-to-com (com data)
  ;;(format t  "QQ~%")
  (format (ai-stream com) "~a~%" (jonathan:to-json data))
  (finish-output  (ai-stream com)))


(defun get-data-from-com (com)
  (let* ((str (read-line (ai-stream com)))
	       (wake (ppcre:split "," str)))
    (setf (actmove com) (car wake)
	        (actbomb com) (cadr wake))))
  ;;(format t "RR~%"))



;;-----------------------------------------------------------------
;;comがボムおく処理
(defun update-com-put-bomb (com)
  (when (and (> (setbomblimit com) (setbombcount com))
             (string= (actbomb com) "true"))
    (push (make-instance 'bomb :timer 10 :power (power com) :id (id com)
			                         :pos (copy-list (pos com)))
          (bombs *game*))
    (incf (setbombcount com))
    (when (playing com)
      (setf (actbomb com) "false"))))

;;comの移動
(defun update-com-position (com)
  ;;(format t "TT~%")
  (let ((dir (actmove com))
	(temp (copy-list (pos com))))
    ;;(format t "YY~%")
    (cond
      ((string= dir "UP") (incf (cadr (pos com))))
      ((string= dir "DOWN") (decf (cadr (pos com))))
      ((string= dir "LEFT") (decf (car (pos com))))
      ((string= dir "RIGHT") (incf (car (pos com))))
      (t nil))
    (let ((bomb (find (pos com) (bombs *game*) :key #'pos :test #'equal)))
      (cond
	((or (find (pos com) (blocks *game*) :test #'equal)
	     (find (pos com) (walls *game*) :test #'equal))
	 (setf (pos com) temp))
	((and bomb (kick com))
	 (setf (pos com) temp
	       (kicked bomb) dir))
	(bomb
	 (setf (pos com) temp))))
     ;;操作キャラ移動方向初期化
    (when (playing com)
      (setf (actmove com) "STOP"))))

(defun update-coms ()
  (let ((data (create-send-data)))
    ;;(format t "~a~%" (jonathan:to-json data))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (if (playing com)
		 (update-player com)
		 (progn (send-data-to-com com data)
			(get-data-from-com com)))))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (update-com-put-bomb com)))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (update-com-position com)))))


(defun draw-turn ()
  (draw-text (format nil "TURN:~d" (turn *game*))
	     (vec2 0 430) :font *gamespeed-font*))

(defmethod gamekit:draw ((app mogemberman))
  (draw-background)
  (draw-gameinfo)
  (draw-field)
  (draw-items)
  (draw-fires)
  (draw-bombs)
  (draw-players)
  (draw-turn))
;;(draw-player *player*))

;;アイテムとキャラの当たり判定
(defun collision-item-com ()
  (loop :for p :in (players *game*)
     :do (let ((item (find (pos p) (items *game*) :key #'pos :test #'equal)))
	   (when item
	     (cond
	       ((eq (img item) :bombup) (incf (setbomblimit p)))
	       ((eq (img item) :fireup) (incf (power p)))
	       ((eq (img item) :kick) (setf (kick p) t)))
	     (setf (items *game*)
		   (remove item (items *game*) :test #'equal))))))


(defun collision-wall-com ()
 (loop :for p :in (players *game*)
    :do (when (find (pos p) (walls *game*) :test #'equal)
	         (setf (isalive p) nil))))

;;;火とキャラの当たり判定
(defun collision-fire-com ()
  (loop :for p :in (players *game*)
     :do (when (find (pos p) (mapcar #'pos (fires *game*)) :test #'equal)
	         (setf (isalive p) nil))))

(defun collision-fire-item ()
  (loop :for item :in (items *game*)
     :do (when (find (pos item) (mapcar #'pos (fires *game*)) :test #'equal)
	   (setf (items *game*)
		 (remove item (items *game*) :test #'equal)))))


;;fire-bombの当たり判定
(defun collision-fire-bomb ()
  (let ((hoge nil))
    (loop :for bomb :in (bombs *game*)
       :do (when (find (pos bomb) (mapcar #'pos (fires *game*)) :test #'equal)
      	     (create-fire bomb)
      	     (setf hoge t)
      	     (setf (bombs *game*)
      		         (remove bomb (bombs *game*) :test #'equal))))
    (when hoge
      (collision-fire-bomb))))


(defun delete-fires ()
  (setf (fires *game*) nil))

;;
(defun my-sleep (m)
  (let ((time1 (get-internal-real-time))
	(time2 (get-internal-real-time )))
    (loop :while (>= m (- time2 time1))
       :do (setf time2 (get-internal-real-time)))))


;;キー入力をnilにする
(defun init-keystate ()
  (with-slots (zkey left right up down spc) *keystate*
    (setf zkey nil left nil right nil up nil down nil spc nil)))


(defun add-wall ()
  (when (>= (turn *game*) 300)
    (multiple-value-bind (s a)
	(floor (turn *game*) 300)
      (when (> (length *falling-wall*) a)
	(push (nth a *falling-wall*)
	      (walls *game*))))))

;;ゲーム勝敗判定
(defun end-game? ()
  (cond
    ;;生き残りが一人
    ((= 1 (count t (players *game*) :key #'isalive))
     (let ((p (find t (players *game*) :key #'isalive)))
       (setf (winner *game*) (name p)
	     (state *game*) nil)))
    ;;1ターンで同時死亡
    ((= 0 (count t (players *game*) :key #'isalive))
     (setf (winner *game*) :draw
	   (state *game*) nil))))


(defmethod gamekit:act ((app mogemberman))
  (with-slots (zkey left right up down spc) *keystate*
    (when (or (and (state *game*)
		               (not (control *game*))
	                 (zerop (mod *frame* *gamespeed*)))
              (and (state *game*)
		               (control *game*)
	                 (or zkey left right up down spc)))
      ;;(update-player)
      (add-wall)
      (delete-fires)
      (update-coms) ;;移動
      (update-bombs) ;;ボムの更新
      (collision-fire-bomb)
      (collision-item-com)
      (collision-fire-item)
      (collision-fire-com)
      (collision-wall-com)
      (end-game?)
      (init-keystate)
      (incf (turn *game*))
      (setf *frame* 0))
    (incf *frame*)))

(defmethod gamekit:post-initialize ((app mogemberman))
  (set-font)


  (create-field)
  ;;(create-player)
  ;;(create-players)
  (bind-button :r :pressed
	       (lambda () (init-data)))
  (bind-button :z :pressed
	       (lambda () (setf (zkey *keystate*) t)))
  (bind-button :z :released
	       (lambda () (setf (zkey *keystate*) nil)))
  (bind-button :left-shift :pressed
	       (lambda ()
		 (incf *gamespeed-count*)
		 (when (> *gamespeed-count* 4)
		   (setf *gamespeed-count* 0))
		 (setf *gamespeed* (nth *gamespeed-count* *gamespeed-list*))))
  (bind-button :right :pressed
	       (lambda () (setf (right *keystate*) t)))
  (bind-button :left :pressed
	       (lambda () (setf (left *keystate*) t)))
  (bind-button :up :pressed
	       (lambda () (setf (up *keystate*) t)))
  (bind-button :down :pressed
	       (lambda () (setf (down *keystate*) t)))
  (bind-button :right :released
	       (lambda () (setf (right *keystate*) nil)))
  (bind-button :left :released
	       (lambda () (setf (left *keystate*) nil)))
  (bind-button :up :released
	       (lambda () (setf (up *keystate*) nil)))
  (bind-button :down :released
	       (lambda () (setf (down *keystate*) nil)))
  (gamekit:bind-button :escape :pressed
                       (lambda ()
                         (gamekit:stop))))

(defmethod gamekit:pre-destroy ((app mogemberman))
  ;; (setf *game* nil
  ;; 	*player* nil
  ;; 	*keystate* nil)
  )


(defun run ()
  (setf *game* (make-instance 'game)
	;;*player* (make-instance 'player :pos (list 1 11))
	*keystate* (make-instance 'keystate))
  (get-ai-from-txt)
  (gamekit:start 'mogemberman :autoscaled t :viewport-resizable t)) ;;:viewport-resizable t))
