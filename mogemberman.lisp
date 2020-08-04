;;trivial-gamekitの関数の外でai読み込みしないとだめらしい
(ql:quickload '(:trivial-gamekit :cl-bodge :jonathan :cl-ppcre))

(defpackage moge
  (:use :cl :trivial-gamekit))

(in-package moge)

(defvar *window-w* 1024)
(defvar *window-h* 728)
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

(defclass game ()
  ((walls     :initarg :walls     :initform nil :accessor walls)
   (blocks      :initarg :blocks      :initform nil :accessor blocks)
   (players  :initarg :players  :initform nil :accessor players)
   (bombs    :initarg :bombs    :initform nil :accessor bombs)
   (items    :initarg :items    :initform nil :accessor items)
   (fires    :initarg :fires    :initform nil :accessor fires)
   (turn     :initarg :turn     :initform nil :accessor turn)
   ))

(defclass player ()
  ((pos          :initarg :pos          :initform nil :accessor pos)
   (id           :initarg :id           :initform 0   :accessor id)
   (img          :initarg :img          :initform 0   :accessor img)
   (isalive      :initarg :isalive      :initform t   :accessor isalive)
   (name         :initarg :name         :initform nil :accessor name)
   (action       :initarg :action       :initform nil :accessor action)
   (power        :initarg :power        :initform 2   :accessor power)
   (actmove      :initarg :actmove      :initform 2   :accessor actmove)
   (actbomb      :initarg :actbomb      :initform 2   :accessor actbomb)
   (setbomblimit :initarg :setbomblimit :initform 2   :accessor setbomblimit)
   (setbombcount :initarg :setbombcount :initform 0   :accessor setbombcount)))

(defclass bomb ()
   ((pos     :initarg :pos    :initform nil :accessor pos)
    (timer   :initarg :timer  :initform 0   :accessor timer)
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

(defun get-ai-from-txt ()
  (with-open-file (in "ai.txt")
    (loop :for line = (read-line in nil)
       :for pos :in (list (list 1 11) (list 15 11) (list 1 1) (list 15 1))
       :for img :in (list :p1 :p2 :p3 :p4)
       :for id :from 0 :to 3
       :while line
       :do (print id)
	 (load-ai id pos img (chomp (format nil "~a" line))))))

(define-condition handshake-error (error) ())



;;ai.txtからai起動するコマンドを読み込む
;;*ai* ストリーム？
(defun load-ai (id pos img command-line)
  (let* ((hoge (ppcre:split #\space command-line))
	 (atama nil)
	 (com (make-instance 'com :pos pos :id id :img img)))
    (format t "MOGE")
    (setf (proc com) (sb-ext:run-program
		      (car hoge) (cdr hoge)
		      :input :stream
		      :output :stream
		      :wait nil
		      :search t))
    (format t "HOGE")
    (setf (ai-stream com) (make-two-way-stream (sb-ext:process-output (proc com))
					       (sb-ext:process-input (proc com))))
    (format t "NOGE")
    (handler-case
     (setf (name com) (read-line (ai-stream com)))
     (end-of-file (c)
                  (format t "~A~%" c)
                  (error 'handshake-error)))
    (format t "KOGE")
    (when (equal (name com) "")
      (format t "AIの名前が空です。~%")
      (error 'handshake-error))
    (format t "GOGE")
    (setf atama (char (name com) 0))
    (cond
      ((= 2 (moge-char-width atama))
       (setf (atama com) (format nil "~c" atama)))
      ((= 1 (moge-char-width atama))
       (setf (atama com) (format nil "~c" (code-char (+ 65248 (char-code atama))))))
      (t
       (setf (atama com) "主")))
    (format t "LOGE")
    (format (ai-stream com) "~d~%" id)
    (finish-output (ai-stream com))
    (format t "POGE")
    (push com (players *game*))
    (format t "HOGE2~%")))


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




(defun create-players ()
  (push
   (make-instance 'player :pos '(1 1) :name "も")
   (players *game*)))

(defun draw-background ()
  (draw-rect (vec2 0 0) *window-w* *window-h* :fill-paint *green*))


(defun draw-field ()
  (with-slots (walls blocks) *game*
    (loop :for w :in walls
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :wall))
    (loop :for w :in blocks
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :block))))


(defun draw-player (p)
  (when (isalive p)
    (let* ((x (car (pos p)))
	   (y (cadr (pos p))))
      (draw-image (vec2 (* x 32) (* y 32)) (img p)))))

(defun draw-players ()
  (loop :for p :in (players *game*)
     :do (draw-player p)))

(defun draw-test ()
  (with-slots (up down left right) *keystate*
    (cond
      (up    (draw-image (vec2 100 100) :fire :origin (vec2 0 64) :width 32 :height 32  ))
      (down  (draw-image (vec2 200 100) :wall))
      (right (draw-image (vec2 100 200) :wall))
      (left  (draw-image (vec2 200 200) :wall)))))


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


(defun update-player ()
  (with-slots (up down left right zkey) *keystate*
    (let ((hoge (copy-list (pos *player*))))
      (when (and zkey
		 (> (setbomblimit *player*) (setbombcount *player*)))
	(push 
	 (make-instance 'bomb :timer 10 :power (power *player*)
			:pos hoge)
	 (bombs *game*)))
      (cond
	(up    (incf (cadr (pos *player*))))
	(down  (decf (cadr (pos *player*))))
	(right (incf (car (pos  *player*))))
	(left  (decf (car (pos  *player*)))))
      (when (or (find (pos *player*) (blocks *game*) :test #'equal)
		(find (pos *player*) (walls *game*) :test #'equal)
		(find (pos *player*) (bombs *game*) :key #'pos :test #'equal))
	(setf (pos *player*) hoge))
      )))


;;アイテム追加
(defun put-item (pos)
  (case (random 5)
    ((0 1)
     (push (make-instance 'item :pos pos :name "力" :img :fireup)
	   (items *game*)))
    ((2 3)
     (push (make-instance 'item :pos pos :name "弾" :img :bombup)
	   (items *game*)))
    (4
     (push (make-instance 'item :pos pos :name "蹴" :img :bombup)
	   (items *game*)))))
		

;;火起こし
(defun create-fire (b)
  (let ((x+flag t) (x-flag t)
	(y+flag t) (y-flag t))
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

(defun update-bomb (b)
  (decf (timer b))
  (when (= (timer b) 0)
    (create-fire b)
    (setf (bombs *game*)
	  (remove b (bombs *game*) :test #'equal))))

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
  (when (string= (actbomb com) "true")
    (push (make-instance 'bomb :timer 10 :power (power com)
			 :pos (copy-list (pos com)))
	  (bombs *game*))))

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
      ((string= dir "RIGHT") (incf (car (pos com)))))
    (when (or (find (pos com) (blocks *game*) :test #'equal)
	      (find (pos com) (walls *game*) :test #'equal)
	      (find (pos com) (bombs *game*) :key #'pos :test #'equal))
	(setf (pos com) temp))))
  
(defun update-coms ()
  (let ((data (create-send-data)))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (send-data-to-com com data)
	     (get-data-from-com com)))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (update-com-put-bomb com)))
    (loop :for com :in (players *game*)
       :do (when (isalive com)
	     (update-com-position com)))))

(defmethod gamekit:draw ((app mogemberman))
  (draw-background)
  (draw-field)
  (draw-bombs)
  (draw-fires)
  (draw-items)
  (draw-test)
  (draw-players))
;;(draw-player *player*))


(defun collision-fire-com ()
  (loop :for p :in (players *game*)
     :do (when (find (pos p) (mapcar #'pos (fires *game*)) :test #'equal)
	   (setf (isalive p) nil))))

;;fire-bombの当たり判定
(defun collision-fire-bomb ()
  (loop :for bomb :in (bombs *game*)
     :do (when (find (pos bomb) (mapcar #'pos (fires *game*)) :test #'equal)
	   (create-fire bomb)
	   (setf (bombs *game*)
		 (remove bomb (bombs *game*) :test #'equal)))))


(defun delete-fires ()
  (setf (fires *game*) nil))

(defmethod gamekit:act ((app mogemberman))
  ;;(update-player)
  (delete-fires)
  (update-coms) ;;移動
  (update-bombs) ;;ボム更新&火
  ;;(collision-fire-com)
  (collision-fire-bomb)
  (sleep 1))

(defmethod gamekit:post-initialize ((app mogemberman))
  
  
  
  (create-field)
  ;;(create-player)
  ;;(create-players)
  (bind-button :z :pressed
	       (lambda () (setf (zkey *keystate*) t)))
  (bind-button :z :released 
	       (lambda () (setf (zkey *keystate*) nil)))
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
  (gamekit:start 'mogemberman)) ;;:viewport-resizable t))
