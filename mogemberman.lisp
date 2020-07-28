(ql:quickload '(:trivial-gamekit :cl-bodge))

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
  ((wall     :initarg :wall     :initform nil :accessor wall)
   (blk      :initarg :blk      :initform nil :accessor blk)
   (players  :initarg :players  :initform nil :accessor players)
   ))

(defclass player ()
  ((pos          :initarg :pos          :initform nil :accessor pos)
   (id           :initarg :id           :initform 0   :accessor id)
   (isalive      :initarg :isalive      :initform t   :accessor isalive)
   (name         :initarg :name         :initform nil :accessor name)
   (power        :initarg :power        :initform 2   :accessor power)
   (setbomblimit :initarg :setbomblimit :initform 2   :accessor setbomblimit)
   (setbombcount :initarg :setbombcount :initform 0   :accessor setbombcount)))

(defclass keystate ()
  ((up     :initarg :up    :initform nil   :accessor up)
   (down   :initarg :down  :initform nil   :accessor down)
   (right  :initarg :right :initform nil   :accessor right)
   (left   :initarg :left  :initform nil   :accessor left)))

(gamekit:register-resource-package :keyword "img/")
(define-image :wall "wall.png")
(define-image :block "block.png")
(define-image :p1 "player1.png")

(gamekit:defgame mogemberman () ()
  (:viewport-width *window-w*)
  (:viewport-height *window-h*)
  (:viewport-title "ƒ‚ƒQƒ“ƒo[ƒ}ƒ“"))


(defun create-field ()
  (loop :for y :from 0 :to *tate*
     :do (loop :for x :from 0 :to *yoko*
	    :do (cond
		  ((or (= y 0) (= x 0)
		       (= y *tate*) (= x *yoko*)
		       (and (evenp x) (evenp y)))
		   (push (list x y) (wall *game*)))
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
		   (push (list x y) (blk *game*)))))))

(defun create-player ()
  (setf (p *game*)
	(make-instance 'player :pos '(1 11) :name "‚à")
	))


(defun create-players ()
  (push
   (make-instance 'player :pos '(1 1) :name "‚à")
   (players *game*)))

(defun draw-background ()
  (draw-rect (vec2 0 0) *window-w* *window-h* :fill-paint *green*))


(defun draw-field ()
  (with-slots (wall blk) *game*
    (loop :for w :in wall
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :wall))
    (loop :for w :in blk
       :do (draw-image (vec2 (* (car w) 32) (* (cadr w) 32)) :block))))


(defun draw-player (p)
  (let* ((x (car (pos p)))
	 (y (cadr (pos p))))
    (draw-image (vec2 (* x 32) (* y 32)) :p1)))

(defun draw-players ()
  (loop :for p :in (players *game*)
     :do (draw-player p)))

(defun draw-test ()
  (with-slots (up down left right) *keystate*
    (cond
      (up    (draw-image (vec2 100 100) :wall))
      (down  (draw-image (vec2 200 100) :wall))
      (right (draw-image (vec2 100 200) :wall))
      (left  (draw-image (vec2 200 200) :wall)))))


(defun update-player ()
  (with-slots (up down left right) *keystate*
      (let ((hoge (pos *player*)))
	(cond
	  (up    (incf (cadr (pos *player*))))
	  (down  (decf (cadr (pos *player*))))
	  (right (incf (car (pos  *player*))))
	  (left  (decf (car (pos  *player*)))))
	(when (or (find (pos *player*) (blk *game*) :test #'equal)
		  (find (pos *player*) (wall *game*) :test #'equal))
	  (setf (pos *player*) '(1 1))))))

(defmethod gamekit:draw ((app mogemberman))
  (draw-background)
  (draw-field)
  (draw-test)
  (draw-player *player*))

(defmethod gamekit:act ((app mogemberman))
  (update-player)
  (sleep 0.1))

(defmethod gamekit:post-initialize ((app mogemberman))
  
  (create-field)
  ;;(create-player)
  ;;(create-players)
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
	*player* (make-instance 'player :pos '(1 11))
	*keystate* (make-instance 'keystate))
  (gamekit:start 'mogemberman)) ;;:viewport-resizable t))
