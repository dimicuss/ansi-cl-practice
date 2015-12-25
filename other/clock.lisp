(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)




(defmacro with-begin (type &body body)
 `(progn (gl:begin ,type)
 	 ,@body
	 (gl:end)))




(defun get-time ()
 (let ((time (make-hash-table)))
  (multiple-value-bind  (sec min hur) (get-decoded-time)
   (setf (gethash 'hur time) hur
	 (gethash 'min time) min
	 (gethash 'sec time) sec))
  time))




(defun draw-circle (start end r)
 (let ((x (* r (cos (* start (/ pi 180)))))
       (y (* r (sin (* start (/ pi 180))))))
  
  (cond ((eq (mod start 90) 0) (gl:point-size 9))
	((eq (mod start 5) 0) (gl:point-size 6)))
  
  (when (<= start end)
   (with-begin :points
    (gl:vertex x y))
   (gl:point-size 2)
   (draw-circle (+ start 6) end r))))




(defun draw-line (angle r)
 (let ((x (* r (cos (* angle (/ pi 180)))))
       (y (* r (sin (* angle (/ pi 180))))))
  (with-begin :lines
   (gl:vertex 0 0)
   (gl:vertex x y))))


(defun draw-arrows (s np sec min hur)
 (when (> s 0)
  (do ((i (- sec s) (+ i (/ (* s 2) np))))
      ((>= i (+ sec s)))
   (gl:clear :color-buffer)
   (draw-circle 0 360 0.5)
   (gl:line-width 2)
   (draw-line i .5)
   (gl:line-width 4)
   (draw-line min .25)
   (gl:line-width 8)
   (draw-line hur .125)
   (sdl:update-display))
  
  (do ((i (+ sec s) (- i (/ (* s 2) np))))
      ((<= i (+ sec (- s) 0.4))) 
   (gl:clear :color-buffer)
   (draw-circle 0 360 0.5)
   (gl:line-width 2)
   (draw-line i .5)
   (gl:line-width 4)
   (draw-line min .25)
   (gl:line-width 8)
   (draw-line hur .125)
   (sdl:update-display))

  (draw-arrows (- s 0.4) (* np 2) sec min hur))
 
 (gl:clear :color-buffer)
 (draw-circle 0 360 0.5)
 (gl:line-width 2)
 (draw-line sec .5)
 (gl:line-width 4)
 (draw-line min .25)
 (gl:line-width 8)
 (draw-line hur .125)
 (sdl:update-display))
	 

		   
   
 (defun start ()
  (let* ((size 800)
	 (time (get-time))
	 (sec (- 90 (* (gethash 'sec time) 6)))
	 (min (- (- 90 (* (gethash 'min time) 6)) (/ (gethash 'sec time) 10)))
	 (hur (- (- 90 (* (gethash 'hur time) 30)) (/ (gethash 'min time) 2))))
   
   (sdl:with-init ()
    (sdl:window size size :opengl t)
    (gl:viewport 0 0 size size)
    (gl:color 0.2 0.5 0.1)
    (gl:clear-color 0.123 0.104 0.238 0)
    (gl:clear :color-buffer)
    (setf (sdl:frame-rate) 1)
    
    (draw-circle 0 360 0.5)
    
    (draw-arrows 1.2 20 0 0 0)

   
    (sdl:with-events ()
     (:quit-event () t)
     (:idle (gl:clear :color-buffer)
	    (draw-circle 0 360 0.5)

	    (draw-arrows 1.2 20
	     (setf sec (- sec 6))
	     (setf min (- min (/ 6 60)))
	     (setf hur (- hur (/ 6 3600))))
	    (sleep 0.8))))))

(start)
