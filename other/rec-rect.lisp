(load "quicklisp/setup.lisp")
(ql:quickload 'cl-opengl)
(ql:quickload 'cl-glu)
(ql:quickload 'lispbuilder-sdl)

(defun draw-rects (x y a b n) 
  (let ((crAx (- x (/ a 2)))
        (crAy (- y b))
        (crBx (- x (/ a 2)))
        (crBy (- y (/ b 2)))
        (crCx (+ x a))
        (crCy (+ y (/ b 2)))
        (crDx (+ x a))
        (crDy (+ y b)))
    (draw-rectangle-* crAx crAy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crBx crBy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crCx crCy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crDx crDy (ceiling a 2) (ceiling b 2))
    
    (draw-rects crAx crAy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crBx crBy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crCx crCy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crDx crDy (ceiling a 2) (ceiling b 2) (- n 1))))



(sdl:with-init ()
  (sdl:window 1000 1000 :title-caption "Serpynsky" :icon-caption "Serpynsky")
  (setf (sdl:frame-rate) 5)
  (sdl:clear-display (sdl:color :r 0 :g 0 :b 0))
  
  (sdl:with-surface (surf sdl:*default-display*)
    (sdl:with-color (col (sdl:color :r 255 :g 100 :b 70))
        (draw-rectangle-* 0 900 900 900)
        (draw-cects 0 900 900 900 5)
  (sdl:update-display)
  (sdl:with-events ()
    (:quit-event () t)
    (:key-down-event (:key key)
		     (if (sdl:key= key :SDL-KEY-ESCAPE)
			 (sdl:push-quit-event)))
    (:video-expose-event () (sdl:update-display))))


    
    
