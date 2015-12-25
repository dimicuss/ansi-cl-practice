(require 'cl-opengl)
(require 'cl-glu)
(require 'lispbuilder-sdl)

(load "cube.lib.lisp")
(load "cube.cube.lisp")
(load "cube.rubik.lisp")



(let* ((size 800)
       (colors #1a((209/255 17/255 65/255)
		   (0 177/255 89/255)
		   (0 174/255 219/255)
		   (243/255 119/255 53/255)
		   (1 196/255 37/255)
		   (196/255 37/255 1)))
       (rubik (make-instance 'rubik-cube :color colors)))
 
 (sdl:with-init ()
  (sdl:window size size :opengl t
			:opengl-attributes '((:sdl-gl-doublebuffer 1)
					     (:sdl-gl-depth-size 16)))
  (gl:viewport 0 0 size size)
  (setf (sdl:frame-rate) 60)
  
  
  (gl:matrix-mode :projection)
  (gl:load-identity)
  
  (gl:frustum -1 1 -1 1 1 10)
  (glu:look-at 3 3 3
	       0 0 0
	       0 1 0)

  
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  
  (gl:light :light0 :position '(5 5 5 0))
  (gl:enable :color-material :depth-test :lighting :light0 :multisample)
 
  (gl:clear :depth-buffer :color-buffer)
  (set-rubik rubik)
  (draw-rubik rubik)
  (sdl:update-display)


  (sdl:with-events (:poll)
   (:quit-event () t)
   (:video-expose-event () (sdl:update-display))
   (:idle
    (gl:clear :color-buffer :depth-buffer)
    (draw-rubik rubik)
    (sdl:update-display)
    (rotate-graphics (+ 1 (random 7)) rubik)))))



  
  
  
  

      
