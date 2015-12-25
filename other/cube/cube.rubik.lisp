(defclass rubik-cube ()
  ((color :accessor color :initarg :color)
   (cubes :accessor cubes :initform (make-array '(3 3 3)))
   (buff :accessor buff :initform (make-array '(3 3)))))


(defmethod draw-rubik ((this rubik-cube))
 (with-matrix
  (gl:translate -1.5 -1.5 -1.5)
  (embeded-do ((x 0 2) (y 0 2) (z 0 2))
	      (draw-trans x y z (aref (cubes this) x y z)))))


(defmethod set-rubik ((this rubik-cube))
 (embeded-do ((x 0 2) (y 0 2) (z 0 2)) (setf (aref (cubes this) x y z) (make-instance 'cube :size 0.9 :color (copy-seq (color this))))))


(defmethod rotate-coords (k (this rubik-cube))
 (case k
   ((1)
    (embeded-do ((x 0 2) (z 0 2))
     (setf (aref (buff this) x z)
	   (aref (cubes this) x 2 z)))
    
    (setf (buff this) (quarter-turn (buff this)))
    
    (embeded-do ((x 0 2) (z 0 2))
     (setf (aref (cubes this) x 2 z)
	   (aref (buff this) x z)))
    
    (embeded-do ((x 0 2) (z 0 2))
     (rotate-z (aref (cubes this) x 2 z))))

   
   ((2)
    (embeded-do ((x 0 2) (z 0 2))
     (setf (aref (buff this) x z)
	   (aref (cubes this) x 0 z)))
    
    (setf (buff this) (quarter-turn (buff this)))
    
    (embeded-do ((x 0 2) (z 0 2))
     (setf (aref (cubes this) x 0 z)
	   (aref (buff this) x z)))
    
    (embeded-do ((x 0 2) (z 0 2))
     (rotate-z (aref (cubes this) x 0 z))))

   
   ((3)
    (embeded-do ((y 0 2) (z 0 2))
     (setf (aref (buff this) y z)
	   (aref (cubes this) 0 y z)))
    
    (dotimes (i 3)
     (setf (buff this) (quarter-turn (buff this))))
    
    (embeded-do ((y 0 2) (z 0 2))
     (setf (aref (cubes this) 0 y z)
	   (aref (buff this) y z)))
    
    (embeded-do ((y 0 2) (z 0 2))
     (dotimes (i 3) (rotate-y (aref (cubes this) 0 y z)))))

   
   ((4)
    (embeded-do ((y 0 2) (z 0 2))
     (setf (aref (buff this) y z)
	   (aref (cubes this) 2 y z)))
    
    (dotimes (i 3)
     (setf (buff this) (quarter-turn (buff this))))
    
    (embeded-do ((y 0 2) (z 0 2))
     (setf (aref (cubes this) 2 y z)
	   (aref (buff this) y z)))
    
    (embeded-do ((y 0 2) (z 0 2))
     (dotimes (i 3) (rotate-y (aref (cubes this) 2 y z)))))


   ((5)
    (embeded-do ((x 0 2) (y 0 2))
     (setf (aref (buff this) x y)
	   (aref (cubes this) x y 2)))
    
    (dotimes (i 3)
     (setf (buff this) (quarter-turn (buff this))))
    
    (embeded-do ((x 0 2) (y 0 2))
     (setf (aref (cubes this) x y 2)
	   (aref (buff this) x y)))
    
    (embeded-do ((x 0 2) (y 0 2))
     (dotimes (i 3) (rotate-x (aref (cubes this) x y 2)))))


   ((6)
    (embeded-do ((x 0 2) (y 0 2))
     (setf (aref (buff this) x y)
	   (aref (cubes this) x y 0)))
    
    (dotimes (i 3)
     (setf (buff this) (quarter-turn (buff this))))
    
    (embeded-do ((x 0 2) (y 0 2))
     (setf (aref (cubes this) x y 0)
	   (aref (buff this) x y)))
    
    (embeded-do ((x 0 2) (y 0 2))
     (dotimes (i 3) (rotate-x (aref (cubes this) x y 0)))))))



(defmethod rotate-graphics (n (this rubik-cube))
 (let* ((j 360)
	(k (/ j 90)))
  (case n
    
    ((1)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)
      
      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 0 1) (z 0 2))
	(draw-trans x y z (aref (cubes this) x y z))))
      
      (rotate-all)
      
      (with-matrix
       (gl:rotate (/ i k) 0 1 0)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (z 0 2))
	(draw-trans x 2 z (aref (cubes this) x 2 z))))
      
      (sdl:update-display))
     
     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display))
    

    
    ((2)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)
      
      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 1 2) (z 0 2))
	(draw-trans x y z (aref (cubes this) x y z))))
      
      (rotate-all)

      (with-matrix
       (gl:rotate (/ i k) 0 1 0)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (z 0 2))
	(draw-trans x 0 z (aref (cubes this) x 0 z))))
      
      (sdl:update-display))
     
     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display))

    

    ((3)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)

      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 1 2) (y 0 2) (z 0 2))
	(draw-trans x y z (aref (cubes this) x y z))))

      (rotate-all)

      (with-matrix
       (gl:rotate (/ i k) 1 0 0)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((y 0 2) (z 0 2))
	(draw-trans 0 y z (aref (cubes this) 0 y z))))

      (sdl:update-display))

     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display))


    
    ((4)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)
      
      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 1) (y 0 2) (z 0 2))
	(draw-trans x y z (aref (cubes this) x y z))))

      (rotate-all)
      
      (with-matrix
       (gl:rotate (/ i k) 1 0 0)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((y 0 2) (z 0 2))
	(draw-trans 2 y z (aref (cubes this) 2 y z))))

      (sdl:update-display))

     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display))


    
    ((5)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)
      
      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 0 2) (z 0 1))
	(draw-trans x y z (aref (cubes this) x y z))))
      
      (rotate-all)
      
      (with-matrix
       (gl:rotate (/ i k) 0 0 1)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 0 2))
	(draw-trans x y 2 (aref (cubes this) x y 2))))

      (sdl:update-display))

     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display))


    
    ((6)
     (embeded-do ((i 1 j))
      (gl:clear :color-buffer :depth-buffer)
      
      (with-matrix
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 0 2) (z 1 2))
	(draw-trans x y z (aref (cubes this) x y z))))

      (rotate-all)
      
      (with-matrix
       (gl:rotate (/ i k) 0 0 1)
       (gl:translate -1.5 -1.5 -1.5)
       (embeded-do ((x 0 2) (y 0 2))
	(draw-trans x y 0 (aref (cubes this) x y 0))))
      
      (sdl:update-display))

     (rotate-coords n this)
     
     (gl:clear :color-buffer :depth-buffer)
     (draw-rubik this)
     (sdl:update-display)))))
     
     


    

       
   	
 
 

	
   
