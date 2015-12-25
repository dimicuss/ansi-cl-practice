(defclass cube ()
  ((size :accessor  c-size
	 :initarg :size)
   (color :accessor c-color
	  :initarg :color)))

(defmethod rotate-x ((this cube))
 (let ((buff (aref (c-color this) 5)))
  (setf (aref (c-color this) 5) (aref (c-color this) 3))
  (setf (aref (c-color this) 3) (aref (c-color this) 4))
  (setf (aref (c-color this) 4) (aref (c-color this) 2))
  (setf (aref (c-color this) 2) buff)))

(defmethod rotate-y ((this cube))
 (let ((buff (aref (c-color this) 2)))
  (setf (aref (c-color this) 2) (aref (c-color this) 1))
  (setf (aref (c-color this) 1) (aref (c-color this) 3))
  (setf (aref (c-color this) 3) (aref (c-color this) 0))
  (setf (aref (c-color this) 0) buff)))

(defmethod rotate-z ((this cube))
 (let ((buff (aref (c-color this) 0)))
  (setf (aref (c-color this) 0) (aref (c-color this) 4))
  (setf (aref (c-color this) 4) (aref (c-color this) 1))
  (setf (aref (c-color this) 1) (aref (c-color this) 5))
  (setf (aref (c-color this) 5) buff)))

(defmethod set-color (pos color (this cube))
 (setf (aref (c-color this) pos) color))

(defmethod draw-cube (this)
 (let* ((start (/ (- 1 (c-size this)) 2))
	(end (+ start (c-size this)))
	(colors (c-color this)))
  (with-matrix
   (with-begin :quads
    ;;верх
    (apply #'gl:color (aref colors 0))
    (gl:normal 0 0 1)
    (gl:vertex end end end)          
    (gl:vertex start end end)          
    (gl:vertex start start end)          
    (gl:vertex end start end)      
       
    ;; низ
    (apply #'gl:color (aref colors 1))
    (gl:normal 0 0 -1)
    (gl:vertex end start start)
    (gl:vertex start start start)   
    (gl:vertex start end start)  
    (gl:vertex end end start)
    
    ;;спереди
    (apply #'gl:color (aref colors 2))
    (gl:normal 0 -1 0)
    (gl:vertex end start end)
    (gl:vertex start start end)   
    (gl:vertex start start start)      
    (gl:vertex end start start) 
    
    ;; сзади
    (apply #'gl:color (aref colors 3))
    (gl:normal 0 1 0)
    (gl:vertex end end start)   
    (gl:vertex start end start)      
    (gl:vertex start end end)   
    (gl:vertex end end end)
    
    ;; слева
    (apply #'gl:color (aref colors 4))
    (gl:normal -1 0 0)
    (gl:vertex start end end)   
    (gl:vertex start end start)      
    (gl:vertex start start start)         
    (gl:vertex start start end)      
    
    ;; справа
    (apply #'gl:color (aref colors 5))
    (gl:normal 1 0 0)
    (gl:vertex end end start)
    (gl:vertex end end end)
    (gl:vertex end start end)   
    (gl:vertex end start start)))))

(defmethod draw-trans(x y z (this cube))
 (with-matrix
  (gl:translate x y z)
  (draw-cube this)))
