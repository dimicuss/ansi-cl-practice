(defmacro with-matrix (&body body)
 `(progn
   (gl:push-matrix)
   ,@body
   (gl:pop-matrix)))

(defmacro with-begin (type &body body)
 `(progn
   (gl:begin ,type)
   ,@body
   (gl:end)))

(defmacro embeded-do (vars &body body)
 (if vars
     (let ((a (first (car vars)))
	   (b (second (car vars)))
	   (c (third (car vars))))
      `(do ((,a ,b (if (> ,b ,c)
		       (- ,a 1)
		       (+ ,a 1))))
	   ((if (< ,b ,c)
		(> ,a ,c)
		(< ,a ,c)) t)
	 (embeded-do ,(cdr vars) ,@body)))
     `(progn ,@body)))
 


(defun rotate-all ()
 (gl:rotate 0.007 1 0 0)
 (gl:rotate 0.004 0 1 0)
 (gl:rotate 0.002 0 0 1))



(defun quarter-turn (arr)
 (let* ((n (car (array-dimensions arr)))
	(new-arr (make-array (list n n))))
   (dotimes (i n)
    (dotimes (j n)
     (setf (aref new-arr j (- n i 1))
	   (aref arr i j))))
  new-arr))

