(defun evc (m n)
  (if (< m n) 
      (evc n m)
      (let ((p (mod m n)))
	(if (> p 0)
	    (evc n p)
	    n))))
	

(evc 18 18)


	
      
