(defun expected-value (list)
 (reduce #'(lambda (fst scd)
	    (+ fst
	       (* (car scd)
		  (cdr scd))))
	 list :initial-value 0))
 

(defun dispersion (list)
 (- (reduce #'(lambda (fst scd)
	       (+ fst
		  (* (expt (car scd) 2)
		     (cdr scd))))
	    list :initial-value 0)
    (expt (expected-value list) 2)))


(defun standart-deviation (list)
 (sqrt (dispersion list)))


(dispersion '((-2 . 0.3) (4 . 0.5) (10 . 0.2)))
	    
