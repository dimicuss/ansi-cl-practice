(defun in ( var list )
  (if list
      (find var list)))




(defun apply-subst ( subst sequence )
  (cond ((not sequence) (return-from apply-subst) )
   
	((atom sequence)
	 (if (eq (cdr subst) sequence)
	     (car subst)
	   sequence))
	
	((listp sequence)
	 (cons
	  (apply-subst subst (car sequence))
	  (apply-subst subst (cdr sequence))))))

	 

		  
	 
