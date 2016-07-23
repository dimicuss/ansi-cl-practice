(defun in ( var list )
  (if (listp list)
      (member var list)))



(defun variablep ( s )
  (and (atom s) (not (constantp s))))


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



(defun composition ( a b )
  (cond ((null a) b)
	((null b) a)
	(t (list a b))))
	

    

(defun unify ( e1 e2 )
  (cond ((or (and (null e1)      (null e2))
	     (and (constantp e1) (constantp e2)))
	 (if (not (eql e1 e2)) 'fail))

	
	((variablep e1)
	 (if (in e1 e2)
	     'fail
	   (cons e2 e1)))
	   
	
	((variablep e2)
	 (if (in e2 e1)
	     'fail
	   (cons e1 e2)))

	
	((or (null e1) (null e2)) 'fail)

	
	(t (let* ((he1 (car e1))
		  (he2 (car e2))
		  (subs1 (unify he1 he2)))
	     
	     (if (eql subs1 'fail) 'fail
	       (let* ((te1 (apply-subst subs1 (cdr e1)))
		      (te2 (apply-subst subs1 (cdr e2))))
		 (print subs1)
		 (print te1)
		 (print te2)
		 (setf subs2 (unify te1 te2))

	   
		 (if (eql subs2 'fail) 'fail
		   (composition subs1 subs2))))))))






(defconstant bill    'bill)
(defconstant parents 'parents)
(defconstant father  'father)
(defconstant mother  'mother)

(print (unify '(parents x (father x) (mother bill)) '(parents bill (father bill) y)))

		  
	       
	
	 
	 
  
		  
	 
