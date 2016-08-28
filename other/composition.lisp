(defun in ( var list )
  (if (listp list)
      (member var list)))



(defun variablep ( s )
  (and (atom s) (not (constantp s))))


(defun constant-p ( s )  
  (cond ((not s) t)
	
	((and (atom s) (constantp s)) t)
	
	((listp s)
	 (let ((elm (constant-p (car s)))
	       (rest (constant-p (cdr s))))
	   (if (not (and elm rest)) nil t)))
	       
	   
	(t (return-from constant-p nil))))


(defun apply-subst ( subst sequence )
  (cond ((not sequence) nil)
   
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
  (cond ((or (and (null e1)       (null e2))
	     (and (constant-p e1) (constant-p e2)))
	 (if (not (equal e1 e2)) 'fail))

	
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
	       (let* ((te1   (apply-subst subs1 (cdr e1)))
		      (te2   (apply-subst subs1 (cdr e2)))
		      (subs2 (unify te1 te2)))
		 
	   
		 (if (eql subs2 'fail) 'fail
		   (composition subs1 subs2))))))))






(defconstant ancestor 'ancestor)
(defconstant father   'father)
(defconstant david    'david)
(defconstant george   'george)




(print (unify '(ancestor x x) '(ancestor david george)))



		  
	       
	
	 
	 
  
		  
	 
