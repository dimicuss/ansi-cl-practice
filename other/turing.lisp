(defstruct (dl (:print-function print-dl))
  prev data next)

(defun print-dl (dl stream depth)
  (declare (ingnore depth))
  (format stream "#(DL ~A)" (dl->list dl)))

(defun dl->list (lst)
  (if (dl-p lst)
      (cons (dl-data lst) (dl->list (dl-next lst)))
      lst))

(defun dl-insert (x lst)
  (let ((elt (make-dl :data x :next lst)))
    (when (dl-p lst)
      (if (dl-prev lst)
	  (setf (dl-next (dl-prev lst)) elt
		(dl-prev elt) (dl-prev lst)))
      (setf (dl-prev lst) elt))
    elt))

(defun dl-list (&rest args)
  (reduce #'dl-insert args
	  :from-end t :initial-value nil))


(defmacro left ()
  (if (dl-prev lst)
      `(setf lst (dl-prev ,lst))))

(defmacro right ()
  (if (dl-next lst)
       `(setf lst (dl-next ,lst))))

(defmacro curreplace (x lst)
  `(setf (dl-data ,lst) ,x))

(setf lst (dl-list 0 1 1 0))

(defmacro operate (&rest args)
  `((lambda ()
      (defun q0 () (print-dl lst))
	,@(mapcar #'(lambda (i)
		    `(defun ,(car i) ()
		       (case (dl-data lst)
			 ,@(mapcar #'(lambda (j)
				       `(,(car j) ,@(cdr j)))
			    (cdr i)))))
		  args)
	(q1))))

(operate 
 (q1 (0 . ((right) (q1)))
     (1 . ((right) (q1)))
     (nil . ((left) (q2))))
 
 (q2 (0 . ((curreplace 1 lst) (left) (q3)))
     (1 . ((left) (q2)))
     (nil . ((q0))))
 
 (q3 (0 . ((left) (q3)))
     (1 . ((left) (q3)))
     (nil . (q0))))

	      
	     
		
	     
				      

