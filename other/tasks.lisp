(defun order (lst)
  (if lst
      (if (member (first lst) (rest lst))
	  (cons (first lst) (ordering (remove (first lst) lst :from-end t :count 1)))
	  (cons (first lst) (ordering (rest lst))))))

(order '(1 2 1 3 2 5 4 7 4 1)) ;; => (1 1 1 2 2 3 5 4 4 7) 



(defun compress (lst)
 (labels ((to-count (lst &optional (n 1))
	   (if (member (first lst) (rest lst))
	       (to-count (remove (first lst) lst :from-end t :count 1) (1+ n))
	       n))
	  
	  (compr (lst)
	   (if lst
	       (cons (list (counter lst) (first lst))
		     (compr (remove (first lst) lst))))))
  (compr lst)))

(print (compress '(3 2 1 1 2 3 6 6 2 1))) ;; => ((2 3) (3 2) (3 1) (2 6))





      

