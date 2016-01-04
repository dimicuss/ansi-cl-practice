(defun ordering (lst)
  (if lst
      (if (member (first lst) (rest lst))
	  (cons (first lst) (ordering (remove (first lst) lst :from-end t :count 1)))
	  (cons (first lst) (ordering (rest lst))))))

(print (ordering '(1 2 1 3 2 5 4 7 4 1)))

