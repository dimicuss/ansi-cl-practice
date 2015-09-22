(defun one-level (lst)
  (if lst
      (if (atom (car lst))
	  (cons (car lst) (one-level (cdr lst)))
	  (append (one-level (car lst))
		  (one-level (cdr lst))))))

(one-level '(1 2 3 4 (2 2 3) (123 (23 (23)))))
