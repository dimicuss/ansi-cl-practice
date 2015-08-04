;;1. Заишите следующие выражения без использования let или let*,
;;   а также без вычисления одного и того же выражения дважды:

;;   (let ((x (car y)))
;;       (cons x x))

;;   (let* ((w (car x))
;;          (y (+ w z)))
;;       (cons w y))

(defun let-a (y)
	      ((lambda (x)
		   (cons x x))
	       (car y)))

(let-a '(a b)) ;;=> (A . A)


(defun let-b (x y z)
    ((lambda (w)
	 ((lambda (y)
	      (cons w y))(+ w z))) (car x)))

(let-b '(1 0) 0 3) ;;=> (1 . 4)
