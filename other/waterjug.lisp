(defparameter *maxA* 8)
(defparameter *maxB* 3)
(defparameter *funcs* '(pourA pourB fillA fillB emptyA emptyB))
(defparameter *states* '((0 . 0)))

(defun action (fn state)
  (case fn
    (pourA (let* ((a (car state)) (b (cdr state)) (perm (- *maxA* a)))
	      (cond ((or (= b 0) (= a *maxA*)) state)
		    ((<= b perm) (cons (+ a b) 0))
		    (t  (cons (+ a perm) (- b perm))))))

    (pourB (let* ((a (car state)) (b (cdr state)) (perm (- *maxB* b)))
	      (cond ((or (= a 0) (= b *maxB*)) state)
		    ((<= a perm) (cons 0 (+ a b)))
		    (t (cons (- a perm) (+ b perm))))))

    (emptyA  (cons 0 (cdr state)))
    (emptyB (cons (car state) 0))
    (fillA (cons *maxA* (cdr state)))
    (fillB (cons (car state) *maxB*))))

(defun check (state n)
  (or (eql (car state) n) 
      (eql (cdr state) n)))

(defun solve (n)
  (dolist (fn *funcs* (unless (check (car *states*) n) (solve n)))
    (let ((state (action fn (car *states*))))
      (unless (member state *states* :test #'equal)
	(push state *states*))
      (when (check state n)
	(return *states*)))))

(solve 7)

