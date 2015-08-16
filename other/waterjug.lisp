(defparameter *maxA* 8)
(defparameter *maxB* 3)
(defparameter *funcs* '(a->b b->a fillA fillB emptyA emptyB))
(defparameter *states* '((0 . 0)))

(defun pourA (state)
  (let* ((a (car state)) (b (cdr state)) (perm (- *maxA* a)))
    (if (or (= b 0) (= a *maxA*))
	state
	(if (<= b perm)
	    (cons (+ a b) 0)
	    (cons (+ a perm) (- b perm))))))

(defun pourB (state)
  (let* ((a (car state)) (b (cdr state)) (perm (- *maxB* b)))
    (if (or (= a 0) (= b *maxB*))
	state
	(if (<= a perm)
	    (cons 0 (+ a b))
	    (cons (- a perm) (+ b perm))))))

(defun fillA (state)
  (cons *maxA* (cdr state)))

(defun fillB (state)
  (cons (car state) *maxB*))

(defun emptyA (state)
  (cons 0 (cdr state)))

(defun emptyB (state)
  (cons (car state) 0))

(defun check (state n)
  (when (or (eql (car state) n) 
	    (eql (cdr state) n))
    t))

(defun solve (n)
  (dolist (fn *funcs* (unless (check (car *states*) n) (solve n)))
    (let ((state (funcall fn (car *states*))))
      (unless (member state *states* :test #'equal)
	(push state *states*))
      (when (check state n)
	(return *states*)))))

(solve 7)

















