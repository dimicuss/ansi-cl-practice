(defparameter *maxA* 8)
(defparameter *maxB* 3)
(defparameter *funcs* '(a->b b->a fullA fullB emptyA emptyB))
(defparameter *states* '((0 . 0)))

(defun a->b (state)
  (let* ((fulnsA (car state))
	 (fulnsB (cdr state))
	 (psblB (- *maxB* fulnsB))
	 (nfulnsB nil))
    (if (> (+ fulnsA fulnsB) *maxB*)
	(setf nfulnsB *maxB*)
	(setf nfulnsB (+ fulnsA fulnsB)))    
    (setf fulnsA (- fulnsA (- nfulnsB fulnsB)))
    (cons fulnsA nfulnsB)))

(defun b->a (state)
  (let* ((fulnsB (cdr state))
	 (fulnsA (car state))
	 (psblA (- *maxA* fulnsA))
	 (nfulnsA nil))
    (if (> (+ fulnsA fulnsB) *maxA*)
	(setf nfulnsA *maxA*)
	(setf nfulnsA (+ fulnsA fulnsB)))    
    (setf fulnsB (- fulnsB (- nfulnsA fulnsA)))
    (cons nfulnsA fulnsB)))

(defun fullA (state)
  (cons *maxA* (cdr state)))

(defun fullB (state)
  (cons (car state) *maxB*))

(defun emptyA (state)
  (cons 0 (cdr state)))

(defun emptyB (state)
  (cons (car state) 0))

(defun check (state n)
  (when (or (eql (car state) n) 
	    (eql (cdr state) n))
    t))

(defun solve ()
  (dolist (fn *funcs* (unless (check (car *states*) 7) (solve)))
    (let ((state (funcall fn (car *states*))))
      (unless (member state *states* :test #'equal)
	(push state *states*))
      (when (check state 7)
	(return *states*)))))

(solve)













