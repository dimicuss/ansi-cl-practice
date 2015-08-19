(defparameter *maxA* 8)
(defparameter *maxB* 3)
(defparameter *funcs* '(pour-a pour-b fill-a fill-b empty-a empty-b))
(defparameter *states* '((0 . 0)))

(defun action (fn state)
  (case fn
    (pour-a (let* ((a (car state)) (b (cdr state)) (perm (- *maxA* a)))
	      (cond ((or (= b 0) (= a *maxA*)) state)
		    ((<= b perm) (cons (+ a b) 0))
		    (t  (cons (+ a perm) (- b perm))))))

    (pour-b (let* ((a (car state)) (b (cdr state)) (perm (- *maxB* b)))
	      (cond ((or (= a 0) (= b *maxB*)) state)
		    ((<= a perm) (cons 0 (+ a b)))
		    (t (cons (- a perm) (+ b perm))))))

    (empty-a  (cons 0 (cdr state)))
    (empty-b (cons (car state) 0))
    (fill-a (cons *maxA* (cdr state)))
    (fill-b (cons (car state) *maxB*))))

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




(defun add-node (start-state) ;; добавить узел
  (cons start-state
	(remove start-state
		(mapcar #'(lambda (fn)
			    (action fn start-state))
			*funcs*) :test #'equal)))

(defun add-nodes (node) ;; добавить новые узлы на основе предыдущего
  (mapcar #'(lambda (state)
	      (add-node state))
	  (cdr node)))

		      

