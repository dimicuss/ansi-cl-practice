;;1.
(setf x 'a
      y 'b
      z '(c d))

;;a 
`(,z a z)

;;b
`(x ,y ,@z)

;;c
`((,@z ,x) z)




;;2. Определите if через cond.

(defmacro owr-if (a b &optional c)
  `(cond (,a ,b)
	 (t ,c)))

(owr-if t t)




;;3. Определите макрос, аргументами которого являются число n
;;   и следующие за ним произвольные выражение. Макрос должен
;;   возвращать значение n-го выражение.

(defmacro nth-expr (n &body body)
  `(case ,n
     ,@(let ((key 0))
	 (mapcar #'(lambda (exp)
		     `(,(incf key) ,exp))
		 body))))


(let ((n 2))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0))) ;;=> 3




;;4. Определите ntimes (стр. 175), раскрывающуюся в (локальную) рекурсивную 
;;   фнкцию.

(defmacro n-times (n &body body)
  (let ((gsym (gensym))
	(gname (gensym)))
    `(labels ((,gname (,gsym)
		(unless (zerop ,gsym)
		  ,@body
		  (,gname (1- ,gsym)))))
       (,gname ,n))))

(let ((x 10))
  (n-times x (setf x (+ x 1)))
  x) ;;=> 20




;;5. 

(defmacro n-of (n exp)
  (let ((fname (gensym)) (fvar (gensym)))
    `(labels ((,fname (,fvar)
		(if (zerop ,fvar)
		    nil
		    (cons ,exp (,fname (1- ,fvar))))))
       (,fname ,n))))

(let ((i 0) (n 4))
  (n-of n (incf i))) ;;=> (1 2 3 4)




;;6.

(defmacro owr-let (varlst &body body)
  `((lambda ,varlst ,@body) ,@varlst))

(setf x nil
      y nil
      z nil)

(owr-let (x y z)
	 (setf x t)
	 (setf y t)
	 (setf z t))

x ;;=> nil
y ;;=> nil
z ;;=> nil




;;7.

(defmacro opush (obj lst)
  `(setf ,lst (cons ,obj ,lst)))

(setf x #(1 2 3))
(setf y #(1 2 3))
(setf i 0)
(setf j 0)

(push 1 (aref x (incf i))) ;; => (1 . 2)

(opush 1 (aref y (incf j))) ;; => (1 . 3)




;;8.

(define-modify-macro double ()
  (lambda (var) (* var 2)))

(let ((x 4))
  (double x)
  x) ;;=> 8
