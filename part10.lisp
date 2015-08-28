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
  (let ((gsym (gensym)))
    `(let ((,gsym ,n))
       

(let ((n 1))
  (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
