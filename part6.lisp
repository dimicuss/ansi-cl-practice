;;1. Определите tokens (стр 81) с :test :star, по умолчанию 
;;   равные #'constitunent и 0 соответственно.

(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c) (not (funcall test c)))
			       str 
			       :start p1)))
	  (cons (subseq str p1 p2)
		(if p2 
		    (tokens str :test test :start p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\ ))))

(tokens "ab12 3cde.f gh") ;=> ("ab12" "3cde.f "gh")




;;2. Определите верссию bin-search (стр 75), использующую ключи
;;   :key, :test, :start, :end.

(defun bin-search (obj vec &key (key #'aref) 
			        (test #'eql) 
			        (start 0) 
			        (end (- (length vec) 1)))
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec key test start end))))

(defun finder (obj vec key test start end)
  (let* ((range (- end start))
	 (mid (+ start (round (/ range 2))))
	 (obj2 (funcall key vec mid)))
    (if (zerop range)
        (if (funcall test obj (funcall key vec start))
            obj
            nil)
	(if (< obj obj2)
	    (finder obj vec key test start (- mid 1))
	    (if (> obj obj2)
		(finder obj vec key test (+ mid 1) end)
		obj)))))

(bin-search 7 #(1 2 3 4 5 6 7)) ;;=> 7



;;3. Определите функцию, принимающую любое количество аргументов,
;;   и возвращающую их количество.

(defun owr-count (&rest rest)
  (length rest))

(owr-count 1 2 3 4 5) ;;=> 5




;;4. Определите функцию most (стр 118), так чтобы она 
;;   возвращала 2 значения - два значения, имеющие наибольший вес.

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setf wins obj
                    max score))))
	(values wins 
		(most fn (remove wins lst))))))

(most #'length '((a b c) (a b c d) (h s d w 2 23 s) (e f g h s))) ;;=> (H S D W 2 23 S), (E F G H S)




;;5. Определите remove-if с помощью filter (страница 117) 

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun owr-remove-if (fn lst)
  (filter #'(lambda (obj)
	      (and (funcall fn obj) 
		   obj))
	  lst))

(owr-remove-if #'oddp '(1 2 3 4 5 6 7)) ;;=> (1 3 5 7)




;;6. Определите функцию, принимающую одно число и возвращающую 
;;   наибольшее число из всех ранее полученных ею.

(let (data)
  (defun closure-most (n)
    (car (sort (push n data) #'>)))) ;; запускать в repl через load




;;7. Определите функцию, принимающую одно число и возвращающую его же,
;;   елсли оно больше числа, переданного этой функции н апредыдущем вызове.

(let (data)
  (defun more-than-prev (n)
    (unless data
      (push n data)
      (return-from more-than-prev))
    (cond 
      ((> n (car data)) (car (push n data)))
      (t (push n data)
	 (return-from more-than-prev))))) ;; запукать в repl через load




;;8. Определите функции expencive и frugal, так чтобы expencive и frugal
;;   возвращали один и тот же результат при одинаковых аргументах, и так чтобы
;;   функция frugal вызывала exencive если ей передается аргумент не 
;;   передававщийся ранее.

(defun expensive (n)
  (and (>= n 0) (<= n 100)
       (sqrt n)))

(let (data)
  (defun frugal (n)
    (if (and (member n data)
	     (>= n 0) (<= n 100))
	(sqrt (car (push n data)))
	(expencive (car (push n data)))))) ;; запускать в repl через load



;;9. Определите функцию на подобие apply, но где все числа, которые можгут быть
;;   напечатаны при ее выполнении, выводятся в восьмеричном формате (base 8).


(defun owr-apply (fn lst)
  (let ((*print-base* 8))
    (print (reduce fn lst))))


(owr-apply #'* '(1 2 3 4 5)) ;;=> 120, в repl => 170
    




