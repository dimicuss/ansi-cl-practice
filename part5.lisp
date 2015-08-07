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




;;2. Определите функцию mystery с использованием cond.

(defun mystery (obj lst)
    (cond ((null lst) nil)
	    ((eql obj (car lst)) 0)
	      (t (let ((z (mystery obj (cdr lst))))
		     (and z (1+ z))))))
    

(mystery 'a '(b b b b a)) ;;=> 4




;;3. Определите функцию, возвращающую квадрат своего аргумента,
;;   лишь когда аргумент - положительное число, меньшее или равное пяти.

(defun square (n)
    (when (and (< 0 n 6))
	(* n n)))

(square 0) ;;=> 4




;;4. Перепешите month-num, используя case вместо svref.

(defun leap? (y)
    (and (zerop (mod y 4))
	 (or (zerop (mod y 400))
	     (not (zerop (mod y 100))))))

(defun month-num (m y)
    (+ (case m
	 (1 0)
	 (2 31)
	 (3 59)
	 (4 90)
	 (5 120)
	 (6 151)
	 (7 181)
	 (8 212)
	 (9 243)
	 (10 273)
	 (11 304)
	 (12 334)
	 (13 365))
       (if (and (> m 2) (leap? y)) 1 0)))

(month-num 2 2015) ;;=> 31




;;5. Определите функцию, принимающую объект и вектор и возвращающую новый список,
;;   в котором находятся знаки стоящие непосредственно перед объектом.

;;   Рекурсия:
(defun prece (x v)
    (let ((pos (position x v)))
	(if pos
	    (if (zerop pos)
		(prece x (subseq v (1+ pos)))
		(cons (aref v (1- pos))
		      (prece x (subseq v (1+ pos))))))))

(defun precedes-rec (x v)
    (remove-duplicates (prece x v)))

(precedes-rec #\a "abracadabra") ;;=> (#\c #\d #\r)

;;Итерация:

(defun precedes-it (x v)
    (let (lst)
	(dotimes (i (length v))
	    (and (eql x (aref v i))
		 (not (zerop i))
		 (push (aref v (- i 1)) lst)))
	(remove-duplicates (reverse lst))))

(precedes-it #\a "abracadabra") ;;=> (#\c #\d #\r)




;;6. Определите функцию, принимающую объект и список и возвращающую новый список,
;;   в котором заданный элемент находится между каждой парой элементов исходного списка.

;;   Рекурсия

(defun intersperce-rec (sym lst)
    (if (null (cdr lst))
	lst
	(cons (car lst) (cons sym (intersperce-rec sym (cdr lst))))))

(intersperce-rec '- '(a b c d)) ;;=> (A - B - C - D)

;;   Итерация

(defun intersperce-it (sym lst)
    (let ((new-lst nil)
	  (last-elt (car (reverse lst))))
	(dolist (elt lst)
	    (and (eql elt last-elt)
		 (push elt new-lst)
		 (return))
	    (push elt new-lst)
	    (push sym new-lst))
	(reverse new-lst)))
	    
	      

(intersperce-it '- '(a b c d e f)) ;;=> (A - B - C - D)




;;6. Определите функцию, принимающую список чисел и возвращающую истину,
;;   если разница между каждой последующей их парой равна 1.


;;   Рекурсия
(defun difference (lst)
    (if (null (cdr lst))
	t
	(if (eql (- (car lst) (cadr lst)) 1)
	    (difference (cdr lst)))))

(difference '(6 5 4 3 2 1)) ;;=> T


;;  do

(defun difference-do (lst)
    (do ((i lst (cdr i)))
	((null (cdr i)) t)

	(if (not (eql (- (car i) (cadr i)) 1))
	    (return))))

(difference-do '(6 5 4 3 2 1)) ;;=> T

;;  mapcar и return

(defun difference-mapc (lst)
    (block nil
	(mapc #'(lambda (x y)
		    (when (not (= (- x y) 1))
			(return)))
	      lst
	      (cdr lst))
	t))

(difference-mapc '(6 5 4 3 2 1)) ;;=> T




;;8. Определите одиночную рекурсивную функцию, которая возвращает
;;   максимальный и минимальный элементы списка.

(defun min-max (vec)
    (if (= (length vec) 1)
	(values (svref vec 0) (svref vec 0))
	(values (multiple-value-bind (min max) 
		    (if (< (svref vec 0) (svref vec 1))
			(min-max (remove (svref vec 1) vec))
			(min-max (subseq vec 1)))
		    min)
		(multiple-value-bind (min max) 
		    (if (> (svref vec 0) (svref vec 1))
			(min-max (remove (svref vec 1) vec))
			(min-max (subseq vec 1)))
		    max))))

;; (svrev vec 0) - первый элемент вектора
;; (svrev vec 1) - второй элемент вектора
;; (subseq vec 1) - хвост вектора
		
(min-max #(45 3 -100 2 2 -100 2312 21 2312)) ;;=> -100, 2312




;;9. С помощью catch throw и без измените программу (рис 3.12) таким образом,
;;   чтобы она возвращала первый найденный путь в сети сразу же после того, как он найден.

;;catch trow:

(defun shortest-path (start end net)
    (catch 'bfs
	(bfs end (list (list start)) net)))

(defun bfs (end queue net)
	(if (null queue)
	    nil
	    (let* ((path (car queue))
		   (node (car path)))
		(if (eql node end)
		    (reverse path)
		    (bfs end
			 (append (cdr queue)
				 (new-paths path node net end))
			 net)))))

(defun new-paths (path node net end)
    (mapcar #'(lambda (n)
		  (let ((new-path (cons n path)))
		      (if (eql (car new-path) end)
			  (throw 'bfs (reverse new-path))
			  new-path)))
	    (cdr (assoc node net))))

(shortest-path 'a 'd '((a b c) (b c) (c d))) ;;=> (A B C D)


;; без catch trow:
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
    (if (null queue)
	nil
	(let ((path (car queue)))
	    (let ((node (car path)))
		(mapc #'(lambda (lst) 
			    (print lst)
			    (if (member end lst)
				(return-from bfs (reverse lst))))
		      queue)
		(bfs end
		     (append (cdr queue)
			     (new-paths path node net))
		     net)))))

(defun new-paths (path node net)
    (mapcar #'(lambda (n)
		  (cons n path))
            (cdr (assoc node net))))

(shortest-path 'a 'd '((a b c) (b c) (c d))) ;;=> (A C D)
