;;1. Определите функцию принимающую список действительных чисел
;;   и возвращающую истину, когда числа слуют в порядке неубывания.

(defun nondecreasing (lst)
  (if (cdr lst)
      (if (>= (cadr lst) (car lst))
	  (nondecreasing (cdr lst)))
      t))

(nondecreasing '(1 2 3 4 5 5 5)) ;;=> T




;;2. Определите функцию, принимающую целочисленную величину (кол-во
;;   центов и возращаующую 4 значения показывающих как собрать заданную
;;   сумму из 25, 10, 5 и 1 цента.
(let ((elt nil))
  (defun sum (n)
    (cond ((zerop n) (reverse elt))
	  ((>= n 25) (push 25 elt) (sum (- n 25)))
	  ((>= n 10) (push 10 elt) (sum (- n 10)))
	  ((>= n 5) (push 5 elt) (sum (- n 5)))
	  ((>= n 1) (push 1 elt) (sum (- n 1))))))

(sum 96) ;; => (25 25 25 10 10 1)




;;3. Так как соотношение победителей всегда в пределах от 4/6 до 6/4,
;;   то можно сказать, что судии некомпетентны и их решения неверны.
;;   Вот примр симуляции соревнований.

(defun wiggles-wobbles (i)
  (if (zerop i)
      nil
      (let* ((wiggle (+ 1 (random 10)))
	    (wobble (- 10 wiggle)))
	(cons (list wiggle wobble)
	      (wiggles-wobbles (- i 1))))))

(wiggles-wobbles 10) ;;= random
    

	  
;;4. Украдено у shido.

(defun isec (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((dx1 (- x2 x1))
	 (dy1 (- y2 y1))
	 (dx2 (- x4 x3))
	 (dy2 (- y4 y3))
	 (dx3 (- x3 x1))
	 (dy3 (- y3 y1))
	 (d (- (* dx1 dy2) (* dx2 dy1))))
    (unless (= d 0)
      (let ((k1 (/ (- (* dx3 dy2) (* dx2 dy3)) d))
	    (k2 (/ (- (* dx3 dy1) (* dx1 dy3)) d)))
	(if (and (<= 0 k1 1) (<= 0 k2 1))
	    (cons (+ x1 (* dx1 k1)) (+ y1 (* dy1 k1))))))))




;;5. Реализуйте метод бисекции для поиска числа i при том, что f(i) = 0.

(defun bisection (f min max eps)
  (let* ((mid (/ (+ max min) 2))
	 (fmax (funcall f max))
	 (fmin (funcall f min))
	 (fmid (funcall f mid)))
    (cond ((> (* fmax fmin) 0) "error")
	  ((or (< (- max min) eps) (= fmid 0)) mid)
	  ((< (* fmin fmid) 0) (bisection f min mid eps))
	  (t (bisection f mid max eps)))))
	  
	     
(bisection #'(lambda (x) (- (+ (* x x) (* 2 x)) 1)) -2 5 0.001)




;;6. Реализуйте Метод Гарнера для решения полиномов.

(defun garner (x &rest nums)
  (if (cdr nums)
      (+ (* x (apply #'garner (cons x (reverse (cdr (reverse nums)))))) 
	 (car (last nums)))
      (car nums)))

(garner 8 6 4 3 1) ;;=> 3353

(defun reduce-garner (x &rest nums) ;;как альтернатива
  (reduce #'(lambda (a b)
	      (+ (* x a) b))
	  nums))

(reduce-garner 8 6 4 3 1) ;;=> 3353




;;7. Сколько бит использовала бы ваша реализация для предствавления
;;   fixnum

(log most-positive-fixnum 2) ;;=> 48.0 бит в clisp




;;8. Сколько различных типов чисел с плавающей запятой представлено
;;   в вашей реализации.

;;   В clisp представленно всего четыре типа: short-float, 
;;   single-float, double-float, long float.
;;   http://www.clisp.org/impnotes/num-concepts.html







