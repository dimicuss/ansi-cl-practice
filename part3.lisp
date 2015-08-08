;; 1. Представьте следующие списки в виде ячеек

'(a b (c d))

;; |---|---|   |---|---|   |---|---|
;; | a |   | > | b |   | > |   |nil|
;; |---|---|   |---|---|   |---|---|
;;                           |
;;                           |
;;                         |---|---|   |---|---|
;;                         | c |   | > | d |nil|
;;                         |---|---|   |---|---|

'(a (b (c (d))))

;; |---|---|   |---|---|
;; | a |   | > |   |nil|
;; |---|---|   |---|---|
;;               |
;;               |
;;             |---|---|   |---|---|
;;             | b |   | > |   |nil|
;;             |---|---|   |---|---|
;;                           |
;;                           |
;;                         |---|---|   |---|---|
;;                         | c |   | > |   |nil|
;;                         |---|---|   |---|---|
;;                                       |
;;                                       |
;;                                     |---|---|
;;                                     | d |nil|
;;                                     |---|---|

'(((a b) c) d)

;; |---|---|   |---|---|
;; |   |   | > | d |nil|
;; |---|---|   |---|---|
;;   |
;;   |
;; |---|---|   |---|---|
;; |   |   | > | c |nil|
;; |---|---|   |---|---|
;;   |
;;   |
;; |---|---|   |---|---|
;; | a |   | > | b |nil|
;; |---|---|   |---|---|

'(a (b . c) . d)

;; |---|---|    |---|---|
;; | a |   | >  |   | d |
;; |---|---|    |---|---|
;;                |
;;                |
;;            |---|---|
;;            | b | c |
;;            |---|---|

;;(украдено у feola)

;;2. Напишите свой вариант union, который сохраняет порядок 
;;   следования элементов согласно исходным спискам.

(defun new-union (lst1 lst2)
  (if (null lst2)
      lst1
      (if (member (car lst2) lst1)
	  (new-union lst1 (cdr lst2))
	  (new-union (reverse (cons (car lst2) 
				    (reverse lst1)))
		     (cdr lst2)))))

(new-union '(a b c) '(b a d)) ;; => (A B C D)




;;3. Напишите функцию, определяющую количество повторений (с точки зрения eql)
;;   каждого элемента в заданном списке и сортирующую их по убыванию встречаемости.

(defun count-elm (elm lst)
  (if lst
      (if (eql elm (car lst))
	  (+ 1 (count-elm elm (cdr lst)))
	  (count-elm elm (cdr lst)))
      0))

(defun occur (lst)
  (if lst
      (cons 
       (cons (car lst) (count-elm (car lst) lst))
       (occur (remove (car lst) lst)))))

(defun occurrences (lst)
  (sort (occur lst) #'> :key #'cdr))

(occurrences '(a b a d a c d c a)) ;;=> ((A . 4) (D . 2) (C . 2) (B . 1))




;;4. Почему (member '(a) '((a) (b))) => nil.
;;   Так как функция eql, которая используется по умолчанию в member,
;;   сравнивает только знаки и числа, то при вызове member следует использовать :key #'equal:

(member '(a) '((a) (b)) :test #'equal) ;;=> ((A) (B))


;;5. Функция pos+ принимает список и возвращает новый, каждый элемент которого
;;   увеличен на его позицию в списке.

;;Рекурсия:
(defun pos+ (lst)
  (if lst
      (reverse (cons (+ (car (reverse lst)) (length (cdr lst)))
		     (reverse (pos+ (reverse (cdr (reverse lst)))))))))
(pos+ '(7 5 1 4)) ;;=> (7 6 3 7)

;;Итерация
(defun ipos+ (lst)
  (let (new-lst)
    (dotimes (i (length lst))
      (push (+ (nth i lst) i) new-lst))
    (reverse new-lst)))

(ipos+ '(7 5 1 4)) ;;=> (7 6 3 7)

;;Mapcar

(defun mappos+ (lst)
  (let ((i -1))
    (mapcar #'(lambda (elt)
		(+ (incf i) elt))
	    lst)))

(mappos+ '(7 5 1 4))


;;Определите функции cons, list, length и member, при условии что car = хвост списка и cdr = голова списка.
;;6. A

(defun bad-cons (a b)
  (let ((cns '(nil . nil)))
    (setf (cdr cns) a)
    (setf (car cns) b)
    cns))

(defun bad-cons_ (a b) ;; Альтернатива
  `(,b . ,a))

(bad-cons nil 'a) ;; => (A)

;;6. B

(defun bad-list (&rest args)
  (cons (cdr args) (car args)))

(bad-list 1 2 3 4 5) ;;=> ((2 3 4 5) . 1)

;;6. C 
(defun bad-length (lst)
  (if lst
      (+ 1 (bad-length (car (cons (cdr lst) (car lst)))))
      0))

(bad-length '(a b c d)) ;=> 4

;;6. D

(defun bad-member (obj lst)
  (if lst
      (if (eql (cdr (cons (cdr lst) (car lst))) obj)
	  lst
	  (bad-member obj (car (cons (cdr lst) (car lst)))))))

(bad-member 'c '(a b c d)) ;;=> (C D)




;;7. Измените программу compress таким образом, чтобы она создавала как можно меньше ячеек.
;;   Под "как можно меньше ячеек" подразумевается замена в n-elts (list n elt) на (cons n elt).
   
(defun compress (lst)
  (compr (car lst) 1 (cdr lst)))

(defun compr (fst n lst)
  (if (null lst)
      (cons (n-elts fst n) nil)
      (let ((next (car lst)))
	(if (eql fst next)
	    (compr fst (1+ n) (cdr lst))
	    (cons (n-elts fst n) (compr next 1 (cdr lst)))))))

(defun n-elts (elm n)
  (if (> n 1)
      (cons n elm)
      elm))

(compress '(1 1 1 0 1 0 0 0 0 ))

    
;;8. Определите функцию печатающую список в точечной нотации.

(defun showdots (lst) 
  (if lst
      (if (atom lst)
	  lst
	  (format nil "(~A . ~B)" (showdots (car lst)) (showdots (cdr lst))))))


(showdots '(a b c d)) ;;=> "(A . (B . (C . (D . NIL))))"




;;9. Напишите программу, которая ищет наиболее длинный путь в сети содержащей циклы.

(defun longest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if queue
      (let ((path (car queue)))
        (let ((node (car path)))
          (if (eql node end)
              (reverse path)
              (bfs end
                   (append (new-paths path node net)
			   (cdr queue))
                   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (unless (member n path)
		(cons n path)))
	  (cdr (assoc node net))))


(setf net '((a b c) (b a c f) (c a d)))

(longest-path 'a 'd net) ;;=> (A B C D)
