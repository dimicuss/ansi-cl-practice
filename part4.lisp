;;1. Определите функцию, поворачивающую квадратный массив на 90 
;;   градусов по часовой стрелке.
;;   Для решение данного примера потребуется транспонирование матрицы (массива).

(defun quarter-turn (arr)
    (let* ((n (car (array-dimensions arr)))
	   (new-arr (make-array (list n n))))
	(dotimes (i n)
	    (dotimes (j n)
		(setf (aref new-arr j (- n i 1))
		      (aref arr i j))))
	new-arr))

(quarter-turn #2a((A B C D) (E F G H) (I G K L) (M N O P)))

;;=> #2A((M I E A) (N G F B) (O K G C) (P L H D))

;;2. Разберитесь с описанием reduce на странице 382, затем 
;;   с ее помощью определите: compy-list и reverse.


(defun owr-copy-list (lst)
    (reduce #'cons lst :from-end t :initial-value nil))

(defun owr-reverse (lst)
    (reduce #'(lambda (a b) (cons b a)) lst :initial-value nil))


(owr-copy-list '(1 2 3 4 5 6)) ;;=> (1 2 3 4 5 6)

(owr-reverse '(1 2 3 4 5 6)) ;;=> (6 5 4 3 2 1)




;;3. Создайте структуру для дерева, каждый узел которого помимо некоторых 
;;   данных  имеет трех потомков. Определите: функцию копирующее такое дерево,
;;   а также функцию, которая ищет объект в дереве.

(defstruct node
  data 
  f
  s
  t)

(defun tree (num depth) ;;Функция создающее дерево. Делит num на 3 и записывает результат в последующие 3
    (if (<= num depth)  ;;узла.
	(make-node
	 :data num)
	(make-node 
	 :data num
	 :f (tree (/ num 3) depth)
	 :s (tree (/ num 3) depth)
	 :t (tree (/ num 3) depth)))) 


(defun copy-ternary-tree (tree)
    (if tree
	(make-node
	 :data (node-data tree)
	 :f (copy-ternary-tree (node-f tree))
	 :s (copy-ternary-tree (node-s tree))
	 :t (copy-ternary-tree (node-t tree)))))

(defun find-obj (obj tree)
    (when tree
	(if (eql obj (node-data tree))
	    t
	    (progn
		(find-obj obj (node-f tree))
		(find-obj obj (node-s tree))
		(find-obj obj (node-t tree))))))

(defun tree-to-list (tree)                    ;;функция для отображения дерева в виде списка. Написал по фану. 
    (let* ((first-n (node-f tree))            ;;В задании может не использоваться.
	   (second-n (node-s tree))
	   (third-n (node-t tree))
	   (first-d (node-data first-n))
	   (second-d (node-data second-n))
	   (third-d (node-data third-n)))
	(list
	 (list first-d
	       (if (node-f first-n)
		   (tree-to-list first-n)
		   'end))
	 (list second-d 
	       (if (node-s second-n)
		   (tree-to-list second-n)
		   'end))
	 (list third-d
	       (if (node-t third-n)
		   (tree-to-list third-n)
		   'end))))) 


(setf tr (tree 81 1))

(copy-ternary-tree tr) ;;=> new tree

(find-obj 27 tr) ;;=> T




;;4. Опоеделите функцию, которая строит из BST-дерева список его объектов,
;;   сортированный от большего к меньшему.

(defstruct node
  elt
  l
  r)

(defun bst-insert (obj bst <) ;;Строит двоичное дерево
    (if (null bst)
	(make-node :elt obj)
	(let ((elt (node-elt bst)))
	    (if (eql obj elt)
		bst
		(if (funcall < obj elt)
		    (make-node
		     :elt elt
		     :l (bst-insert obj (node-l bst) <)
		     :r (node-r bst))
		    (make-node
		     :elt elt
		     :r (bst-insert obj (node-r bst) <)
		     :l (node-l bst)))))))



(defun bst-to-list (bst)
    (if bst
	(append (bst-to-list (node-r bst)) ;;append рулит!! Если поменять местами (node-t bst)
		(list (node-elt bst))      ;;и (node-l bst), то список сортируется от большего к меньему.
		(bst-to-list (node-l bst)))))

(setf nums nil)

(dolist (x '(5 8 4 2 1 9 6 7 3))
    (setf nums (bst-insert x nums #'<)))

(bst-to-list nums) ;;=> (9 8 7 6 5 4 3 2 1)

;;5. bst-adjoin=bst=insert


;;6. Представить хэш таблицу в вид списка и наоборот.

(setf x '((a . 1) (b . 2) (c . 34) (d . 4)))

(defun lst-hash (lst)
    (let ((table (make-hash-table)))
	(dolist (elt lst)
	    (setf (gethash (car elt) table) (cdr elt)))
	table))

(defun hash-lst (table)
    (let ((lst))
	(maphash #'(lambda (k v)
		       (push (cons k v) lst))
		 table)
	lst))


(setf hash (lst-hash x)) ;;=> #S(HASH-TABLE :TEST FASTHASH-EQL (D . 4) (C . 34) (B . 2) (A . 1))


(hash-lst hash) ;;=> ((A . 1) (B . 2) (C . 34) (D . 4))


