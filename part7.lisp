;;1. Определите функцию возращающую список из строк,
;;   прочитанных из заданного файла.

(defun cat-list (file)
  (let (acc)
    (with-open-file (str file :direction :input)
      (do ((line (read-line str nil 'eof)
		 (read-line str nil 'eof)))
	  ((eql line 'eof))
	(push line acc)))
    (reverse acc)))

(cat-list "test/test.txt") ;; => ("Test1" "Test2" "Test3" "Test4" "Test5")




;;2. Определите функцию, возвращающую список выражений,
;;   содержащихся в данном файле.
	   
(defun list-exprs (file)
  (let (acc)
    (with-open-file (str file :direction :input)
      (do ((expr (read str nil 'eof)
		 (read str nil 'eof)))
	  ((eql expr 'eof))
	(if (listp expr)
	    (push expr acc))))
    (reverse acc)))

(list-exprs "test/expr.txt") ;;=> ((+ 2 3 4) (APPLY FN ARGS) (/ 1 0) (/ 1 0))




;;3. Пусть в файле некоторого формата комментарии помечаются знаком %.
;;   Определите функцию, кторая принимает 2 имени файла и записывает
;;   во второй содержимое первого без комментариев.

(defun ign-comm (file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			       :if-exists :supersede)
      (do ((line (read-line in nil 'eof)
		 (read-line in nil 'eof)))
	  ((eql line 'eof))
	(unless (and (not (zerop (length line)))
		     (eql (read-from-string line nil 'eof :end 1) '%))
	  (format out "~A~%" line))))))

(ign-comm "test/script.m" "test/script.m~~") ;;=> nil




;;4.Определите функцию, принимающую двумерный массив чисел с плавающей точкой,
;;  и выводящую его аккуратными столбцами с 2-мя  знаками после запятой и 
;;  пространстве в 10 знаков.

(defun parse-array (arr)
  (let ((dim (array-dimensions arr)))
    (dotimes (i (car dim))
      (dotimes (j (- (cadr dim) 1)
		  (format t "~10,2F~%" (aref arr i j)))
	(format t "~10,2F" (aref arr i j))))))

(parse-array #2A((1.34 2.122 3.56 4.12334 5.4323) 
		 (1.34 2.122 3.56 4.12334 5.5643) 
		 (1.34 2.122 3.56 4.12334 5.5643) 
		 (1.34 2.122 3.56 4.12334 5.5643)
		 (1.34 2.122 3.56 4.12334 5.5643))) ;;=> 1.34      2.12      3.56      4.12      5.43
                                                    ;;   1.34      2.12      3.56      4.12      5.56
                                                    ;;   1.34      2.12      3.56      4.12      5.56
                                                    ;;   1.34      2.12      3.56      4.12      5.56
                                                    ;;   1.34      2.12      3.56      4.12      5.56 in repl


;;5. Измените stream-subst так, чтобы она понимала шаблоны с 
;;   метазнаком +. Если знак + встречается в строке old, он 
;;   может соответствовать любому занку.


(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((or ;;Нужно добавить следующую строку
	      (char= #\+ (char old pos)) 
	      (char= c (char old pos)))
	     (incf pos)

	     (cond ((= pos len)           
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)         
		    (buf-insert c buf)))

	     ((zerop pos)                   
	      (princ c out)
	      (when from-buf
		(buf-pop buf)
		(buf-reset buf)))
	     (t                             
	      (unless from-buf
		(buf-insert c buf))
	      (princ (buf-pop buf) out)
	      (buf-reset buf)
	      (setf pos 0))))
      (buf-flush buf out))))	    



