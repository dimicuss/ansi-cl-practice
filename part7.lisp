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
	(if (typep expr 'list)
	    (push expr acc))))
    (reverse acc)))

(list-exprs "test/expr.txt") ;; => => ((+ 2 3 4) (APPLY FN ARGS) (/ 1 0) (/ 1 0))
