;;1. Могут ли два символа иметь одно имя но не быть 
;;   эквивалентными с точки зрения eql.
;;   Да, могут быть, если они из разных пакетов.

(defpackage "NEW-PACKAGE"
  (:export "SYM"))

(eql 'sym 'new-package:sym) ;;=> nil




;;2. Оцените различие между количеством памяти, использованной
;;   для определения строки "FOO" и символа foo.

;;   foo однозначно больше "FOO", так как foo содержит такие поля как:
;;   name = "FOO", package = "current-package" и тд.




;;3. В вызове defpackage, приведенном на стр. 148, использовались лишь
;;   строки. Тем немение вместо строк мы могли бы воспользоваться символами.
;;   Чем это может быть чревато.

;;   При определении пакета с использванием символов вместо строк, создаются
;;   соотвестствующие символы в текущем пакете, в моем  случаее в cl-user.

(defpackage foo-package)

(defpackage "BAR-PACKAGE")

(intern "FOO-PACKAGE")  ;;=> FOO-PACKAGE, :INTERNAL
(intern "BAR-PACKAGE")  ;;=> BAR-PACKAGE, nil




;;4. Добавте в программу на на рис. 7.1 код, который помещает 
;;   ее содержимое в пакет "RING". Аналогично для кода на рис. 7.2
;;   создайте пакет "FILE". Уже имеющийся код должени оставаться без изменений.

(defpackage "RING"
  (:use "COMMON-LISP")
  (:export "BUF" "BREF" "NEW-BUF" "BUF-INSERT" "BUF-POP" "BUF-NEXT" "BUF-RESET" "BUF-CLEAR" "BUF-FLUSH"))

(defpackage "FILE"
  (:use "COMMON-LISP" "RING")
  (:export "FILE-SUBST" "STREAM-SUBST"))



(in-package ring)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
         (mod n (length (buf-vec buf)))))

(defun (setf bref) (val buf n)
  (setf (svref (buf-vec buf)
               (mod n (length (buf-vec buf))))
        val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  (setf (bref b (incf (buf-end b))) x))

(defun buf-pop (b)
  (prog1 
    (bref b (incf (buf-start b)))
    (setf (buf-used b) (buf-start b)
          (buf-new  b) (buf-end   b))))

(defun buf-next (b)
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  (setf (buf-used b) (buf-start b)
        (buf-new  b) (buf-end   b)))

(defun buf-clear (b)
  (setf (buf-start b) -1 (buf-used  b) -1
        (buf-new   b) -1 (buf-end   b) -1))

(defun buf-flush (b str)
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))
  


(in-package file)

(defun file-subst (old new file1 file2)
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			       :if-exists :supersede)
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0)
         (len (length old))
         (buf (new-buf len))
         (from-buf nil))
    (do ((c (read-char in nil :eof)
            (or (setf from-buf (buf-next buf))
                (read-char in nil :eof))))
        ((eql c :eof))
      (cond ((char= c (char old pos))
             (incf pos)
             (cond ((= pos len)            
                    (princ new out)
                    (setf pos 0)
                    (buf-clear buf))
                   ((not from-buf)         
                    (buf-insert c buf))))
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
    (buf-flush buf out)))


(file:file-subst "Test" "shit" "test/test.txt" "../new.txt")


(in-package cl-user)

;;5. Напиште программу, проверяющую, была ли
;;   заданная цитата произведена с помощью Henley.



(defun is-it-henley (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword))          (pos 0))
      (do ((c (read-char s nil :eof) 
              (read-char s nil :eof)))
          ((eql c :eof) t)
        (cond ((or (alpha-char-p c) (char= c #\'))
	       (setf (aref buffer pos) c)
	       (incf pos))
	      (t (let ((buffer-sym (intern (string-downcase 
					    (subseq buffer 0 pos))))
		       (p (punc c)))		   
		   (unless (or (zerop pos)
			       (and (gethash buffer-sym *words*)
				    (if p 
					(gethash p *words*)
					t)))
		     (return-from is-it-henley))
		   (setf pos 0)))))))) 

;;  Перед вызовом нужно прочесть с помощью read-text исходный текст
;;  и создать *words* хэш таблицу с словами.

  

;;6. Напишите свою версию Henley, которая принимает слово и производит
;;   предложение, в середине которого находится слово.
;;   Заменить generate-text на функцию приведенную ниже.

(defun generate-text (n word)
  (let ((next (random-next '|.|))
	(mid (ceiling n 2)))
    (do ((i n (1- i)))
	((zerop i) (terpri))
      (if (= i mid)
	  (format t "~A " word)
	  (format t "~A " next))
      (setf next (random-next next)))))
      
