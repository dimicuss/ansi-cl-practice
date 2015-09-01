(ql:quickload :cl-cairo2)
(in-package cl-cairo2)

(defmacro with-pdf-context (args &rest body)
  `(let ((*context* (create-pdf-context ,(first args) 
					,(second args)
					,(third args))))
     ,@body
     (destroy *context*)))

(with-pdf-context ("slayer.pdf" 200 100)
  (set-source-rgb 0.2 0.2 1)
  (paint)
  (move-to 200 0)
  (line-to 0 100)
  (set-source-rgb 1 1 1)
  (set-line-width 5)
  (stroke))
  

