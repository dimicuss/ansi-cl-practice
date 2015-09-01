(ql:quickload :cl-cairo2)
(in-package cl-cairo2)

(defmacro with-svg (args &rest body)
  `(let ((*context* (create-svg-context ,(first args) 
					,(second args)
					,(third args))))
     ,@body
     (destroy *context*)))

(defun new-line (sx sy ex ey)
  (new-sub-path)
  (move-to sx sy)
  (line-to ex ey))

(with-svg ("slayer.svg" 200 200)
  (set-source-rgb 0 0 0)
  (paint)
  (arc 100 100 90 0 (* 2 pi))
  (new-line 100 190 30 1)
  (new-line 100 190 170 1)
  (new-line 40 30 199 130)
  (new-line 160 30 1 130)
  (set-source-rgb 1 0 0)
  (set-line-width 6)
  (stroke))
  

