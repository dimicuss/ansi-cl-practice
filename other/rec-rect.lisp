(defun draw-rects (x y a b n) 
  (let ((crAx (- x (/ a 2)))
        (crAy (- y b))
        (crBx (- x (/ a 2)))
        (crBy (- y (/ b 2)))
        (crCx (+ x a))
        (crCy (+ y (/ b 2)))
        (crDx (+ x a))
        (crDy (+ y b)))
    (draw-rectangle-* crAx crAy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crBx crBy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crCx crCy (ceiling a 2) (ceiling b 2))
    (draw-rectangle-* crDx crDy (ceiling a 2) (ceiling b 2))
    
    (draw-rects crAx crAy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crBx crBy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crCx crCy (ceiling a 2) (ceiling b 2) (- n 1))
    (draw-rects crDx crDy (ceiling a 2) (ceiling b 2) (- n 1))))
    
    
    
(draw-rectangle-* 0 900 900 900)
(draw-cects 0 900 900 900 5)


    
    
