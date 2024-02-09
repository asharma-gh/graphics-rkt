#lang racket
(require graphics/graphics)

(open-graphics)
(define vp (open-viewport "VP TEST" (make-posn 600 400)))

;; Number Number Number Number -> Empty list
;; Generates a Line based on the DDA Algorithm 
;; NOTE: Modifies global Viewport (vp)
(define (dda xa ya xb yb (col "blue"))
  (let* ([x xa]
         [y ya]
         [dx (- xb xa)]
         [dy (- yb ya)]
         [steps (if (> (abs dx) (abs dy)) (abs dx) (abs dy))]
         [xinc (/ dx steps)]
         [yinc (/ dy steps)])
    (map
     (λ (ii) ((draw-pixel vp) ii col))
     (build-list steps (λ (ii) (make-posn (+ x (floor (* xinc ii))) (+ y (floor (* yinc ii)))))))
    empty))


(dda 40  390 580 20)
(dda 200 200 103 100 "red")
(dda 100 100 500 100)
(dda 50  50  50  200 "purple")