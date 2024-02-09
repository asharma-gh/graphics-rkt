#lang racket

(require graphics/graphics)
;; 
(open-graphics)
(define vp (open-viewport "VP TEST" (make-posn 600 400)))
;;
;; Number Number Number Number Color -> Empty list
;; Generates a Line based on the DDA Algorithm 
;; NOTE: Modifies global Viewport (vp)
(define (dda xa ya xb yb (col "blue"))
  (let* ([x     xa]
         [y     ya]
         [dx    (- xb xa)]
         [dy    (- yb ya)]
         [steps (if (> (abs dx) (abs dy)) (add1 (abs dx)) (add1 (abs dy)))]
         [xinc  (/ dx steps)]
         [yinc  (/ dy steps)])
     (build-list steps
                 (λ (ii)
                   ((draw-pixel vp)
                    (make-posn (+ x (floor (* xinc ii))) (+ y (floor (* yinc ii))))
                    col))) empty))
;;
;; Number Number Number Number Color -> Empty List
;; Generates a Line using Bresenham's Algorithm
;; NOTE: Modifies the global Viewport (vp)
;; NOTE: Only renders lines where |dy/dx| < 1
(define (bresenham xa ya xb yb (col "black"))
  (let* ([abdx     (abs (- xb xa))]
         [abdy     (abs (- yb ya))]
         [p        (- (* 2 abdy) abdx)]
         [2dy      (* 2 abdy)]
         [2dydx    (* 2 (- abdy abdx))]
         [boundpts (if (< xa xb)
                       (cons (make-posn xa ya) (make-posn xb yb))
                       (cons (make-posn xb yb) (make-posn xa ya)))]
         [startpt (car boundpts)]
         [endpt   (cdr boundpts)])
    ((draw-pixel vp) startpt)
    (foldl (λ (cur-x acc)
             ((λ (in)
               ((draw-pixel vp) (make-posn cur-x (cdr in)) col)
                in)
              (if (< (car acc) 0)
                 (cons (+ 2dy (car acc)) (cdr acc))
                 (cons (+ 2dydx (car acc))
                              (sub1 (cdr acc))))))
           (cons p (posn-y startpt))
           (build-list (add1 abdx) (λ (ii) (+ ii (posn-x startpt))))) empty)) 
    

;;
(define (test-line fun) `(,(fun 40  390 580 20)
                          ,(fun 200 200 103 100 "red")
                          ,(fun 100 100 500 100 "green")
                          ,(fun 50  50  50  200 "purple")))
;(test-line dda)
(test-line bresenham)
