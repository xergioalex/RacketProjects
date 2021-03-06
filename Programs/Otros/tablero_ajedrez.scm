(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define A(open-viewport "Ajedrez" 640 640))
(define (Tablero_ajedrez x y)
  (if (< y 640)
      (if (< x 640)
          (begin
            ((draw-solid-rectangle A)(make-posn x y) 80 80 (make-rgb 00 00 00))
            (Tablero_ajedrez (+ x 160) y)
            )
          (Tablero_ajedrez (- x 720) (+ y 80))
          )
      )
  )

(Tablero_ajedrez 80 0)