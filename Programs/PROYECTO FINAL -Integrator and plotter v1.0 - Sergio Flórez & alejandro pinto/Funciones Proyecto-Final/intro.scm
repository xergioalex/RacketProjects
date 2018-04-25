;ESTA ES UNA FUNCIÓN DE PRUEBA PARA LA INTRO

(require (lib "graphics.ss" "graphics"))

(open-graphics)
(define ventana (open-viewport "Graficador" 590 640))

(((draw-pixmap-posn "Logo.bmp" 'bmp) ventana) (make-posn 0 0) "black")
;((draw-solid-rectangle ventana)(make-posn 160 500) 260 13 "orange")

(define (tiempo_espera posicion_x inicial final)
  (if (< 0 final)
      (tiempo_espera posicion_x inicial (- final 1))
      (barra_carga (+ posicion_x 3))
  )      
)

(define (barra_carga posicion_x)
  (if (> posicion_x 412)
      (display "Fin")
      (begin
        ((draw-solid-rectangle ventana)(make-posn posicion_x 492) 1 30 "orange")
        (tiempo_espera posicion_x 1 500000)
      )          
  )
)
(barra_carga 184)      


