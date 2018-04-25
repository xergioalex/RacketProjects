(require (lib "graphics.ss" "graphics"))

(open-graphics)

(define ventana (open-viewport "Graficador" 650 650))
((draw-line ventana) (make-posn 325 0) (make-posn 325 650) "black")
((draw-line ventana) (make-posn 0 325) (make-posn 650 325) "black")

;------------------------------------------VALOR EN Y--------------------------------------------
(define (valores_y vector-coeficientes vector-exponentes valor-x contador)
        (if (= contador (vector-length vector-coeficientes))
            0
            (+(* (vector-ref vector-coeficientes contador) (expt valor-x (vector-ref vector-exponentes contador)))
              (valores_y vector-coeficientes vector-exponentes valor-x (+ contador 1)))
       )
)
;-------------------------------------------------------------------------------------------------------------
(define (graficar vector-coeficientes vector-exponentes valor-x )
        (if (= valor-x 4)
            "fin"
            (begin
                 ((draw-rectangle ventana) (make-posn (+ 325 (* valor-x 100)) (+ 325 (*(* (valores_y vector-coeficientes vector-exponentes valor-x 0) -1) 100) )) 
                   2 2 "blue") 
                  (graficar vector-coeficientes vector-exponentes (+ valor-x 0.0001) )
            )
        )
)

;------------------------------------------CREAR Y LLENAR VECTOR---------------------------------------------
(define (llenar_vector vector contador numero_vector)
        (if (= contador (vector-length vector))
            vector
            (begin
                  (display "Por favor ingrese el elemento numero ")
                  (display (+ contador 1))
                  (display " del vector ")
                  (display numero_vector )
                  (display " :")
                  (vector-set! vector contador (read))
                  (llenar_vector vector (+ contador 1) numero_vector)
            )
        )
)

(graficar (llenar_vector (make-vector 3) 0 "coeficiente") (llenar_vector (make-vector 3) 0 "Exponentes") -4 )
(read)