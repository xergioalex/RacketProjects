(require (lib "graphics.ss" "graphics"))

(open-graphics)
(define ventana (open-viewport "Graficador" 590 660))

;------------------------------------------Pintar Ejes de la grafica----------------------------------------------------------
((draw-rectangle ventana) (make-posn 20 5) 550 550 "black") 
((draw-line ventana) (make-posn 295 5) (make-posn 295 555) "black")
((draw-line ventana) (make-posn 20 270) (make-posn 570 270) "black")
(define (pintar_ejes radio )
        (if (>  radio 6)
            "fin"
            (if (= radio 0)
                (pintar_ejes (+ radio 1))
                (begin
                    ((draw-string ventana) (make-posn (+ 291 (* radio 40)) 285 )  (number->string radio) "black")
                    ((draw-string ventana) (make-posn 281 (+ 276 (* radio 40)) )  (number->string (* radio -1)) "black")
                    (pintar_ejes (+ radio 1))
                )
            )
        )
)
(pintar_ejes -6 )

;------------------------------------------VALOR EN Y--------------------------------------------
;Esta funcion se encarga de remplazar cualquier valor de x en la ecuacion del polinomio, y retorna su respectivo en y
;ejemplo 3x+1  valor-x= 5  ---> valor-y= 16
(define (valores_y vector-coeficientes vector-exponentes valor-x contador)
        (if (= contador (vector-length vector-coeficientes))
            0
            (+(* (vector-ref vector-coeficientes contador) (expt valor-x (vector-ref vector-exponentes contador)))
              (valores_y vector-coeficientes vector-exponentes valor-x (+ contador 1)))
        )
)

;---------------------------------------------GRAFICAR-------------------------------------------
;Esta funcion se encarga de graficar el polinomio en la ventana
(define (graficar vector-coeficientes vector-exponentes valor-x )
      (if (= valor-x 10)
          "fin"
          (begin
              ((draw-solid-rectangle ventana) (make-posn (+ 295 (* valor-x 40)) (+ 270 (*(* (valores_y vector-coeficientes vector-exponentes valor-x 0) -1) 40) )) 
                2 2 "blue") 
              (graficar vector-coeficientes vector-exponentes (+ valor-x 0.001) )
          )
     )
)

;-----------------------------------------------DERIVAR-----------------------------------------------------
;Esta funcion recibe un polinomio y lo deriva
(define (derivar vector-coeficientes vector-exponentes contador)
        (if (= contador (vector-length vector-coeficientes))
            (begin
                 (display vector-coeficientes)
                 (display vector-exponentes)
            )
           (begin
                 (vector-set! vector-coeficientes  contador (* (vector-ref vector-coeficientes contador) (vector-ref vector-exponentes contador)))
                 (vector-set! vector-exponentes contador (- (vector-ref vector-exponentes contador) 1) )
                 (derivar vector-coeficientes vector-exponentes (+ contador 1))
           )
       )
)

;-----------------------------------------------INTEGRAR-----------------------------------------------------
;Esta funcion recibe un polinomio y lo integra
(define (integrar vector-coeficientes vector-exponentes contador)
        (if (= contador (vector-length vector-coeficientes))
            (begin
                 (display vector-coeficientes)
                 (display vector-exponentes)
                 (graficar vector-coeficientes vector-exponentes -10 )
            )
            (begin
                 (if (= (vector-ref vector-coeficientes contador) 0)
                     (integrar vector-coeficientes vector-exponentes (+ contador 1))
                     (begin
                           (vector-set! vector-exponentes contador (+ (vector-ref vector-exponentes contador) 1) )
                           (vector-set! vector-coeficientes  contador (/ (vector-ref vector-coeficientes contador) (vector-ref vector-exponentes contador)))
                           (integrar vector-coeficientes vector-exponentes (+ contador 1))
                      )
                 )
           )
       )
)

;------------------------------------------CREAR Y LLENAR VECTOR---------------------------------------------
;Esta funcion llena los vectores
(define (llenar_vector vector contador numero_vector)
        (if (= contador (vector-length vector))
            vector
            (begin
                  (vector-set! vector contador (read))
                  (llenar_vector vector (+ contador 1) numero_vector)
            )
        )
)

;-----------------------------------------------INICIO-----------------------------------------------------
(display "Por favor ingrese los elementos del cada vector: ")
(graficar (llenar_vector (make-vector 3) 0 "coeficiente") (llenar_vector (make-vector 3) 0 "Exponentes") -10 )

(close-graphics)