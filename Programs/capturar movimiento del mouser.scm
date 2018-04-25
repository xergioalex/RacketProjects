;La siguiente funcion captura el movimiento del mouse

(require (lib "graphics.ss" "graphics"))

(open-graphics)

;Aqui defino la ventana
(define ventana (open-viewport "Capturar Mouse" 600 600))

;


;Esta funcion muestra la posicion del mouse en la pantalla
(define (MostrarPosicion posicion)
        ((draw-solid-rectangle ventana) (make-posn 401 501) 98 28 "white")
        ((draw-string ventana) (make-posn 401 520) (number->string (posn-x posicion)))
        ((draw-string ventana) (make-posn 451 520) (number->string (posn-y posicion)))
)

;Esta funcion valida que el click dado sobre el cuadro cerrar 
(define (ValidarClick c)
        (define po1 (make-posn 100 500))
        (define po2 (make-posn 200 530))
        (define po3 (make-posn 100 100))
        (define po4 (make-posn 500 350))
        (define po (mouse-click-posn c))
        (if (and (and (<= (posn-x po) (posn-x po2)) (<= (posn-y po) (posn-y po2))) (and (>= (posn-x po) (posn-x po1)) (>= (posn-y po) (posn-y po1))))
            (begin
                 (display "Good Bye!")
                 (close-viewport ventana)
             )
            (begin
                  (if (and (and (<= (posn-x po) (posn-x po4)) (<= (posn-y po) (posn-y po4))) (and (>= (posn-x po) (posn-x po3)) (>= (posn-y po) (posn-y po3))))
                      (begin
                            (Lapiz ventana po3 po4)
                       )
                      (begin
                            (EsperarClick ventana)
                      )
                  )
            )
        )
)

;Esta funcione espera la respuesta del click del mouse
(define (EsperarClick ventana)
        (define c (ready-mouse-click ventana))
        (if (not c)
            (begin
                 (MostrarPosicion (query-mouse-posn ventana))
                 (EsperarClick ventana)
             )
             (begin
                  (ValidarClick c)
             )    
        )
)

;Esta funcion imprime en pantalla los rectangulos y la palabra cerrar
(define (Pintar_cuadros ventana)
        ((draw-rectangle ventana) (make-posn 100 500) 100 30 "green")
        ((draw-rectangle ventana) (make-posn 400 500) 100 30 "green")
        ((draw-rectangle ventana) (make-posn 100 100) 400 250 "green")
        ((draw-string ventana) (make-posn 118 520) "CERRAR")
        (EsperarClick ventana)
        
)  

;Esta funcion pinta pixeles en pantalla
(define (Lapiz ventana po1 po2)
        (define c (ready-mouse-release ventana))
        (define po (query-mouse-posn ventana))
        (if (not c)
            (begin
                  (if (and (and (<= (posn-x po) (posn-x po2)) (<= (posn-y po) (posn-y po2))) (and (>= (posn-x po) (posn-x po1)) (>= (posn-y po) (posn-y po1))))
                      (begin
                            ((draw-pixel ventana) po "blue")
                            (Lapiz ventana po1 po2)
                       )
                       (begin
                            (EsperarClick ventana)
                       )
                   )
              )
             (begin
                   (EsperarClick ventana)
             )
         )
)

(Pintar_cuadros ventana)

(close-graphics)