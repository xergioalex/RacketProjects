 (require (lib "graphics.ss" "graphics"))

(open-graphics)
(define ventana (open-viewport "Graficador" 590 640))

;------------------------------------------Pintar Ejes de la grafica----------------------------------------------------------
;Esta función se encarga de pintar el área de graficación.
((draw-viewport ventana) "black")   ;Pinta el fondo
((draw-solid-rectangle ventana) (make-posn 20 20) 550 550 "white")  ;Pinta el área dondes se encuentra el eje X y Y
((draw-line ventana) (make-posn 20 285) (make-posn 570 285) "black") ;Horizontal
((draw-line ventana) (make-posn 295 20) (make-posn 295 570) "black") ;Vertical
((draw-solid-rectangle ventana) (make-posn 20 592) 275 25 "white") ;Pinta el cuadro de ingreso para el polinomio
((draw-rectangle ventana) (make-posn 510 592) 60 25 "white")  ;Pinta el botón salir
((draw-string ventana) (make-posn 518 610) "SALIR" "white")  ;Escribe la palabra salir
((draw-ellipse ventana) (make-posn 435 600) 12 10 "white")
((draw-ellipse ventana) (make-posn 455 600) 12 10 "white")
((draw-ellipse ventana) (make-posn 475 600) 12 10 "white")

;((draw-solid-rectangle ventana) (make-posn 510 592) 5 5 "blue")
;((draw-solid-rectangle ventana) (make-posn 570 617) 5 5 "blue")
(define (pintar_ejes radio)
        (if (>  radio 9)
            "fin"
            (if (= radio 0)
                (pintar_ejes (+ radio 1))
                (begin
                    ((draw-solid-rectangle ventana) (make-posn (+ 295 (* radio 20)) 284.99 ) 3 3 "black") ;Puntos Horizontal
                    ((draw-solid-rectangle ventana) (make-posn 294 (+ 285 (* radio 20)) ) 3 3 "black") ;Puntos Vertical
                    ((draw-string ventana) (make-posn (+ 291 (* radio 20)) 300 )  (number->string radio) "black")  ;Numeros Horizontal
                    ((draw-string ventana) (make-posn 281 (+ 291 (* radio 20)) )  (number->string (* radio -1)) "black") ;Numeros Vertical
                    (pintar_ejes (+ radio 1))
                )
            )
        )
)
(pintar_ejes -9)

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
      (define limite_1 (make-posn 20 20))
      (define limite_2 (make-posn 570 570))
      (define punto_xy (make-posn valor-x (* (valores_y vector-coeficientes vector-exponentes valor-x 0) -1)))
      (if (= valor-x 20)
          "fin"
          (if (and (<= (+ 295 (* valor-x 20)) (posn-x limite_2))
                   (<= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_2))
                   (>= (+ 295 (* valor-x 20)) (posn-x limite_1))
                   (>= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_1))
              )
             
              (begin
                    ((draw-solid-ellipse ventana) (make-posn (+ 295 (* valor-x 20)) (+ 285 (* (posn-y punto_xy) 20) )) 1 1 "orange")
                    (graficar vector-coeficientes vector-exponentes (+ valor-x 0.001) )
              )
              (if  (or  (and (>= (+ 295 (* valor-x 20)) (posn-x limite_2)) (>= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_2)))
                        (and (>= (+ 295 (* valor-x 20)) (posn-x limite_2)) (<= (+ 295 (* valor-x 20)) (posn-x limite_1)))
                        (and (>= (+ 295 (* valor-x 20)) (posn-x limite_2)) (<= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_1)))
                        (and (>= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_2)) (<= (+ 295 (* valor-x 20)) (posn-x limite_1)))
                        (and (>= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_2)) (<= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_1)))
                        (and (<= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_1)) (<= (+ 285 (* (posn-y punto_xy) 20) ) (posn-y limite_1)))
                   )
                   (inicio 25 608 (make-string 1 #\@) 0)
                   (graficar vector-coeficientes vector-exponentes (+ valor-x 0.001) )
                   
              )
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
            (graficar vector-coeficientes vector-exponentes -20 )
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

(define (convertir->numero cadena contador)
      (if (char? cadena)
          (- (char->integer cadena) 48)         
          (if (= contador (string-length cadena))
              0
              (+  (* (- (char->integer (string-ref cadena contador)) 48) (expt 10 (- (string-length cadena) contador 1))) (convertir->numero cadena (+ contador 1)))
          )
     )
)

;--------------------------------------------------------ORDENAR Y SIMPLIFICAR------------------------------------------
;Esta funcion ordena el polinomio de acuerdo a los exponentes, y simplifica al mismo tiempo
(define (ordenar variable grado vector_coeficiente vector_exponente vector_coefi_ordenado vector_exp_ordenado posicion_vectores posicion_vec)
      (if (= grado -1)
          (begin
               (display vector_coefi_ordenado)
               (newline)
               (display vector_exp_ordenado)
               (graficar vector_coefi_ordenado vector_exp_ordenado -20 )
          )
          (if (= posicion_vectores (vector-length vector_coeficiente))
             (ordenar variable (- grado 1) vector_coeficiente vector_exponente vector_coefi_ordenado vector_exp_ordenado 0 (+ posicion_vec 1))
             (if (= (vector-ref vector_exponente posicion_vectores) grado)
                (begin
                     (vector-set! vector_coefi_ordenado posicion_vec (+ (vector-ref vector_coefi_ordenado posicion_vec) (vector-ref vector_coeficiente posicion_vectores)))
                     (vector-set! vector_exp_ordenado posicion_vec grado)
                     (ordenar variable grado vector_coeficiente vector_exponente vector_coefi_ordenado vector_exp_ordenado (+ posicion_vectores 1) posicion_vec)
                )
                (begin
                     (vector-set! vector_exp_ordenado posicion_vec grado)
                     (ordenar variable grado vector_coeficiente vector_exponente vector_coefi_ordenado vector_exp_ordenado (+ posicion_vectores 1) posicion_vec)
                )
             )
          )
      )
)

;"2+32x-2x2-23x21-587+3x2+3"
;--------------------------------------------------------ESTABLECER SIGNOS--------------------------------------------
(define (establecer_signos polinomio variable  grado vector_coeficiente vector_exponente posicion_pol posicion_vec)
        (if (= posicion_pol (string-length polinomio)) 
            (if (= posicion_vec 0)
                (ordenar variable grado vector_coeficiente vector_exponente (make-vector (+ grado 1) 0) (make-vector (+ grado 1) 0) 0 0)
                (if (> (vector-ref vector_exponente (- posicion_vec 1)) grado)
                    (establecer_signos polinomio variable  (vector-ref vector_exponente (- posicion_vec 1)) vector_coeficiente vector_exponente posicion_pol (- posicion_vec 1))
                    (establecer_signos polinomio variable  grado vector_coeficiente vector_exponente posicion_pol (- posicion_vec 1))
                )
            )
            (if (or (and (= posicion_pol 0) (and (>= (char->integer (string-ref polinomio posicion_pol)) 48) (<= (char->integer (string-ref polinomio posicion_pol)) 57)))
                    (and (= posicion_pol 0) (eqv? (string-ref polinomio posicion_pol) #\+))
                    (and (= posicion_pol 0) (eqv? (string-ref polinomio posicion_pol) variable))
                    (eqv? (string-ref polinomio posicion_pol) #\+))
               (begin
                    (vector-set! vector_exponente posicion_vec (convertir->numero (vector-ref vector_exponente posicion_vec) 0))
                    (vector-set! vector_coeficiente posicion_vec (convertir->numero (vector-ref vector_coeficiente posicion_vec) 0)) 
                    (establecer_signos polinomio variable  grado vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1))
               )
              (if (eqv? (string-ref polinomio posicion_pol) #\-)
                   (begin
                      (vector-set! vector_exponente posicion_vec (* (convertir->numero (vector-ref vector_exponente posicion_vec) 0) 1))
                      (vector-set! vector_coeficiente posicion_vec (* (convertir->numero (vector-ref vector_coeficiente posicion_vec) 0) -1))
                      (establecer_signos polinomio variable grado vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1))
                   )
                   (establecer_signos polinomio variable grado vector_coeficiente vector_exponente (+ posicion_pol 1) posicion_vec)
              )
          )
      )
)
;--------------------------------------------------------SEPARAR EN VECTORES------------------------------------------
;Esta funcion recibe el polinomio y saca sus respectivos coeficientes y exponentes en dos vectores diferentes

(define (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol posicion_vec cont_der cont_izq indep)
        (if (= posicion_pol (string-length polinomio))
            (establecer_signos polinomio variable 0 vector_coeficiente vector_exponente 0 0)
            (if (eqv? (string-ref polinomio posicion_pol) variable)
                (cond ;Aqui entra cuando la variable no tiene valores a los lados
                      ((or (= (string-length polinomio) 1)
                           (and (= posicion_pol 0) (or (eqv? (string-ref polinomio (+ posicion_pol 1)) #\+) (eqv? (string-ref polinomio (+ posicion_pol 1)) #\-)))
                           (and (= posicion_pol (- (string-length polinomio) 1))
                                (or (eqv? (string-ref polinomio (- posicion_pol 1)) #\+) (eqv? (string-ref polinomio (- posicion_pol 1)) #\-)))
                           (and (> posicion_pol 0)(or (eqv? (string-ref polinomio (- posicion_pol 1)) #\+) (eqv? (string-ref polinomio (- posicion_pol 1)) #\-))
                                (or (eqv? (string-ref polinomio (+ posicion_pol 1)) #\+) (eqv? (string-ref polinomio (+ posicion_pol 1)) #\-))))
                           (begin
                                 (vector-set! vector_coeficiente posicion_vec #\1)
                                 (vector-set! vector_exponente posicion_vec #\1)
                                 (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1) 1 1 indep)
                             )
                      
                      )
                      ;Aqui entra cuando la posicion es 0 o lo que hay al lado izquierdo de la variable es un operador
                      ((or (= posicion_pol 0) (eqv? (string-ref polinomio (- posicion_pol 1)) #\-) (eqv? (string-ref polinomio (- posicion_pol 1)) #\+))
                         (if (or  (= (+ posicion_pol (+ cont_izq 1)) (string-length polinomio))
                                  (eqv? (string-ref polinomio (+ posicion_pol (+ cont_der 1))) #\-)
                                  (eqv? (string-ref polinomio (+ posicion_pol (+ cont_der 1))) #\+))
                             (begin
                                 (vector-set! vector_coeficiente posicion_vec #\1)
                                 (vector-set! vector_exponente posicion_vec (substring polinomio (+ posicion_pol 1) (+ posicion_pol (+ cont_der 1))))
                                 (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1) 1 1 indep)
                             )
                             (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol posicion_vec (+ cont_der 1) cont_izq indep)
                         )
                      )
                      ;Aqui entra cuando la posicion es la ultima del polinomio o lo que hay al lado derecho de la variable es un operador
                      ((or (= posicion_pol (- (string-length polinomio) 1))
                           (eqv? (string-ref polinomio (+ posicion_pol 1)) #\-)
                           (eqv? (string-ref polinomio (+ posicion_pol 1)) #\+))
                         (if (or (= (- posicion_pol (+ cont_izq 1)) -1)
                                 (eqv? (string-ref polinomio (- posicion_pol (+ cont_izq 1))) #\-)
                                 (eqv? (string-ref polinomio (- posicion_pol (+ cont_izq 1))) #\+))
                             (begin
                                 (vector-set! vector_coeficiente posicion_vec (substring polinomio (- posicion_pol cont_izq ) posicion_pol))
                                 (vector-set! vector_exponente posicion_vec #\1)
                                 (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1) 1 1 indep)
                             )
                             (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol posicion_vec cont_der (+ cont_izq 1) indep)
                         )
                      )
                      
                      ;Aqui entra cuando a ambos lados de la variable hay numeros
                      (else
                         (if (or (= (- posicion_pol cont_izq) 0)
                                 (eqv? (string-ref polinomio (- posicion_pol (+ cont_izq 1))) #\-)
                                 (eqv? (string-ref polinomio (- posicion_pol (+ cont_izq 1))) #\+))
                             (if (or (= (+ posicion_pol cont_der) (- (string-length polinomio) 1))
                                     (eqv? (string-ref polinomio (+ posicion_pol (+ cont_der 1))) #\-)
                                     (eqv? (string-ref polinomio (+ posicion_pol (+ cont_der 1))) #\+))
                                 (begin
                                    (vector-set! vector_coeficiente posicion_vec (substring polinomio (- posicion_pol cont_izq ) posicion_pol))
                                    (vector-set! vector_exponente posicion_vec (substring polinomio (+ posicion_pol 1) (+ posicion_pol (+ cont_der 1))))
                                    (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1) 1 1 indep)
                                 )
                                 (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol posicion_vec (+ cont_der 1) cont_izq indep)
                             )
                             (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol posicion_vec cont_der (+ cont_izq 1) indep)
                         )
                      )
                )
                ;Aqui entra cuando hace referencia a un numero o al termino independiente el polinomio
               (if (and (and (>= (char->integer (string-ref polinomio posicion_pol)) 48) (<= (char->integer (string-ref polinomio posicion_pol)) 57))
                        (or  (= posicion_pol 0)
                             (eqv? (string-ref polinomio (- posicion_pol 1)) #\+)
                             (eqv? (string-ref polinomio (- posicion_pol 1)) #\-)))
                   (if (or (= (+ posicion_pol indep ) (string-length polinomio))
                           (eqv? (string-ref polinomio (+ posicion_pol indep )) #\+)
                           (eqv? (string-ref polinomio (+ posicion_pol indep )) #\-))
                       (begin
                            (vector-set! vector_coeficiente posicion_vec (substring polinomio posicion_pol (+ posicion_pol indep)))
                            (vector-set! vector_exponente posicion_vec #\0)
                            (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) (+ posicion_vec 1) 1 1 1)
                       )
                       (if (eqv? (string-ref polinomio (+ posicion_pol indep)) variable)
                           (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1) posicion_vec cont_der cont_izq 1)
                           (vectores polinomio variable vector_coeficiente vector_exponente posicion_pol  posicion_vec cont_der cont_izq (+ indep 1))
                       )
                   )
                   (vectores polinomio variable vector_coeficiente vector_exponente (+ posicion_pol 1)  posicion_vec cont_der cont_izq indep)
             )
           )
       )
)

;----------------------------------------------VERIFICAR POLINOMIO----------------------------------------------
;El programa verifica si lo que entra contiene los elementos de un polinomio

(define (verificar polinomio memoria_variable contador variable_seguida tamaño-vector)
    (if (string? polinomio)  
              ;Aqui vefifica que no hayan dos variables seguidas sin operadores        
        (cond ((> variable_seguida 1)
                  (begin
                     (display "Error: No sea bruto, ingrese un polinomio, no ingrese dos variables seguidas sin signos")
                     (verificar (read) 0 0 0 0)
                  )
              )
              ;Aqui retorna el polinomio, si cumple con los criterios de polinomio
              ((= contador (string-length polinomio))
                   (vectores polinomio (integer->char memoria_variable) (make-vector (+ tamaño-vector 1)) (make-vector (+ tamaño-vector 1)) 0 0 1 1 1)
              )
              ;Aqui entra cuando el caracter evaluado de la cadena es numerico
              ((and (>= (char->integer (string-ref polinomio contador)) 48) (<= (char->integer (string-ref polinomio contador)) 57))
                 (verificar polinomio memoria_variable (+ contador 1) variable_seguida tamaño-vector)                                  
              )
              ;Aqui entra cuando el caracter evaluado de la cadena es una variable                
              ((or (and (>= (char->integer (string-ref polinomio contador)) 65) (<= (char->integer (string-ref polinomio contador)) 90))
                   (and (>= (char->integer (string-ref polinomio contador)) 97) (<= (char->integer (string-ref polinomio contador)) 122)))
                   (if (= memoria_variable 0)
                       (verificar polinomio (char->integer (string-ref polinomio contador)) (+ contador 1) (+ variable_seguida 1) tamaño-vector)
                       (if (= memoria_variable (char->integer (string-ref polinomio contador)) )
                           (verificar polinomio (char->integer (string-ref polinomio contador)) (+ contador 1) (+ variable_seguida 1) tamaño-vector)
                           (begin
                              (display "Error: No sea bruto, ingrese un polinomio, ingrese una sola variable")
                              (verificar (read) 0 0 0 0)
                           )
                       )
                    )
              )
              ;Aqui entra cuando el caracter evaluado de la cadena es un operador de suma o resta     
              ((or (eqv? (string-ref polinomio contador) #\+) (eqv? (string-ref polinomio contador) #\-))
                   (if (= contador (- (string-length polinomio) 1))
                       (verificar polinomio memoria_variable (+ contador 1) 0 tamaño-vector)
                       (if (or (eqv? (string-ref polinomio (+ contador 1)) #\+) (eqv? (string-ref polinomio (+ contador 1)) #\-))
                           (begin
                               (display "Error: No sea bruto, ingrese un polinomio sin dos operadores seguidos")
                               (verificar (read) 0 0 0 0)
                           )
                           (if (= contador 0)
                               (verificar polinomio memoria_variable (+ contador 1) 0 tamaño-vector)
                               (verificar polinomio memoria_variable (+ contador 1) 0 (+ tamaño-vector 1))
                           )
                       )
                   )
              )
              ;Aqui entra cuando no es polinomio
              (else
                  (begin
                     (display "Error: No sea bruto, ingrese un polinomio")
                     (verificar (read) 0 0 0 0)
                  )
              )              
        )
        ;Aqui entra cuando no es ni siquiera cadena
        (begin
            (display "Error: No sea bruto, ingrese un polinomio entre comillas")
            (verificar (read) 0 0 0 0)
        )
    )
)



;------------------------------------------CREAR Y LLENAR VECTOR---------------------------------------------
;Esta funcion llena los vectores
(define (llenar_vector vector contador)
        (if (= contador (vector-length vector))
            vector
            (begin
                  (vector-set! vector contador (read))
                  (llenar_vector vector (+ contador 1))
            )
        )
)

;Este es el llamado a la funciòn que grafica
;(display "Por favor ingrese los valores de los coeficientes del polinomio separados de un espacio, luego presiona enter e ingresa los valores de los exponentes")
;(newline)
;(graficar (llenar_vector (make-vector 3) 0) (llenar_vector (make-vector 3) 0 ) -20 )

;-----------------------------------IGNORAR @ DEL RETROCESO-----------------------------------------------------------------
;Ignorar Retroceso
(define (ignorar_retroceso polinomio_antes contador polinomio_despues)
      (if (= contador (string-length polinomio_antes))
          polinomio_despues
          (if (eqv? (string-ref polinomio_antes contador) #\@)
             (ignorar_retroceso polinomio_antes (+ contador 1) polinomio_despues)
             (ignorar_retroceso polinomio_antes (+ contador 1) (string-append polinomio_despues (make-string 1 (string-ref polinomio_antes contador)) ))   
          )
      )
)  
;-----------------------------------CAJA PARA ESCRIBIR EL POLINOMIO-----------------------------------------------------------------
;Esta función escribe el polinomio en pantalla y lo retorna

(define (escribir teclear pos_x pos_y polinomio contador)
        (define caracter (key-value teclear))
        (cond ( (not (char? caracter)) (inicio pos_x pos_y polinomio contador) )
              ((= (char->integer caracter) 127) (close-viewport ventana))
              ((= (char->integer caracter) 13) (verificar (ignorar_retroceso polinomio 0 (make-string 0)) 0 0 0 0))
              ((= (char->integer caracter) 8)
                  (if (and (= pos_x 25) (= pos_y 608))
                      (inicio pos_x pos_y polinomio contador)    
                      (if (eqv? (string-ref polinomio (- (string-length polinomio) 1 contador)) #\@)
                          (escribir teclear pos_x pos_y polinomio (+ contador 1))
                          (begin
                              (string-set! polinomio (- (string-length polinomio) 1 contador) #\@)
                              ((draw-solid-rectangle ventana) (make-posn  (- pos_x 10) (- pos_y 15)) 10 20 "white")
                              (inicio (- pos_x 10) pos_y polinomio contador)
                          )
                      )
                  )
              )
              ((= pos_x 295) (inicio pos_x pos_y polinomio contador))
              (else
                  (begin
                      ((draw-string ventana)(make-posn pos_x pos_y)(make-string 1 caracter) "black")
                      (inicio (+ pos_x 10) pos_y (string-append polinomio (make-string 1 caracter)) 0)
                  )
              )
        )
)



;------------------------------------------------------VALIDAR MOUSE-----------------------------------------------
;Esta funcion valida que el click dado sobre el cuadro cerrar
(define (validar_click click pos_x pos_y polinomio contador)
        (define limite_1 (make-posn 510 592))
        (define limite_2 (make-posn 570 617))
        (define posicion_click (mouse-click-posn click))
        (if (and (<= (posn-x posicion_click) (posn-x limite_2)) (<= (posn-y posicion_click) (posn-y limite_2))
                 (>= (posn-x posicion_click) (posn-x limite_1)) (>= (posn-y posicion_click) (posn-y limite_1)))
            (begin
                 (display "Gracias Por Interactuar Con Nuestro Graficador")
                 (close-viewport ventana)
             )
            (inicio pos_x pos_y polinomio contador)
        )
)
;-----------------------------------------------INICIO-----------------------------------------------------
(define (inicio pos_x pos_y polinomio contador)
        (define click  (ready-mouse-click ventana))
        (define teclear (ready-key-press ventana))
        (if  (and (not teclear) (not click))
             (inicio pos_x pos_y polinomio contador)
             (cond ((not(not teclear)) (escribir teclear pos_x pos_y polinomio contador))
                   ((not(not click)) (validar_click click pos_x pos_y polinomio contador))
                   (else (inicio pos_x pos_y polinomio contador))
             )
        )                   
)
(inicio 25 608 (make-string 1 #\@) 0)