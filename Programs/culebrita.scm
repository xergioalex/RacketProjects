(require (lib "draw.ss" "htdp"))

;==================================================================================================
;<=================VENTANA FINAL==================>


(define (final)
  (begin
    (stop)
    (start 300 300)
    (draw-solid-string (make-posn 0 60) "                           perdiste ")
    (draw-solid-string (make-posn 0 100) "                     tu puntaje fue de:")
    (draw-solid-string (make-posn 140 140) (number->string (vector-ref cantidad-cuadros 2)))
    (sleep 5)
    (stop)
    (exit)
    )
  )


;==================================================vectores============================================================
;este almacena la cantidad de cuadros de la serpiente (vector cero), almacena la posicion de la comida (vector 1 ) y  almacena el puntaje (vector 2), ademas almacena en un temporal la cantidad de cuadros para poder usarla despues al redefinirla (vector 3), almacena el tiempo en la casilla 4, a su vez en la 5 almacena un temporal del tiempo, el cual se hace operable, ademas en la 6 almacena un temporal del puntaje.
(define cantidad-cuadros (make-vector 7 0))

;______________________________________ los obstaculos __________________________________

(define obs1(make-vector 50 0))
(define obs2(make-vector 49 0))
(define obs3(make-vector 49 0))
(define obs4(make-vector 49 0))




 

;este almacenala pósicion de cada cuadro de la serpiente
(define posiciones (make-vector 500 0))
(define posiciones2 (make-vector (vector-ref cantidad-cuadros 0) 0))

;=======================================redefinicion de los vectores===================================================
; (vector-set! posiciones 0 (make-posn 30 0))
; (vector-set! posiciones 1 (make-posn 24 0))
; (vector-set! posiciones 2 (make-posn 18 0))
; (vector-set! posiciones 3 (make-posn 12 0))
; (vector-set! posiciones 4 (make-posn 6 0))


(vector-set! cantidad-cuadros 1 (make-posn 80 365))
(vector-set! cantidad-cuadros 0 5)
(vector-set! cantidad-cuadros 4 0.05)

(define (primera m)
  (if (>= m 0)
      (begin
        (vector-set! posiciones m (make-posn (* m 6) 340))
        (primera (- m 1))
        )
      )
  )
   

(define (obstaculosx vector m x y)
  (if (>= m 0)
      (begin
        (vector-set! vector m (make-posn x y))
        (obstaculosx vector (- m 1) (+ x 5) y)
        )
      )
  )

(define (obstaculosy vector m x y)
  (if (>= m 0)
      (begin
        (vector-set! vector m (make-posn x y))
        (obstaculosy vector (- m 1) x (+ y 5))
        )
      )
  )

(primera 12)


;(obstaculosx obs2 (- m 1))
;(obstaculosy obs3 (- m 1))
;(obstaculosy obs4 (- m 1))

;________________________________________EL MUÑECO____________________________________
(define (cabra r color)
  (begin
    (draw-solid-disk (make-posn (- (posn-x r) 6) (posn-y r)) 2 color)
    (draw-solid-disk (make-posn (posn-x r) (posn-y r)) 4 color)
    (draw-solid-disk (make-posn (+ (posn-x r) 4) (posn-y r)) 4 color)
    (draw-solid-disk (make-posn (+ (posn-x r) 2) (- (posn-y r) 2)) 2 color)
    (draw-solid-line (make-posn (posn-x r) (+ (posn-y r) 2)) (make-posn (- (posn-x r) 1) (+ (posn-y r) 6)) color)
    (draw-solid-line (make-posn (+ (posn-x r) 4) (+ (posn-y r) 2)) (make-posn (+ (posn-x r) 5) (+ (posn-y r) 6)) color)
    )
  )

(define (borrar-cabra r color)
  (begin
    (draw-solid-disk (make-posn (- (posn-x r) 6) (posn-y r)) 2 color)
    (draw-solid-disk (make-posn (posn-x r) (posn-y r)) 4 color)
    (draw-solid-disk (make-posn (+ (posn-x r) 4) (posn-y r)) 4 color)
    (draw-solid-disk (make-posn (+ (posn-x r) 2) (- (posn-y r) 2)) 2 color)
    (draw-solid-line (make-posn (posn-x r) (+ (posn-y r) 2)) (make-posn (- (posn-x r) 1) (+ (posn-y r) 6)) color)
    (draw-solid-line (make-posn (+ (posn-x r) 4) (+ (posn-y r) 2)) (make-posn (+ (posn-x r) 5) (+ (posn-y r) 6)) color)
    )
  )
;_______________________________________________________________________________________

;===========================================movimiento recursivo=============================================
;_______________________________________________evaluacion____________________________________________________

(define (evaluacion n)
  (cond ((> n 1)
         (if (equal? #t (not (equal? (vector-ref posiciones 0) (vector-ref posiciones n))))
           (evaluacion (- n 1))
           (begin
;             (stop)
             (final)))
          )
        ((<= n 3) (equal? #t (not (equal? (vector-ref posiciones 0) (vector-ref posiciones (+ n 1))))))
        (else (display "no hay problema")
              )
        )
  )

;____________________________________________dibujo recursivo________________________________________________
;(> (posn-x (vector-ref posiciones m)) 0) (> (posn-y (vector-ref posiciones m)) 0)

; m es el numero de cuadros de la serpiente, pero debe tener -1 para funcionar
(define (dibujado-rec m)
  (if (>= m 0)
      (if (and (< (posn-x (vector-ref posiciones m)) 924) (< (posn-y (vector-ref posiciones m)) 716)
               (> (posn-x (vector-ref posiciones m)) -1) (> (posn-y (vector-ref posiciones m)) -10))
      (begin
        (draw-solid-rect (vector-ref posiciones m) 5 5 'white)
;        (sleep 0.5)
        (dibujado-rec (- m 1))
        )
      (begin
     ;        (stop)
             (final))
      )
      )
  )
     
;(dibujado-rec (- (vector-ref cantidad-cuadros 0) 1))
;_____________________________________________ paredes _____________________________________________________-
(define (paredes vector n)
  (draw-solid-rect (make-posn 250 0) 5 100 'black)
  (draw-solid-rect (make-posn 0 500) 200 5 'black)
  (draw-solid-rect (make-posn 500 0) 5 445 'black)
  (draw-solid-rect (make-posn 724 528) 200 5 'black)
  (draw-solid-rect (make-posn 445 400) 80 80 'black)
  (draw-solid-rect (make-posn 300 724) 5 200 'black)
  (cond ((> n 1)
         (if
          (or
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 255) (> (posn-x (vector-ref posiciones 0)) 250))
            (and (< (posn-y (vector-ref posiciones 0)) 100) (> (posn-y (vector-ref posiciones 0)) 0)))
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 100) (> (posn-x (vector-ref posiciones 0)) 0))
            (and (< (posn-y (vector-ref posiciones 0)) 500) (> (posn-y (vector-ref posiciones 0)) 495)))
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 500) (> (posn-x (vector-ref posiciones 0)) 495))
            (and (< (posn-y (vector-ref posiciones 0)) 445) (> (posn-y (vector-ref posiciones 0)) 0)))
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 924) (> (posn-x (vector-ref posiciones 0)) 724))
            (and (< (posn-y (vector-ref posiciones 0)) 523) (> (posn-y (vector-ref posiciones 0)) 528)))
;cuadrado          
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 525) (> (posn-x (vector-ref posiciones 0)) 445))
            (and (< (posn-y (vector-ref posiciones 0)) 480) (> (posn-y (vector-ref posiciones 0)) 400)))
           (and
            (and (< (posn-x (vector-ref posiciones 0)) 305) (> (posn-x (vector-ref posiciones 0)) 300))
            (and (< (posn-y (vector-ref posiciones 0)) 924) (> (posn-y (vector-ref posiciones 0)) 724)))
          
           )
      

      
            
            (final)
           (paredes vector (- n 1)))
          )
              )
        )
 

(define (llamados)
        (begin
          (paredes obs1 (- (vector-length obs1) 1))
          )
        )

;___________________________________________borrado recursivo________________________________________________

(define (borrado m)
  (if (> m 0)
        (draw-solid-rect (vector-ref posiciones m) 5 5 'darkgreen)
        )
  )


;__________________________________________movimiento recursivo_________________________________________________

(define (mov-rec m funcion)
      (begin
        (dibujado-rec m)
        ;-------------
        (llamados)
        (sleep (vector-ref cantidad-cuadros 4))
        (teclado-entrada m funcion)
        (borrado m)
        (puntaje (vector-ref cantidad-cuadros 2))
        (evaluacion m)
        (comida funcion)
        (cabra (vector-ref cantidad-cuadros 1) 'pink)
        (funcion m)
        (mov-rec m funcion)
        )
  )
(define (mov-reca m funcion)
      (begin
        (draw-solid-rect (make-posn 0 0) 924 768 'darkgreen)
        (dibujado-rec m)
        (sleep (vector-ref cantidad-cuadros 4))
        (teclado-entrada m funcion)
        (borrado m)
        ;-------------------------
        (funcion m)
        (dibujado-rec m)
        (sleep (vector-ref cantidad-cuadros 4))
        (teclado-entrada m funcion)
        (borrado m)
        (funcion m)
        (dibujado-rec m)
        (sleep (vector-ref cantidad-cuadros 4))
        (teclado-entrada m funcion)
        (borrado m)
        (funcion m)
        (mov-rec m funcion)
        )
  )


;======================================= ||||||||   ||||||||   |||||||| ============================================
;======================================= ||    ||      ||      ||    || ============================================
;======================================= || °° ||      ||      || °° || ============================================
;======================================= ||    ||      ||      ||    || ============================================
;======================================= ||||||||  ||||||      |||||||| ============================================

;n es el numero de casillas que redefinira en un vector, n esta directamente relacionado con la cantidad de cuadros.
 
(define (derecha n)
  (if (> n 0)
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones (- n 1)))
                                         (posn-y (vector-ref posiciones (- n 1)))))
        (derecha (- n 1))
        )
      (begin
        (vector-set! posiciones n (make-posn (+ (posn-x (vector-ref posiciones n)) 6)
                                             (posn-y (vector-ref posiciones n))))
        )
      )
  )
;____________________________________________________________________________________________________________

(define (izquierda n)
  (if (> n 0)
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones (- n 1)))
                                         (posn-y (vector-ref posiciones (- n 1)))))
        (izquierda (- n 1))
        )
      (begin
        (vector-set! posiciones n (make-posn (- (posn-x (vector-ref posiciones n)) 6)
                                             (posn-y (vector-ref posiciones n))))
        )
      )
  )
;____________________________________________________________________________________________________________

(define (abajo n)
  (if (> n 0)
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones (- n 1)))
                                         (posn-y (vector-ref posiciones (- n 1)))))
        (abajo (- n 1))
        )
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones n))
                                             (+ (posn-y (vector-ref posiciones n)) 6)))
        )
      )
  )
;____________________________________________________________________________________________________________

(define (arriba n)
  (if (> n 0)
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones (- n 1)))
                                         (posn-y (vector-ref posiciones (- n 1)))))
        (arriba (- n 1))
        )
      (begin
        (vector-set! posiciones n (make-posn (posn-x (vector-ref posiciones n))
                                           (- (posn-y (vector-ref posiciones n)) 6)))
        )
      )
  )


;============================================= teclado =========================================================

(define (movimiento-a a-key-event m)
  (cond ((symbol=? a-key-event 'up) (mov-rec m arriba))
        ((symbol=? a-key-event 'down) (mov-rec m abajo))
        )
  )
;_______________________________________________________________________________________________________________

(define (movimiento-b a-key-event m)
  (cond ((symbol=? a-key-event 'left) (mov-rec m izquierda))
        ((symbol=? a-key-event 'right) (mov-rec m derecha))
        )
  )
;_______________________________________________________________________________________________________________

(define (decode-user-input-der a-key-event m)
  (if (symbol? a-key-event)
      (cond
        ((or (symbol=? a-key-event 'up) (symbol=? a-key-event 'down)) (movimiento-a a-key-event m))
        )
    (if (boolean? a-key-event)
        (if (boolean=? a-key-event #f) void)
    ))
  )


(define (decode-user-input-izq a-key-event m)
  (if (symbol? a-key-event)
      (cond
        ((or (symbol=? a-key-event 'up) (symbol=? a-key-event 'down)) (movimiento-a a-key-event m))
        )
    (if (boolean? a-key-event)
        (if (boolean=? a-key-event #f) void)
    ))
  )


(define (decode-user-input-arr a-key-event m)
  (if (symbol? a-key-event)
      (cond
        ((or (symbol=? a-key-event 'left) (symbol=? a-key-event 'right)) (movimiento-b a-key-event m))
        )
    (if (boolean? a-key-event)
        (if (boolean=? a-key-event #f) void)
    ))
  )

(define (decode-user-input-ab a-key-event m)
  (if (symbol? a-key-event)
      (cond
        ((or (symbol=? a-key-event 'left) (symbol=? a-key-event 'right)) (movimiento-b a-key-event m))
        )
    (if (boolean? a-key-event)
        (if (boolean=? a-key-event #f) void)
    ))
  )

;_______________________________________________________________________________________________________________
(define (teclado-entrada m funcion)
  (cond ((equal? funcion derecha)
           (decode-user-input-der (get-key-event) m))
        ((equal? funcion izquierda)
           (decode-user-input-izq (get-key-event) m))
        ((equal? funcion arriba)
           (decode-user-input-arr (get-key-event) m))
        ((equal? funcion abajo)
           (decode-user-input-ab (get-key-event) m))
        )
  )
       
;_______________________________________________________________________________________________________________
;================================================puntaje========================================================

(define (puntaje n)
  (begin
    (clear-solid-rect (make-posn 950 64) 50 50 )
    (clear-solid-rect (make-posn 950 339) 50 50 )
    (clear-solid-rect (make-posn 950 538) 50 50 )
    (draw-solid-line (make-posn 924 0) (make-posn 924 768) 'black)
    (draw-solid-string (make-posn 932 60) "PUNTAJE" )
    (draw-solid-string (make-posn 934 60) "PUNTAJE" )
    (draw-solid-string (make-posn 930 300) "NUMERO")
    (draw-solid-string (make-posn 932 300) "NUMERO")
    (draw-solid-string (make-posn 930 315) "    DE")
    (draw-solid-string (make-posn 932 315) "    DE")
    (draw-solid-string (make-posn 930 330) "CUADROS")
    (draw-solid-string (make-posn 932 330) "CUADROS")
    (draw-solid-string (make-posn 932 530) "VELOCIDAD")
    (draw-solid-string (make-posn 954 355) (number->string (vector-ref cantidad-cuadros 0)))
    (draw-solid-string (make-posn 955 75) (number->string (vector-ref cantidad-cuadros 2)))
    (draw-solid-string (make-posn 955 550) (number->string (+ (vector-ref cantidad-cuadros 4) 1)))
    ))
;===============================================comida==========================================================

 

(define (comida funcion)
  (if (and
       (< (posn-x (vector-ref posiciones 0)) (+ (posn-x (vector-ref cantidad-cuadros 1)) 6))
       (> (posn-x (vector-ref posiciones 0)) (- (posn-x (vector-ref cantidad-cuadros 1)) 10))
       (< (posn-y (vector-ref posiciones 0)) (+ (posn-y (vector-ref cantidad-cuadros 1)) 8))
       (> (posn-y (vector-ref posiciones 0)) (- (posn-y (vector-ref cantidad-cuadros 1)) 10))
       )
      (begin
        (vector-set! cantidad-cuadros 3 (+ (vector-ref cantidad-cuadros 0) 1))
        (vector-set! cantidad-cuadros 6 (- (+ (- (* (+ (vector-ref cantidad-cuadros 0) 1) 10) 50)
                                           (- (*(quotient (vector-ref cantidad-cuadros 0) 5) 50) 10)) 40))
        (vector-set! cantidad-cuadros 2 (vector-ref cantidad-cuadros 6))
        (vector-set! cantidad-cuadros 0 (vector-ref cantidad-cuadros 3))
        (vector-set! cantidad-cuadros 5 (vector-ref cantidad-cuadros 4))
        (cond
          ((and (positive? (vector-ref cantidad-cuadros 4))
                (> (vector-ref cantidad-cuadros 0) 0)
                (= (remainder (vector-ref cantidad-cuadros 0) 5) 0)
                (< (vector-ref cantidad-cuadros 0) 30))
           (vector-set! cantidad-cuadros 4 (- (vector-ref cantidad-cuadros 4) 0.01)))
          ((> (vector-ref cantidad-cuadros 0) 29) (vector-set! cantidad-cuadros 4 0))
          (else void))
        (vector-set! posiciones (vector-ref cantidad-cuadros 0)
                     (make-posn
                      (posn-x (vector-ref posiciones (- (vector-ref cantidad-cuadros 0) 1)))
                      (posn-y (vector-ref posiciones (- (vector-ref cantidad-cuadros 0) 1)))
                      )
                     )
        (borrar-cabra (vector-ref cantidad-cuadros 1) 'darkgreen)
        (vector-set! cantidad-cuadros 1 (make-posn (random 900) (random 650)))
        (mov-rec (- (vector-ref cantidad-cuadros 0) 1) funcion)
        )
      void
      )
  )

;============================================entrada==========================================


; (require (lib "graphics.ss" "graphics"))
; (open-graphics)
; (define hungry (open-viewport "Hungry Dragon" 1024 768 ))
; (((draw-pixmap-posn "true-dragon.bmp" 'bmp) hungry) (make-posn 0 0) "black")
; (define (loader n x y)
;   (if (>= n 0)
;       (begin
;         ((draw-solid-rectangle hungry) (make-posn x y) 19 15 (make-rgb 0 0 1))
;         (sleep 0.3)
;         (loader (- n 1) (+ x 9) y)
;         )
;       ((draw-solid-rectangle hungry) (make-posn x y) 15 15 (make-rgb 0 0 1))
;       )
;   )
; (define (inicio)
;   (begin
;     (loader 44 300 606)
;     (sleep 0.05)
;     (close-viewport hungry)
;     (close-graphics)
;     )
;   )
; (inicio)


(require (lib "graphics.ss" "graphics"))
(open-graphics)
(define hungry (open-viewport "Hungry Dragon" 1024 768 ))
;(((draw-pixmap-posn "true-dragon.bmp" 'bmp) hungry) (make-posn 0 0) "black")
(define (loader n x y)
  (if (>= n 0)
      (begin
        ((draw-solid-rectangle hungry) (make-posn x y) 19 15 (make-rgb 0 0 1))
        (sleep 0.03)
        (loader (- n 1) (+ x 9) y)
        )
      ((draw-solid-rectangle hungry) (make-posn x y) 15 15 (make-rgb 0 0 1))
      )
  )
(define (inicio)
  (begin
    (loader 44 300 606)
    (sleep 0.03)
    (close-viewport hungry)
    (close-graphics)
    )
  )


;----------------------------------------------------------------------------------------------




(inicio)


(start 1024 768)





(mov-reca (- (vector-ref cantidad-cuadros 0) 1) derecha)



