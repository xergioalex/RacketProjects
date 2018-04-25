;(define (potencia x y)
  ;      (if (>= y 0)
 ;           (* (potencia x (- y 1)) x)
 ;       )
;)

;(potencia 3 4)

(define (espacios x y)
        (if (<= x y)
            (begin
                  (display " ")             
                  (espacios (+ x 1) y)
            )
        )
)

(define (mostrar_en_pantalla x y)
        (if (<= x y)
            (begin
                  (display "A")             
                  (mostrar_en_pantalla (+ x 1) y)
            )
        )
)

(define (piramide x y)
        (if (<= 1 y)
            (begin
                  (espacios 1 y)
                  (mostrar_en_pantalla 1  x)
                  (newline)
                  (piramide (+ x 1) (- y 1))
            )
        )
)

(piramide 1 (read))
            

