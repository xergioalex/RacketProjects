(define (es_primo x )
        (if (and (integer? x) (> x 0))
            (evaluar_primo x 2)
            (display "parametros equivocados")
        )
)

(define (evaluar_primo x y)
            (if (> y (/ x 2))
                #t
                (if (=(remainder x y)0)
                    #f
                    (evaluar_primo x (+ y 1))
                )
            )
)

(define (ver_primos lim1 lim2)
        (if (and (integer? lim1) (> lim1 0)(integer? lim2)(> lim2 0)(> lim2 lim1))
            (ver_primo lim1 lim2 1)
            ("parametros equivocados")
        )
)
(define (ver_primo lim1 lim2 contador)
         (if (<= lim1 lim2)
             (begin
                   (if (equal? (es_primo lim1 ) #t)
                        (begin
                             (display lim1)
                             (display " ")
                             
                         )
                      
                   )
               (ver_primo (+ lim1 1) lim2 contador )
              )
          (display contador)   
         )
)


(ver_primos 1000 2000 )