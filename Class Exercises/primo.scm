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


(es_primo  6 )