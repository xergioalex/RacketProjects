(require (lib "graphics.ss" "graphics"))
(open-graphics) 
(define w (open-viewport "Paint" 600 600))


(define (validar c n)
  (define polapiz (make-posn 0 0))
  (define polapiz2 (make-posn 82 70))
  (define poborra (make-posn 0 70))
  (define poborra2 (make-posn 82 139))
  (define posalir (make-posn 0 139))
  (define posalir2 (make-posn 82 208))
  (define pobrocha (make-posn 0 208))
  (define pobrocha2 (make-posn 82 284))
  (define po (mouse-click-posn c))
  (cond
      
      [(and (and (<= (posn-x po) (posn-x pobrocha2)) (<= (posn-y po) (posn-y pobrocha2))) (and (>= (posn-x po) (posn-x pobrocha)) (>= (posn-y po) (posn-y pobrocha)))) (begin (esperar 3))
   ]
    
     [(and (and (<= (posn-x po) (posn-x polapiz2)) (<= (posn-y po) (posn-y polapiz2))) (and (>= (posn-x po) (posn-x polapiz)) (>= (posn-y po) (posn-y polapiz)))) (begin (esperar 0))
   ]
    
    [(and (and (<= (posn-x po) (posn-x poborra2)) (<= (posn-y po) (posn-y poborra2))) (and (>= (posn-x po) (posn-x poborra)) (>= (posn-y po) (posn-y poborra)))) (begin (esperar 1))
   ]
    
    
  [(and (and (<= (posn-x po) (posn-x posalir2)) (<= (posn-y po) (posn-y posalir2))) (and (>= (posn-x po) (posn-x posalir)) (>= (posn-y po) (posn-y posalir)))) (begin (display "suerte es que le digo")
                                           (close-viewport w)
                                           (close-graphics))
   ]
  [ (and(> (posn-x po)  82) (= n 0))  (begin (lapiz n))]
 [ (and(> (posn-x po)  82) (= n 1))  (begin (borrador n))]
 [(and (> (posn-x po) 82) (= n 3)) (begin (brocha n))]
  [else (esperar n)]
  )
  )
  

(define (borrador n)
  (define c (ready-mouse-release w))
  (define po (query-mouse-posn w))
  (cond 
   [(equal? c #f) (begin (cond 
                           [ (> (posn-x po) 82) (begin ((clear-solid-ellipse w) po 10 10) (borrador n))]  
                           [else (esperar n)]
                           ))]
   [else (esperar n)]))
  

(define (brocha n)
  
  (define c (ready-mouse-release w))
  (define po (query-mouse-posn w))
  
  (cond 
    [(equal? c #f)(begin 
                    (cond
                     [ (> (posn-x po) 82) (begin ((draw-solid-ellipse w) po 10 10 "red")
                         (brocha n))]
                      [else (esperar n)]
                     ))
                  
                  ]

    [else (esperar n)]
    )
  )
    
    
    
(define (lapiz n)
  
  (define c (ready-mouse-release w))
  (define po (query-mouse-posn w))
  
  (cond 
    [(equal? c #f)(begin 
                    (cond
                     [ (> (posn-x po) 82) (begin ((draw-pixel w) po "red")
                         (lapiz n))]
                      [else (esperar n)]
                     ))
                  
                  ]

    [else (esperar n)]
    )
  )



(define (esperar n)
  (define c (ready-mouse-click w))
  (cond 
    [(equal? c #f) (begin 
                     (viewport-flush-input w)
                     (esperar n))]
    [else(validar c n)]
    ))




(define (panelsito)
  ((draw-rectangle w) (make-posn 0 0) 82 70 "black")
  ((draw-rectangle w) (make-posn 0 0 ) 82 139 "black")
  ((draw-rectangle w) (make-posn 0 0 ) 82 208 "black")
  ((draw-rectangle w) (make-posn 0 0) 82 284 "black")
  ((draw-string w) (make-posn 23 35) "lapiz")
  ((draw-string w) (make-posn 10 110) "borrador")
  ((draw-string w) (make-posn 23 173) "salir")
  ((draw-string w) (make-posn 23 243) "brocha")
  (esperar 0)
  )

(panelsito)