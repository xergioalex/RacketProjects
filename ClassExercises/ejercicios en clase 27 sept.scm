(define (leer_valor)
   (begin
     (display "Ingrese un número entero positivo")
     (menor_nro (read)(expt 10 100000)))
)
  

(define (menor_nro N menor)
  (if (> N 0)
      (evaluar_menor N menor (read)) 
      menor
   )
  )

(define (evaluar_menor N menor valor)
  (if (< valor menor)
      (menor_nro (- N 1) valor)
      (menor_nro (- N 1) menor)
      ))
      
      
(leer_valor)
