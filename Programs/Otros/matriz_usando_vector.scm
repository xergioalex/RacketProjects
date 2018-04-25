; 
; Matriz Usando Estructura con un Vector que representa una Matriz 
; Se manejo un solo vector que se comportará como una matriz (Cómo?) 
; 
(define-struct MatrizLineal (Vector NroColumnas NroFilas))  
 
(define (CrearMatrizLineal Columnas Filas ValorInicial) 
  (make-MatrizLineal (make-vector (* Columnas Filas) ValorInicial) Columnas Filas)) 
 
(define (CrearMatrizCuadradaLineal Tamaño ValorInicial) 
  (CrearMatrizLineal Tamaño Tamaño ValorInicial))  
 
(define (MatrizLineal-Filas MatrizL) 
  (if (MatrizLineal? MatrizL) 
      (MatrizLineal-NroFilas MatrizL)  
      (display "No es Una  Matriz Lineal"))) 

 (define (MatrizLineal-Columnas MatrizL)  
  (if (MatrizLineal? MatrizL) 
      (MatrizLineal-NroColumnas MatrizL)  
      (display "No es Una  Matriz Lineal"))) 
 
(define (MatrizLineal-Matriz MatrizL) 
  (if (MatrizLineal? MatrizL) 
      (MatrizLineal-Vector MatrizL)  
      (display "No es Una  Matriz Lineal"))) 
 
(define (MatrizLineal-set! MatrizL posicionCol posicionFil NuevoValor) 
    (if (MatrizLineal? MatrizL) 
        (vector-set! (MatrizLineal-Vector MatrizL) (+ (* posicionFil (MatrizLineal-NroColumnas MatrizL)) posicionCol) NuevoValor) 
        (display "No es Una  Matriz Lineal")))
(define (MatrizLineal-ref MatrizL posicionCol posicionFil) 
    (if (MatrizLineal? MatrizL)   
        (vector-ref (MatrizLineal-Vector MatrizL) (+ (* posicionFil (MatrizLineal-NroColumnas  MatrizL)) posicionCol))  
        (display "No es Una  Matriz Lineal")))
(define (Matriz-Lineal-Completa MatrizL Col Fil)  
    (if (< Fil (MatrizLineal-Filas MatrizL)) 
      (begin 
        (if (< Col (MatrizLineal-Columnas MatrizL))  
            (begin 
              (display (MatrizLineal-ref MatrizL Col Fil))   
              (display "  ") 
              (Matriz-Lineal-Completa MatrizL (+ Col 1) Fil)) 
            (begin 
              (newline)  
              (Matriz-Lineal-Completa MatrizL 0 (+ Fil 1) )))))) 
 
(define (MatrizLineal-Mostrar MatrizL) 
  (if (MatrizLineal? MatrizL) 
      (Matriz-Lineal-Completa MatrizL 0 0)  
      (display "No es Una  Matriz Lineal")))  
 
; 
;Prueba de Manejo de un Matriz usando Estructuras 
; 
(display "Matriz Con estructuras") 
(newline) 
(define ML (CrearMatrizLineal 3 4 0)) 
(MatrizLineal-set! ML 0 0 1) 
(MatrizLineal-set! ML 1 1 1) 
(MatrizLineal-set! ML 2 1 9) 
(MatrizLineal-set! ML 2 3 8) 
(MatrizLineal-Mostrar ML)  


 