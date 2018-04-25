; 
;Matriz Usando Listas 
; 
 
(define (Crear-MatrizFila Columnas ValorInicio) 
  (if (> Columnas 0) 
      (append (list ValorInicio) (Crear-MatrizFila (- Columnas 1) ValorInicio)) 
      null)) 
 
(define (Crear-MatrizLista Columnas Filas ValorInicio) 
  (if (> Filas 0) 
      (append (list (Crear-MatrizFila Columnas ValorInicio)) (Crear-MatrizLista Columnas (- Filas 1)    ValorInicio)) 
        null))
(define (MatrizLista-ref MatrizL Columnas Filas)  
  (list-ref (list-ref MatrizL Filas) Columnas)) 
 
(define (MatrizLista-Columna-set! ListaFila PosicionCol cont Valor) 
  (if (< cont PosicionCol) 
      (MatrizLista-Columna-set! (cdr ListaFila) PosicionCol (+ cont 1) Valor)  
      (set-car! ListaFila Valor))) 
       
(define (MatrizLista-set! MatrizL PosicionCol PosicionFil Valor) 
  (MatrizLista-Columna-set! (list-ref MatrizL PosicionFil) PosicionCol 0 Valor))  
 
(define (MatrizLista-Mostrar-Filas MatrizL Cont)  
  (if (< Cont (length MatrizL)) 
      (begin 
         (newline) 
         (display (list-ref  MatrizL Cont)) 
         (MatrizLista-Mostrar-Filas MatrizL (+ Cont 1))) 
      (newline))) 
(define (MatrizLista-Mostrar MatrizL)  
  (MatrizLista-Mostrar-Filas MatrizL 0)) 
 
; 
;Prueba de Manejo de un Matriz usando Listas 
; 
(display "Matriz Con Listas") 
(newline) 
(define m (Crear-MatrizLista 3 4 0)) 
(MatrizLista-set! m 0 0 1) 
(MatrizLista-set! m 1 1 1) 
(MatrizLista-set! m 2 1 9) 
(MatrizLista-set! m 2 3 8) 
(MatrizLista-Mostrar m)  
 
 