(require (lib "graphics.ss" "graphics"))  ;; DrScheme version 204
(open-graphics)

;; La ventana grafica
;; dimensiones de la ventana
(define ancho 1100)
(define alto  800)

(define ventana (open-viewport "El mundo de las tortugas" ancho alto))

;; El constructor de tortugas
(define (cons-tortuga color)
  ;; variables auxiliares
  (define pi (acos -1.0))
  
  ;; atributos de la tortuga (variables de estado)
  (define x (/ ancho 2.0))   ; x-posicion
  (define y (/ alto 2.0))   ; y-posicion
  (define posicion (make-posn x y)) ; posicion
  (define a 0.0)   ; angulo
  (define pinta #t) ; si #t (cierto) deja el rastro al avanzar
  
  ;; procedimientos auxiliares
  (define (grados->radianes grados)
    (/ (* pi grados) 180.0))
  
  (define (radianes->grados radianes)
    (/ (* radianes 180.0) pi))
  
  (define (pinta-segmento pos1 pos2)
    ((draw-line ventana) pos1 pos2 color))
  
  
  ;; acciones sobre la tortuga
  (define (avanza d)
    (let ((posicion-inicial posicion)
          (ang (grados->radianes a)))
      (set! x (+ x (* d (cos ang))))
      (set! y (+ y (* d (sin ang))))
      (set! posicion (make-posn x y))
      (if pinta (pinta-segmento posicion-inicial posicion)))
    (void))
  
  (define (obten-x-pos)
    x)
  
  (define (obten-y-pos)
    y)
  
  (define (gira grados)
    (let ((nuevo-a (+ a grados)))
      (if (< nuevo-a 360.0)
          (set! a nuevo-a)
          (set! a (- nuevo-a 360.0))))
    (void))
  
  (define (derecha grados)
    (gira grados))
  
  (define (izquierda grados)
    (gira (- grados))
    (void))
  
  (define (mira-a otra-tortuga)
    (set! a (radianes->grados (atan (- (otra-tortuga 'obten-y-pos) y)
                                    (- (otra-tortuga 'obten-x-pos) x)))))
  
  (define (mira-al-raton)
    (let ((posn-raton (query-mouse-posn ventana)))
      (set! a (radianes->grados (atan (- (posn-y posn-raton) y)
                                      (- (posn-x posn-raton) x))))))
  (define (sube)
    (if pinta
        (set! pinta #f) ;; La tortuga no deja el rastro
        (error "La tortuga ya esta arriba")))
  
  (define (baja)
    (if (not pinta)
        (set! pinta #t) ;; La tortuga deja el rastro
        (error "Ia tortuga ya esta abajo")))
  
  (define (vete-a punto) ;; punto es (x y)
    (if pinta
        (let ((posicion-inicial posicion))
          (set! x (first punto))
          (set! y (second punto))
          (set! posicion (make-posn x y))
          ((draw-line ventana) posicion-inicial posicion color))
        (begin
          (set! x (first punto))
          (set! y (second punto))
          (set! posicion (make-posn x y))))
    (void)
    )
  
  (define (acasa)
    (set! x (/ ancho 2.0)) 
    (set! y (/ alto 2.0))   
    (set! posicion (make-posn x y)) 
    (set! a 0.0)  
    (set! pinta #t) )
  
  (define (reinicia)
    ((clear-viewport ventana))
    (acasa))
  
  ;;;;; Se exporta:
  (define tabla (make-hash-table))
  
  (define (pon a b) 
    (hash-table-put! tabla a b))
  
  (define (obten clave) 
    (hash-table-get tabla clave (lambda () #f)))
  
  (define (despacha m . args)
    (let (
          (accion (obten m))
          )
      (if (not m) (error "Mensaje desconocido" m))
      (apply accion args)))
  
  (pon 'avanza avanza)
  (pon 'obten-x-pos obten-x-pos)
  (pon 'obten-y-pos obten-y-pos)
  (pon 'gira gira)
  (pon 'derecha derecha)
  (pon 'izquierda izquierda)
  (pon 'mira-a mira-a)
  (pon 'mira-al-raton mira-al-raton)
  (pon 'sube sube)
  (pon 'baja baja)
  (pon 'vete-a vete-a)
  (pon 'acasa acasa)
  (pon 'reinicia reinicia)
  
  despacha
  ) ;; FIN del constructor de tortugas

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejemplos

;; Fractales

(define (dragon tam nivel)
  ((draw-viewport ventana) "black")
  (let ((t (cons-tortuga "red")))
    (t 'sube)
    (t 'derecha 90)
    (t 'avanza 200)
    (t 'izquierda 90)
    (t 'baja)
    (dragoni t tam nivel)))

(define (dragoni t tam nivel)
  (if (= nivel 0)
      (t 'avanza tam)
      (begin
        (dragoni t tam (- nivel 1))
        (t 'izquierda 90)
        (dragond t tam (- nivel 1)))))

(define (dragond t tam nivel)
  (if (= nivel 0)
      (t 'avanza tam)
      (begin
        (dragoni t tam (- nivel 1))
        (t 'derecha 90)
        (dragond t tam (- nivel 1)))))

;(dragon 1 18)

;; Copo de nieve

(define (copo talla nivel)
  (let ((t (cons-tortuga "red")))
    (do ((i 1 (+ 1 i)))
      ((> i 3) (void))
      (copo-aux t talla nivel)
      (t 'derecha 120))))

(define (copo-aux t talla nivel)
  (if (= nivel 0)
      (t 'avanza talla)
      (begin
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (t 'izquierda 60)
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (t 'derecha 120)
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (t 'izquierda 60)
        (copo-aux t (/ talla 3.0) (- nivel 1)))))




;(copo 400 8)


(define (copos n)
  (do ((i 1(+ i 1)))
    ((> i n) (void))
    (sleep 1)
    (copo 400 i)))


;(copos 6)


(define (pol-reg long-lado n-lados)
  (let ((t (cons-tortuga "black")))
    (t 'sube)
    (t 'vete-a (list (/ ancho 2.0) 10))
    (t 'baja)
    (do ((i 1 (+ i 1)))
      ((> i n-lados) (void))
      (t 'avanza long-lado)
      (t 'derecha (/ 360.0 n-lados)))))

;(pol-reg 0.01 16000)

(define (ccurva tam niv)
  ((draw-viewport ventana) "black")
  (let ((t (cons-tortuga "yellow")))
    (ccurva-aux t tam niv)))

(define (ccurva-aux t tam niv)
  (if (= niv 0)
      (t 'avanza tam)
      (begin
        (ccurva-aux t tam (- niv 1))
        (t 'derecha 90)
        (ccurva-aux t tam (- niv 1))
        (t 'izquierda 90))))


;(ccurva 1 16)



(define t1 (cons-tortuga "red"))
(define t2 (cons-tortuga "green"))

;(copo 300 9)




(define (pol-reg long-lado n-lados)
  (let ((t (cons-tortuga "black")))
    (t 'sube)
    (t 'vete-a (list (/ ancho 2.0) 10))
    (t 'baja)
    (do ((i 1 (+ i 1)))
      ((> i n-lados) (void))
      (t 'avanza long-lado)
      (t 'derecha (/ 360.0 n-lados)))))

;(pol-reg 200 10)
