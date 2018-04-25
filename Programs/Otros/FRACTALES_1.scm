(require (lib "graphics.ss" "graphics"))  ;; DrScheme version 204
(open-graphics)

;; La ventana grafica
;; dimensiones de la ventana
(define ancho 1200)
(define alto  900)

(define ventana (open-viewport "El mundo de las tortugas" ancho alto))



(define (cons-tortuga color)
  ;; variables auxiliares
  (define pi (acos -1.0))
  
  ;; atributos de la tortuga
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
    (set! a (radianes->grados (atan (- (obten-y otra-tortuga) y)
                                    (- (obten-x otra-tortuga) x)))))
  (define (mira-al-raton)
    (let ((posn-raton (query-mouse-posn ventana)))
      (set! a (radianes->grados (atan (- (posn-y posn-raton) y)
                                      (- (posn-x posn-raton) x))))))
  (define (sube)
    (if pinta
        (set! pinta #f)
        (error "La tortuga ya esta arriba")))
  
  (define (baja)
    (if (not pinta)
        (set! pinta #t)
        (error "Ia tortuga ya esta abajo")))
  
  (define (acasa)
    (set! x (/ ancho 2.0)) 
    (set! y (/ alto 2.0))   
    (set! posicion (make-posn x y)) 
    (set! a 0.0)  
    (set! pinta #t) )
  
  (define (reinicia)
    ((clear-viewport ventana))
    (acasa))
  
  
  
  ;;;;;
  (define (despacha m)
    (cond 
      ((equal? m 'avanza) avanza)
      ((equal? m 'obten-x-pos) obten-x-pos)
      ((equal? m 'obten-y-pos) obten-y-pos)
      ((equal? m 'gira) gira)
      ((equal? m 'derecha) derecha)
      ((equal? m 'izquierda) izquierda)
      ((equal? m 'mira-a) mira-a)
      ((equal? m 'mira-al-raton) mira-al-raton)
      ((equal? m 'sube) sube)
      ((equal? m 'baja) baja)
      ((equal? m 'acasa) acasa)
      ((equal? m 'reinicia) reinicia)
      (else (error "Mensaje desconocido" m))
      )
    )
  
  despacha)

(define (avanza tortuga d)
  ((tortuga 'avanza) d))

(define (obten-x tortuga)
  ((tortuga 'obten-x-pos)))

(define (obten-y tortuga)
  ((tortuga 'obten-y-pos)))

(define (gira tortuga grados)
  ((tortuga 'gira) grados))

(define (derecha tortuga grados)
  ((tortuga 'derecha) grados))

(define (izquierda tortuga grados)
  ((tortuga 'izquierda) grados))

;;pone la tortuga1 mirando hacia la tortuga2
(define (mira-a tortuga1 tortuga2) 
  ((tortuga1 'mira-a) tortuga2))

(define (mira-al-raton tortuga)
  ((tortuga 'mira-al-raton)))

(define (sube tortuga)
  ((tortuga 'sube)))

(define (baja tortuga)
  ((tortuga 'baja)))

(define (acasa tortuga)
  ((tortuga 'acasa)))

(define (reinicia tortuga)
  ((tortuga 'reinicia)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ejemplos

;; Fractales


(define (dragon tam nivel)
  ((draw-viewport ventana) "blue")
  (let ((t (cons-tortuga "green")))
    (dragoni t tam nivel)))

(define (dragoni t tam nivel)
  (if (= nivel 0)
      (avanza t tam)
      (begin
        (dragoni t tam (- nivel 1))
        (izquierda t 90)
        (dragond t tam (- nivel 1)))))

(define (dragond t tam nivel)
  (if (= nivel 0)
      (avanza t tam)
      (begin
        (dragoni t tam (- nivel 1))
        (derecha t 90)
        (dragond t tam (- nivel 1)))))

(dragon 0.5 22)

;; Copo de nieve

(define (copo t talla nivel)
  (do ((i 1 (+ 1 i)))
    ((> i 3) (void))
    (copo-aux t talla nivel)
    (derecha t 120)))

(define (copo-aux t talla nivel)
  (if (= nivel 0)
      (avanza t talla)
      (begin
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (izquierda t 60)
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (derecha t 120)
        (copo-aux t (/ talla 3.0) (- nivel 1))
        (izquierda t 60)
        (copo-aux t (/ talla 3.0) (- nivel 1)))))




(copo 400 8 )


(define (copos t n)
  (do ((i 1(+ i 1)))
    ((> i n) (void))
    (sleep 1)
    (copo t 400 i)))




(copos t1 6)


(define (pol-reg long-lado n-lados)
  (let ((t (cons-tortuga "black")))
    (do ((i 1 (+ i 1)))
      ((> i n-lados) (void))
      (avanza t long-lado)
      (derecha t (/ 360.0 n-lados)))))

;(pol-reg 100 12)

(define (ccurva tam niv)
  ((draw-viewport ventana) "blue")
  (let ((t (cons-tortuga "orange")))
    (ccurva-aux t tam niv)))

(define (ccurva-aux t tam niv)
  (if (= niv 0)
      (avanza t tam)
      (begin
        (ccurva-aux t tam (- niv 1))
        (derecha t 90)
        (ccurva-aux t tam (- niv 1))
        (izquierda t 90))))


;(ccurva 0.1 120)

(define (random-1)
  (+ 0.92 (/ (random 1000)
            1000.0)))

(define (random-2)
  (+ 0.8 (/ (random 1000)
             1000.0)))

(define (random-3)
  (- (/ (random 1000)
        1000.0)
     0.82))


(define (dos-tortugas-y-un-raton)
  ((draw-viewport ventana) "darkgreen")
  (let ((t1 (cons-tortuga "red"))
        (t2 (cons-tortuga "blue")))
    (sube t1)
    (avanza t1 (/ ancho 2.0))
    (izquierda t1 90)
    (avanza t1 (/ alto 2.0))
    (mira-a t1 t2)
    (baja t1)
    (do ((i 1 (+ 1 i)))
      (#f)
      (sleep 0.012)
      (avanza t2 (random-1))
      (mira-al-raton t2)
      (mira-a t1 t2)
      (avanza t1 (random-2)))
    ))

(dos-tortugas-y-un-raton)