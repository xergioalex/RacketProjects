(define horca-base (line 50 0 'black))
(define horca-paral (line 0 250 'black))
(define horca-horizontal (line 250 0 'black))
(define vertical (line 0 30 'black))
(define horca (overlay/xy horca-paral 0 0 horca-horizontal))
(define horca1 (overlay/xy horca 250 0 vertical))
(define horca2 (overlay/xy horca1 0 250 horca-base))
;(display "   ")
(define cabeza (circle 30 "outline" 'black))
(define horca-cabeza (overlay/xy horca2 250 60 cabeza))
(define horca-cuello (overlay/xy horca-cabeza  250 90 vertical))
(define diagonal-1 (line 30 30 'black))
(define horca-mano1  (overlay/xy horca-cuello  250 120  diagonal-1))
(define horca-mano2  (add-line horca-mano1     250 120 220 150 'black))
(define horca-cuerpo (overlay/xy horca-mano2   250 120  vertical))
(define horca-pierna1(add-line horca-cuerpo    250 150 220 180 'black))
(define horca-colgado(overlay/xy horca-pierna1 250 150  diagonal-1))
horca-colgado

(define (rayitas cadena)
  (ray raya 1 (string-length cadena)  )
)

(define raya  (line 30 0 'black) )

(define (ray imagen control-ciclo fin-ciclo)
  (if   (<= control-ciclo fin-ciclo)
           (ray (overlay/xy  imagen (* 60 (- control-ciclo 1)) 0 raya) (+  control-ciclo 1) fin-ciclo)
           imagen
   )
)

(rayitas (read))