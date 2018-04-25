;unidades
;unidades num -> string
;tiene como objetivo evaluar el numero entrante entre 0-15 y de volver una cadena 
;con en el nombre del numero
;ejemplo:(unidades 7)-> "siete"
(define(unidades n)
  (cond
    ((= n 0) "cero")
    ((= n 1) "uno")
    ((= n 2) "dos")
    ((= n 3) "tres")
    ((= n 4) "cuatro")
    ((= n 5) "cinco")
    ((= n 6) "seis")
    ((= n 7) "siete")
    ((= n 8) "ocho")
    ((= n 9) "nueve")
    ((= n 10) "diez")
    ((= n 11) "once")
    ((= n 12) "doce")
    ((= n 13) "trece")
    ((= n 14) "catorce")
    ((= n 15) "quince")
    (else (centimos n))
     )
  )
;decenas
;decenas num -> string
;recibe un numero entre 16 <= n < 100 el cual lo evalua y me de vuelve una cadena con el nombre
;del valor entrado llamando la funcion "unidades" para hacer combinaciones y concatenar las cadenas respectivas
;ejemplo: (decenas 25) -> "veinticinco"
(define(decenas n)
  (if(and(>= n 16) (< n 20))
     (string-append "dieci" (unidades (- n 10)))
     (if(= n 20)
         "veinte"
     (if(and(> n 20) (< n 30))
        (string-append "veinti" (unidades (- n 20)))
        (if(= n 30)
            "treinta"
        (if(and(> n 30) (< n 40))
           (string-append "treinta y " (unidades (- n 30)))
           (if(= n 40)
              "cuarenta"
           (if(and(> n 40) (< n 50))
              (string-append "cuarenta y " (unidades (- n 40)))
              (if(= n 50)
                 "cincuenta"
              (if(and(>= n 50) (< n 60))
                 (string-append "cincuenta y " (unidades (- n 50)))
                 (if(= n 60)
                    "sesenta"
                 (if(and(>= n 60) (< n 70))
                    (string-append "sesenta y " (unidades (- n 60)))
                    (if(= n 70)
                       "setenta"
                    (if(and(>= n 70) (< n 80))
                       (string-append "setenta y " (unidades (- n 70)))
                       (if(= n 80)
                          "ochenta"
                       (if(and(>= n 80) (< n 90))
                          (string-append "ochenta y " (unidades (- n 80)))
                          (if(= n 90)
                             "noventa"
                          (if(and(>= n 90) (< n 100))
                             (string-append "noventa y " (unidades (- n 90)))
                             (unidades n)
                             )))))))))))))))))) 
                                        
;centenas
;centenas num -> string
;recibe un numero entre 100 <= n < 1000 el cual lo evalua y me de vuelve una cadena con el nombre
;del valor entrado llamando la funcion "decenas" para hacer combinaciones y concatenar las cadenas respectivas
;ejemplo: (centenas 110) -> "ciento diez"
(define(centenas n)
  (if(= n 100)
     "cien"
     (if(and(> n 100) (< n 200))
        (string-append "ciento " (decenas (- n 100)))
        (if(= n 200)
           "docientos"
           (if(and(> n 200) (< n 300))
              (string-append "docientos " (decenas (- n 200)))
              (if(= n 300)
                 "trecientos"
                 (if(and(> n 300) (< n 400))
                    (string-append "trecientos " (decenas (- n 300)))
                    (if(= n 400)
                       "cuatrocientos"
                       (if(and(> n 400) (< n 500))
                          (string-append "cuatrocientos " (decenas(- n 400)))
                          (if(= n 500)
                             "quinientos"
                             (if(and(> n 500) (< n 600))
                                (string-append "quinientos " (decenas(- n 500)))
                                (if(= n 600)
                                   "seicientos"
                                   (if(and(> n 600) (< n 700))
                                      (string-append "seicientos " (decenas(- n 600)))
                                      (if(= n 700)
                                         "setencientos"
                                         (if(and(> n 700) (< n 800))
                                            (string-append "setecientos " (decenas (- n 700)))
                                            (if(= n 800)
                                               "ochocientos"
                                               (if(and(> n 800) (< n 900))
                                                  (string-append "ochocientos " (decenas (- n 800)))
                                                  (if(= n 900)
                                                     "novecientos"
                                                     (if(and(> n 900) (< n 1000))
                                                        (string-append "novecientos " (decenas(- n 900)))
                                                        (decenas n)
                                                        )
                                                     )
                                                  )
                                               )
                                            )
                                         )
                                      )
                                   )
                                )
                             )
                          )
                       )
                    )
                 )
              )
           )
        )
     )
  )
;miles
;miles num -> string
;recibe un numero entre 1000 <= n < 1000000 el cual lo evalua y me de vuelve una cadena con el nombre
;del valor entrado, llamandoce asi misma y a la funcion "centenas" para hacer combinaciones y concatenar las cadenas respectivas
;ejemplo: (miles 2120) -> "dos mil ciento veinte"
(define(miles n)
  (if(= n 1000)
     "mil"
     (if(and(> n 1000) (< n 2000))
        (string-append "mil " (centenas(- n 1000)) )
     (if(= n 2000)
        "dos mil"
        (if(and(> n 2000) (< n 3000))
        (string-append "dos mil " (centenas(- n 2000)))
        (if(= n 3000)
           "tres mil"
           (if(and(> n 3000) (< n 4000))
              (string-append "tres mil " (centenas(- n 3000)))
           (if(= n 4000)
              "cuatro mil"
              (if(and(> n 4000) (< n 5000))
                 (string-append "cuatro mil " (centenas(- n 4000)))
               (if(= n 5000)
                 "cinco mil"
                 (if(and(> n 5000) (< n 6000))
                    (string-append "cinco mil " (centenas(- n 5000)))
                 (if(= n 6000)
                    "seis mil"
                    (if(and(> n 6000) (< n 7000))
                       (string-append "seis mil " (centenas(- n 6000)))
                    (if(= n 7000)
                       "siete mil"
                       (if(and(> n 7000) (< n 8000))
                          (string-append "siete mil " (centenas(- n 7000)))
                    (if(= n 8000)
                       "ocho mil"
                       (if(and(> n 8000) (< n 9000))
                          (string-append "ocho mil " (centenas(- n 8000)))
                       (if(= n 9000)
                          "nueve mil"
                          (if(and(> n 9000) (< n 10000))
                             (string-append "nueve mil " (centenas(- n 9000)))                        
                          (if(= n 10000)
                             "diez mil"
                             (if(and(> n 10000) (< n 11000))
                                (string-append "diez mil " (centenas(- n 10000)))
                             (if(= n 11000)
                                "once mil"
                                (if(and(> n 11000) (< n 12000))
                                   (string-append "once mil " (centenas(- n 11000)))
                                (if(= n 12000)
                                   "doce mil"
                                   (if(and(> n 12000) (< n 13000))
                                   (string-append "doce mil " (centenas(- n 12000)))
                                   (if(= n 13)
                                      "trece mil"
                                      (if(and(> n 13000) (< n 14000))
                                   (string-append "trece mil " (centenas(- n 13000)))
                                      (if(= n 14000)
                                         "catorce mil"
                                         (if(and(> n 14000) (< n 15000))
                                   (string-append "catorce mil " (centenas(- n 14000)))
                                         (if(= n 15000)
                                            "quince mil"
  (if(and(> n 15000) (< n 16000))
     (string-append "quince mil " (centenas(- n 15000)))
  (if(and(>= n 16000) (< n 20000))
     (string-append "dieci"  (miles (- n 10000))) 
     (if(= n 20000)
         "veinte mil"
         (if(= n 21000)
            "veintiunmil"
            (if(and(>= n 21000) (< n 22000))
               (string-append "veintiunmil " (centenas(- n 21000)))
            (if(and(> n 20000) (< n 30000))
        (string-append "veinti" (miles(- n 20000)))
        (if(= n 30000)
           "treinta mil"
           (if(= n 31000)
              "treintaiunmil"
              (if(and(>= n 31000) (< n 32000))
               (string-append "treintaiunmil " (centenas(- n 31000)))
           (if(and(> n 30000) (< n 40000))
              (string-append "treinta y " (miles(- n 30000)))
           (if(= n 40000)
              "cuarenta mil"
              (if(= n 41000)
                 "cuarentaiunmil"
                 (if(and(>= n 41000) (< n 42000))
               (string-append "cuarentaiunmil " (centenas(- n 41000)))
              (if(and(> n 40000) (< n 50000))
                 (string-append "cuarenta y "  (miles(- n 40000)))
               (if(= n 50000)
                 "cincuenta mil"
                 (if(= n 51000)
                    "cincuentaiunmil"
                    (if(and(>= n 51000) (< n 52000))
               (string-append "cincuentaiunmil " (centenas(- n 51000)))
                 (if(and(> n 50000) (< n 60000))
                    (string-append "cincuenta y " (miles(- n 50000)))
                 (if(= n 60000)
                    "sesenta mil"
                    (if(= n 61000)
                       "sesentaiunmil"
                       (if(and(>= n 61000) (< n 62000))
               (string-append "sesentaiunmil " (centenas(- n 61000)))
                    (if(and(> n 60000) (< n 70000))
                       (string-append "sesenta y " (miles(- n 60000)))
                    (if(= n 70000)
                       "setenta mil"
                       (if(= n 71000)
                          "setentaiunmil"
                          (if(and(>= n 71000) (< n 72000))
               (string-append "setentaiunmil " (centenas(- n 71000)))
                          (if(and(>= n 71000) (< n 72000))
                             (string-append "setentaiunmil " (centenas(- n 71000)))
                       (if(and(> n 70000) (< n 80000))
                          (string-append "setenta y " (miles(- n 70000)))
                    (if(= n 80000)
                       "ochenta mil"
                       (if(= n 81000)
                          "ochentaiunmil"
                          (if(and(>= n 81000) (< n 82000))
               (string-append "ochentaiunmil " (centenas(- n 81000)))
                       (if(and(> n 80000) (< n 90000))
                          (string-append "ochenta y " (miles(- n 80000)))
                       (if(= n 9000)
                          "noventa mil"
                          (if(= n 91000)
                             "noventaiunmil"
                             (if(and(>= n 91000) (< n 92000))
               (string-append "noventaiunmil " (centenas(- n 91000)))
                          (if(and(> n 90000) (< n 100000))
                             (string-append "noventa y " (miles(- n 90000)))                        
   (if(= n 100000)
       "cien mil"
      (if(= n 101000)
        "cientounmil"
        (if(and(>= n 101000) (< n 102000))
           (string-append "cientounmil "(centenas(- n 101000)))
         (if(and(> n 100000) (< n 200000))
          (string-append "ciento " (miles(- n 100000)))
        (if(= n 200000)
           "docientos mil"
           (if(= n 201000)
              "docientosunmil"
              (if(and(>= n 201000) (< n 202000))
           (string-append "docientosunmil "(centenas(- n 201000)))
           (if(and(> n 200000) (< n 300000))
              (string-append "docientos " (miles (- n 200000)))
              (if(= n 300000)
                 "trecientos mil"
                 (if(= n 301000)
                    "trecientosunmil"
                    (if(and(>= n 301000) (< n 302000))
           (string-append "trecientosunmil "(centenas(- n 301000)))
                 (if(and(> n 300000) (< n 400000))
                    (string-append "trecientos " (miles (- n 300000)))
                    (if(= n 400000)
                       "cuatrocientos mil"
                       (if(= n 401000)
                          "cuatrocientosunmil"
                          (if(and(>= n 401000) (< n 402000))
                   (string-append "cuatrocientosunmil "(centenas(- n 401000)))
                       (if(and(> n 400000) (< n 500000))
                          (string-append "cuatrocientos " (miles(- n 400000)))
                          (if(= n 500000)
                             "quinientos mil"
                             (if(= n 501000)
                                "quinientosunmil"
                                (if(and(>= n 501000) (< n 502000))
                       (string-append "quinientosunmil "(centenas(- n 501000)))
                             (if(and(> n 500000) (< n 600000))
                                (string-append "quinientos " (miles(- n 500000)))
                                (if(= n 600000)
                                   "seicientos mil"
                                   (if(= n 601000)
                                      "seicientosunmil"
                                      (if(and(>= n 601000) (< n 602000))
                                         (string-append "seicientosunmil "(centenas(- n 601000)))
                                   (if(and(> n 600000) (< n 700000))
                                      (string-append "seicientos " (miles(- n 600000)))
                                      (if(= n 700000)
                                         "setencientos mil"
                                         (if(= n 701000)
                                            "setencientosunmil"
                                            (if(and(>= n 701000) (< n 702000))
                                     (string-append "setecientosunmil "(centenas(- n 701000)))
                                         (if(and(> n 700000) (< n 800000))
                                            (string-append "setecientos " (miles (- n 700000)))
                                            (if(= n 800000)
                                               "ochocientos mil"
                                               (if(= n 801000)
                                                  "ochocientosunmil"
                                                  (if(and(>= n 801000) (< n 802000))
                                            (string-append "ochocientosunmil "(centenas(- n 801000)))
                                               (if(and(> n 800000) (< n 900000))
                                                  (string-append "ochocientos " (miles (- n 800000)))
                                                  (if(= n 900000)
                                                     "novecientos mil"
                                                     (if(= n 901000)
                                                        "novecientosunmil"
                                                        (if(and(>= n 901000) (< n 902000))
                                                  (string-append "novecientosunmil "(centenas(- n 901000)))
                                                     (if(and(> n 900000) (< n 1000000))
                                                        (string-append "novecientos " (miles(- n 900000)))
                                                        (centenas n))))))))))))))))))))))))))))
   )))))))))))))))))))))))))
  )))))))))))))))))))))))))))))))))))))))))))))))))
;millones
;millones num -> string
;recibe un numero entre 1000000 <= n < 1000000000000 el cual lo evalua y devuelve una cadena con el nombre
;del valor entrado, llamando la funcion "miles" y llamandoce asi misma, para hacer combinaciones y concatenar las cadenas respectivas
;ejemplo: (millones 5671965) -> "cinco millones seicientos setentaiunmil novecientos tres"
(define(millones n)
  (if(= n 1000000)
     "un millon "
     (if(and(> n 1000000) (< n 2000000))
        (string-append "un millon " (miles(- n 1000000)))
        (if(= n 2000000)
           "dos millones"
           (if(and(> n 2000000) (< n 3000000))
              (string-append "dos millones " (miles(- n 2000000)))
              (if(= n 3000000)
                "tres millones"
                  (if(and(> n 3000000) (< n 4000000))
                     (string-append "tres millones " (miles(- n 3000000)))
                     (if(= n 4000000)
                        "cuatro millones"
                        (if(and(> n 4000000) (< n 5000000))
                           (string-append "cuatro millones "(miles(- n 4000000)))
                           (if(= n 5000000)
                               "cinco millones"
                               (if(and(> n 5000000) (< n 6000000))
                                  (string-append "cinco millones " (miles(- n 5000000)))
                                  (if(= n 6000000)
                                    "seis millones"
                                    (if(and(> n 6000000) (< n 7000000))
                                       (string-append "seis millones " (miles(- n 6000000)))
                                        (if(= n 7000000)
                                          "siete millones"
                                           (if(and(> n 7000000) (< n 8000000))
                                              (string-append "siete millones " (miles(- n 7000000)))
                                              (if(= n 8000000)
                                                  "ocho millones"
                                                   (if(and(> n 8000000) (< n 9000000))
                                                      (string-append "ocho millones " (miles(- n 8000000)))
                                                      (if(= n 9000000)
                                                         "nueve millones"
                                                         (if(and(> n 9000000) (< n 10000000))
                                                            (string-append "nueve millones " (miles(- n 9000000))) 
                                                            (if(= n 10000000)
                                                               "diez millones"
                                                                (if(and(> n 10000000) (< n 11000000))
                                                                   (string-append "diez millones " (miles(- n 10000000)))
                                                                   (if(= n 11000000)
                                                                      "once millones"
                                                                      (if(and(> n 11000000) (< n 12000000))
                                                                         (string-append "once millones " (miles(- n 11000000)))
                                                                         (if(= n 12000000)
                                                                            "doce millones"
                                                                             (if(and(> n 12000000) (< n 13000000))
                                                                                (string-append "doce millones " (miles(- n 12000000)))
                                                                                (if(= n 13000000)
                                                                                   "trece millones"
                                                                                    (if(and(> n 13000000) (< n 14000000))
                                                                                       (string-append "trece millones " (miles(- n 13000000)))
                                                                                    (if(= n 14000000)
                                                                                        "catorce millones"
                                                                                        (if(and(> n 14000000) (< n 15000000))
                                                                                           (string-append "catorce millones " (miles(- n 14000000)))
                                                                                         (if(= n 15000000)
                                                                                            "quince millones"
                                                                                             (if(and(> n 15000000) (< n 16000000))
                                                                                                (string-append "quince millones " (miles(- n 15000000)))
  (if(and(>= n 16000000) (< n 20000000))
     (string-append "dieci" (millones(- n 10000000)))
     (if(= n 20000000)
        "veinte millones"
        (if(= n 21000000)
           "veintiunmillones"
           (if(and(>= n 21000000) (< n 22000000))
              (string-append "veintiunmillones "(miles(- n 21000000)))
     (if(and(> n 20000000) (< n 30000000))
        (string-append "venti"(millones(- n 20000000)))
        (if(= n 30000000)
           "treinta millones"
           (if(= n 31000000)
           "treintaiunmillones"
           (if(and(>= n 31000000) (< n 32000000))
              (string-append "treintaiunmillones "(miles(- n 31000000)))
        (if(and(> n 30000000) (< n 40000000))
           (string-append "treinta y "(millones(- n 30000000)))
           (if(= n 40000000)
              "cuarenta millones"
              (if(= n 41000000)
           "cuarentaiunmillones"
           (if(and(>= n 41000000) (< n 42000000))
              (string-append "cuarentaiunmillones "(miles(- n 41000000)))
           (if(and(> n 40000000) (< n 50000000))
              (string-append "cuarenta y "(millones(- n 40000000)))
              (if(= n 50000000)
                 "cincuenta millones"
                 (if(= n 51000000)
           "cincuentaiunmillones"
           (if(and(>= n 51000000) (< n 52000000))
              (string-append "cincuentaiunmillones "(miles(- n 51000000)))
              (if(and(> n 50000000) (< n 60000000))
                 (string-append "cincuenta y "(millones(- n 50000000)))
                 (if(= n 60000000)
                    "sesenta millones"
                    (if(= n 61000000)
           "sesentaiunmillones"
           (if(and(>= n 61000000) (< n 62000000))
              (string-append "sesentaiunmillones "(miles(- n 61000000)))
                 (if(and(> n 60000000) (< n 70000000))
                   (string-append "sesenta y "(millones(- n 60000000)))
                   (if(= n 70000000)
                      "setenta millones"
                      (if(= n 71000000)
           "setentaiunmillones"
           (if(and(>= n 71000000) (< n 72000000))
              (string-append "setentaiunmillones "(miles(- n 71000000)))
                   (if(and(> n 70000000) (< n 80000000))
                      (string-append "setenta y "(millones(- n 70000000)))
                      (if(= n 80000000)
                         "ochenta millones"
                         (if(= n 81000000)
           "ochentaiunmillones"
           (if(and(>= n 81000000) (< n 82000000))
              (string-append "ochentaiunmillones "(miles(- n 81000000)))
                      (if(and(> n 80000000) (< n 90000000))
                         (string-append "ochenta y "(millones(- n 80000000)))
                         (if(= n 90000000)
                            "noventa millones"
                            (if(= n 91000000)
           "noventaiunmillones"
           (if(and(>= n 91000000) (< n 92000000))
              (string-append "noventaiunmillones "(miles(- n 91000000)))
                         (if(and(> n 90000000) (< n 100000000))
                            (string-append "noventa y "(millones(- n 90000000)))
                            (if(= n 100000000)
                               "cien millones"
                            (if(and(> n 100000000) (< n 200000000))
                              (string-append "ciento "(millones(- n 100000000)))
                              (if(= n 200000000)
                               "docientos millones"
                            (if(and(> n 200000000) (< n 300000000))
                               (string-append "docientos "(millones(- n 200000000)))
                               (if(= n 300000000)
                               "trecientos millones"
                            (if(and(> n 300000000) (< n 400000000))
                              (string-append "trecientos "(millones(- n 300000000)))
                              (if(= n 400000000)
                               "cuatrocientos millones"
                            (if(and(> n 400000000) (< n 500000000))
                              (string-append "cuatrocientos "(millones(- n 400000000)))
                              (if(= n 500000000)
                               "quinientos millones"
                            (if(and(> n 500000000) (< n 600000000))
                              (string-append "quinientos "(millones(- n 500000000)))
                              (if(= n 600000000)
                               "seicientos millones"
                            (if(and(> n 600000000) (< n 700000000))
                              (string-append "seicientos "(millones(- n 600000000)))
                              (if(= n 700000000)
                               "setecientos millones"
                            (if(and(> n 700000000) (< n 800000000))
                              (string-append "setecientos "(millones(- n 700000000)))
                              (if(= n 800000000)
                               "ochocientos millones"
                            (if(and(> n 800000000) (< n 900000000))
                              (string-append "ochocientos "(millones(- n 800000000)))
                              (if(= n 900000000)
                               "novecientos millones"
                            (if(and(> n 900000000) (< n 1000000000))
                              (string-append "novecientos "(millones(- n 900000000)))
                              (if(= n 1000000000)
                                 "mil millones"
                                 (if(and(> n 1000000000) (< n 2000000000))
                                    (string-append "mil " (millones(- n 1000000000)))
                                    (if(= n 2000000000)
                                 "dos mil millones"
                                 (if(and(> n 2000000000) (< n 3000000000))
                                    (string-append "dos mil " (millones(- n 2000000000)))
                                    (if(= n 3000000000)
                                 "tres mil millones"
                                 (if(and(> n 3000000000) (< n 4000000000))
                                    (string-append "tres mil " (millones(- n 3000000000)))
                                    (if(= n 4000000000)
                                 "cuatromil millones"
                                 (if(and(> n 4000000000) (< n 5000000000))
                                    (string-append "cuatro mil " (millones(- n 4000000000)))
                                    (if(= n 5000000000)
                                 "cinco mil millones"
                                 (if(and(> n 5000000000) (< n 6000000000))
                                    (string-append "cinco mil " (millones(- n 5000000000)))
                                    (if(= n 6000000000)
                                 "seis mil millones"
                                 (if(and(> n 6000000000) (< n 7000000000))
                                    (string-append "seismil " (millones(- n 6000000000)))
                                    (if(= n 7000000000)
                                 "siete mil millones"
                                 (if(and(> n 7000000000) (< n 8000000000))
                                    (string-append "siete mil " (millones(- n 7000000000)))
                                    (if(= n 8000000000)
                                 "ocho mil millones"
                                 (if(and(> n 8000000000) (< n 9000000000))
                                    (string-append "mil " (millones(- n 8000000000)))
                                    (if(= n 9000000000)
                                 "nueve mil millones"
                                 (if(and(> n 9000000000) (< n 10000000000))
                                    (string-append "nueve mil " (millones(- n 9000000000)))
                                    (if(= n 10000000000)
                                 "diez mil millones"
                                 (if(and(> n 10000000000) (< n 11000000000))
                                    (string-append "diez mil " (millones(- n 10000000000)))
                                    (if(= n 11000000000)
                                 "once mil millones"
                                 (if(and(> n 11000000000) (< n 12000000000))
                                    (string-append "once mil " (millones(- n 11000000000)))
                                    (if(= n 12000000000)
                                 "doce mil millones"
                                 (if(and(> n 12000000000) (< n 13000000000))
                                    (string-append "doce mil " (millones(- n 12000000000)))
                                    (if(= n 13000000000)
                                 "trece mil millones"
                                 (if(and(> n 13000000000) (< n 14000000000))
                                    (string-append "trece mil " (millones(- n 13000000000)))
                                    (if(= n 14000000000)
                                 "catorce mil millones"
                                 (if(and(> n 14000000000) (< n 15000000000))
                                    (string-append "catorce mil " (millones(- n 14000000000)))
                                    (if(= n 15000000000)
                                 "quince mil millones"
                                 (if(and(> n 15000000000) (< n 16000000000))
                                    (string-append "quince mil " (millones(- n 15000000000)))
                                    (if(and(>= n 16000000000) (< n 20000000000))
                                       (string-append "dieci" (millones(- n 10000000000)))
                                       (if(= n 20000000000)
                                          "veinte mil millones"
                                          (if(= n  21000000000)
                                                "veintiunmil millones"
                                                (if(and(>= n 21000000000) (< n 22000000000))
                                                   (string-append "ventiunmil " (millones(- n 21000000000)))
                                          (if(and(> n  20000000000) (< n 30000000000))
                                             (string-append "veinti" (millones(- n  20000000000))) 
                                                (if(= n 30000000000)
                                          "treinta mil millones"
                                          (if(= n  31000000000)
                                                "treintaiunmil millones"
                                                (if(and(>= n 31000000000) (< n 32000000000))
                                                   (string-append "treintaiunmil " (millones(- n 31000000000)))
                                          (if(and(> n  30000000000) (< n 40000000000))
                                             (string-append "treinta y " (millones(- n  30000000000)))
                                                (if(= n 40000000000)
                                          "cuarenta mil millones"
                                          (if(= n  41000000000)
                                                "cuarentaiunmil millones"
                                                (if(and(>= n 41000000000) (< n 42000000000))
                                                   (string-append "cuarentaiunmil " (millones(- n 41000000000)))
                                          (if(and(> n  40000000000) (< n 50000000000))
                                             (string-append "cuarenta y " (millones(- n  40000000000)))
                                                (if(= n 50000000000)
                                          "cincuenta mil millones"
                                          (if(= n  51000000000)
                                                "cincuentaiunmil millones"
                                                (if(and(>= n 51000000000) (< n 52000000000))
                                                   (string-append "cincuentaiunmil " (millones(- n 51000000000)))
                                          (if(and(> n  50000000000) (< n 60000000000))
                                             (string-append "cincuenta y " (millones(- n  50000000000)))
                                                (if(= n 60000000000)
                                          "sesenta mil millones"
                                          (if(= n  61000000000)
                                                "sesentaiunmil millones"
                                                (if(and(>= n 61000000000) (< n 62000000000))
                                                   (string-append "sesentaiunmil " (millones(- n 61000000000)))
                                          (if(and(> n  60000000000) (< n 70000000000))
                                             (string-append "sesenta y " (millones(- n  60000000000)))
                                                (if(= n 70000000000)
                                          "setenta mil millones"
                                          (if(= n  71000000000)
                                                "setentaiunmil millones"
                                                (if(and(>= n 71000000000) (< n 72000000000))
                                                   (string-append "setentaiunmil " (millones(- n 71000000000)))
                                          (if(and(> n  70000000000) (< n 80000000000))
                                             (string-append "setenta y " (millones(- n  70000000000)))
                                                (if(= n 80000000000)
                                          "ochenta mil millones"
                                           (if(= n  81000000000)
                                                "ochentaiunmil millones"
                                                (if(and(>= n 81000000000) (< n 82000000000))
                                                   (string-append "ochentaiunmil " (millones(- n 81000000000)))
                                          (if(and(> n  80000000000) (< n 90000000000))
                                             (string-append "ochenta y " (millones(- n  80000000000)))
                                                (if(= n 90000000000)
                                          "noventa mil millones"
                                          (if(= n  91000000000)
                                                "noventaiunmil millones"
                                                (if(and(>= n 91000000000) (< n 92000000000))
                                                   (string-append "noventaiunmil " (millones(- n 91000000000)))
                                          (if(and(> n  90000000000) (< n 100000000000))
                                             (string-append "noventa y " (millones(- n  90000000000)))
                                                (if(= n 100000000000)
                                          "cien mil millones"
                                          (if(= n  101000000000)
                                                "cientounmil millones"
                                                (if(and(>= n 101000000000) (< n 102000000000))
                                                   (string-append "cientounmil " (millones(- n 101000000000)))
                                          (if(and(> n  100000000000) (< n 200000000000))
                                             (string-append "ciento" (millones(- n  100000000000)))
                                                (if(= n 200000000000)
                                          "docientos mil millones"
                                           (if(= n  201000000000)
                                                "docientosunmil millones"
                                                (if(and(>= n 201000000000) (< n 202000000000))
                                                   (string-append "docientosunmil " (millones(- n 201000000000)))
                                          (if(and(> n  200000000000) (< n 300000000000))
                                             (string-append "docientos " (millones(- n  200000000000)))
                                                (if(= n 300000000000)
                                          "trecientos mil millones"
                                           (if(= n  301000000000)
                                                "trecientosunmil millones"
                                                (if(and(>= n 301000000000) (< n 302000000000))
                                                   (string-append "trecientosunmil " (millones(- n 301000000000)))
                                          (if(and(> n  300000000000) (< n 400000000000))
                                             (string-append "trecientos " (millones(- n  300000000000)))
                                                (if(= n 400000000000)
                                          "cuatrocientos mil millones"
                                          (if(= n  401000000000)
                                                "cuatrocientosunmil millones"
                                                (if(and(>= n 401000000000) (< n 402000000000))
                                                   (string-append "cuatrocientosunmil " (millones(- n 401000000000)))
                                          (if(and(> n  400000000000) (< n 500000000000))
                                             (string-append "cuatrocientos " (millones(- n  400000000000)))
                                                (if(= n 500000000000)
                                          "quinientos mil millones"
                                          (if(= n  501000000000)
                                                "quinientosunmil millones"
                                                (if(and(>= n 501000000000) (< n 502000000000))
                                                   (string-append "quinientosunmil " (millones(- n 501000000000)))
                                          (if(and(> n  500000000000) (< n 600000000000))
                                             (string-append "quinientos " (millones(- n  500000000000)))
                                                (if(= n 600000000000)
                                          "seicientos mil millones"
                                          (if(= n  601000000000)
                                                "seicientosunmil millones"
                                                (if(and(>= n 601000000000) (< n 602000000000))
                                                   (string-append "seicientosunmil " (millones(- n 601000000000)))
                                          (if(and(> n  600000000000) (< n 700000000000))
                                             (string-append "seicientos " (millones(- n  600000000000)))
                                                (if(= n 700000000000)
                                          "setecientos mil millones"
                                          (if(= n  701000000000)
                                                "setecientosunmil millones"
                                                (if(and(>= n 701000000000) (< n 702000000000))
                                                   (string-append "setecientosunmil " (millones(- n 701000000000)))
                                          (if(and(> n  700000000000) (< n 800000000000))
                                             (string-append "setecientos " (millones(- n  700000000000)))
                                                (if(= n 800000000000)
                                          "ochocientos mil millones"
                                          (if(= n  801000000000)
                                                "ochocientosunmil millones"
                                                (if(and(>= n 801000000000) (< n 802000000000))
                                                   (string-append "ochocientosunmil " (millones(- n 801000000000)))
                                          (if(and(> n  800000000000) (< n 900000000000))
                                             (string-append "ochocientos " (millones(- n  800000000000)))
                                                (if(= n 900000000000)
                                          "novecientos mil millones"
                                          (if(= n  901000000000)
                                                "novecientosunmil millones"
                                                (if(and(>= n 901000000000) (< n 902000000000))
                                                   (string-append "novecientosunmil " (millones(- n 901000000000)))
                                          (if(and(> n  900000000000) (< n 1000000000000))
                                             (string-append "novecientos " (millones(- n  900000000000)))
                                                (if(= n 1000000000000)
                                                   "un billon"
                                       (miles n))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                              )))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
;monto_escrito
;monto_escrito num -> string
;su objetivo es dar paso a todas las funciones anteriores, evaluando el parametro
; en trado en cada una de las condiciones dadas, asi dando paso a la funcion correspondiente
(define(monto_escrito n)
  (cond
    ((and(>= n 0) (< n 15)) (unidades n))
    ((and(>= n 16) (< n 100)) (decenas n))
    ((and(>= n 100) (< n 1000)) (centenas n))
    ((and(>= n 1000) (< n 1000000)) (miles n))
    ((and(>= n 1000000) (<= n 1000000000000)) (millones n))
    )
  )
  
(monto_escrito (read))
  
    
                                          

                                    
                                    
                                    
                                    
                                    
                                    
                                    

                              
                              

                              
                                                            
                     

        
        
                                   
          
                                
                             
     
  
     
                                            
  
     
                      
        
        
     
  
     
  

                    
                             
                          

        
  

              