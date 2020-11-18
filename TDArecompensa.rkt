#lang racket

(provide emptyReward)
(provide recompensa)
(provide getOfertor)
(provide getValorRecompensa)
(provide recompensa?)
(provide recompensa->string)


;TDA recompensa.
;Se usa para establecer recompensas en las preguntas.
;Representación: par(string ofertor, int valorRecompensa).

;Capa constructor.
;Dom: un string correpondiente al nombre de ofertor de la recompensa y un entero representando el valor de la recompensa ofrecida.
;Rec: una recompensa.
(define emptyReward (cons "" 0))
(define recompensa(lambda(ofertor vRecompensa)
                    (if(and (string? ofertor)(integer? vRecompensa)(> vRecompensa 0))
                       (cons ofertor vRecompensa)
                       emptyReward
                       )
                    )
  )

;Capa selector.
;Dom: ambas funciones tienen por entrada una recompensa.
;_____

;Rec: un string.
;Función que entrega el nombre del ofertor.
(define getOfertor(lambda(recompensa)(car recompensa)))

;Rec: un entero.
;Función que entrega el monto ofrecido como recompensa.
(define getValorRecompensa(lambda(recompensa)(cdr recompensa)))
;____

;Capa pertenencia.
;Dom: un par.
;Rec: un booleano.
;Función que entrega un true si el par correponde a una recompensa, sino un false.
(define recompensa?(lambda(recompensa)
                     (if (eqv? emptyReward recompensa)
                         true
                         (if(and (pair? recompensa)(string? (getOfertor recompensa))(integer? (getValorRecompensa recompensa))(> (getValorRecompensa recompensa) 0))
                            true
                            false
                            )
                         )
                     )
  )


;FUNCIÖN EXTRA:

;Dom: una recompensa.
;Rec. un string.
;la función transforma una recompensa en string.
(define recompensa->string(lambda(recompensa)
                            (if (eqv? recompensa emptyReward)
                                "No posee recompensa aun."
                                (string-append (number->string (getValorRecompensa recompensa))" puntos - Realizada por "(getOfertor recompensa)". "))))
                                


