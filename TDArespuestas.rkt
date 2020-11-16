#lang racket
(require "TDArespuesta.rkt")
(require "FuncionesGenerales.rkt")
(provide respuestas?)
(provide emptyAnswers)

;TDA respuestas.
;Representación: una lista de TDAs respuestas (respuestas1, respuesta2,....,..,., respuestaN).

;Capa constructor.
;Dom: TDAs respuesta.
;Rec: TDA respuestas
(define emptyAnswers null)
(define (respuestas . respuesta) respuesta)


;Capa selector.
;Utiliza las funciones "primerElemento" y "siguientesElementos" encontradas en el archivo
;"funcionesGenerales" como funciones de capa selector.


;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Entrega un true si las lista entregada corresponde a un TDA respuestas y sino un false.
;Esta función utiliza la función recursiva "es?" ubicada "funcionesGenerales" para ir
;verificando si cada elemento de la lista corresponde a una respuesta y de esta forma saber
;si es un TDA respuestas.
(define respuestas?(lambda(lista)
                     (if (null? lista)
                         true
                         (es? respuesta? lista)
                         )
                     )
  )

;Ejemplo:
(define respuestas1(respuestas a1 a2 a3))
(define respuestas2(respuestas a4))
respuestas1
(provide respuestas1)
(provide respuestas2)