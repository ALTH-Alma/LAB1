#lang racket

(require "FuncionesGenerales.rkt")
(require "TDArespuesta.rkt")
(require "TDApregunta.rkt")
(provide preguntas?)

;TDA preguntas.
;Representación: una lista de TDAs pregunta (pregunta1, pregunta2,....,..,., preguntaN).

;Capa constructor.
;Dom: TDAs pregunta.
;Rec: TDA preguntas.
(define emptyAsks null)
(define (preguntas . pregunta) pregunta)



;Capa selector.
;Utiliza las funciones "primerElemento" y "siguientesElementos" encontradas en el archivo
;"funcionesGenerales" como funciones de capa selector.


;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Entrega un true si las lista entregada corresponde a un TDA preguntas y sino un false.
;Esta función utiliza la función recursiva "es?" ubicada "funcionesGenerales" para ir
;verificando si cada elemento de la lista corresponde a una pregunta y de esta forma saber
;si es un TDA preguntas.
(define preguntas? (lambda(lista)
                    (es? pregunta? lista)
                     )
  )


(define actualizarPreguntasRecompensa(lambda (listaPreguntas id recompensa)
                                       (actualizar id getIdPreg modificarPregRecompensa recompensa listaPreguntas)))



;Ejemplo:
(actualizarPreguntasRecompensa preguntas1 1 (recompensa "pepe" 20))

;Ejemplo:
;(preguntas? preguntas1)
;Ejemplo:
(provide preguntas1)
(define preguntas1(preguntas p1 p2 p3))
;preguntas1