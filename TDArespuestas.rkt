#lang racket
(require "TDArespuesta.rkt")
(require "FuncionesGenerales.rkt")

(provide respuestas?)
(provide emptyAnswers)
(provide actualizarRespuestasAccept)
(provide actualizarResVot)
(provide mostrarRespuestas)

;TDA respuestas.
;Representación: una lista de respuesta (respuestas1, respuesta2,....,..,., respuestaN).

;Capa constructor.
;Dom: una o más respuesta.
;Rec: respuestas.
(define emptyAnswers null)
(define (respuestas . respuesta) respuesta)


;Capa selector.
;Utiliza las funciones "primerElemento" y "siguientesElementos" encontradas en el archivo
;"funcionesGenerales" como funciones de capa selector.


;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Entrega un true si las lista entregada corresponde a respuestas y sino un false.
;Esta función utiliza la función recursiva "es?" ubicada "funcionesGenerales" para ir
;verificando si cada elemento de la lista corresponde a una respuesta y de esta forma saber
;si es respuestas.
(define respuestas?(lambda(lista)
                     (if (null? lista)
                         true
                         (es? respuesta? lista)
                         )
                     )
  )

;FUNCIONES EXTRAS:

;Dom: respuestas y un entero(identificador respuesta).
;Rec: Una lista de respuestas actualizada.
;La función actualiza respuestas, modificando una de sus respuesta's al cambiar su estado de acceptación por "Aceptada" (acepta una pregunta).
;Usa la función recursiva 'actualizar' que se encuentra en funcionesGenerales.rkt.
;actualizar encontrara la respuesta en la lista de respuestas(en base a su id) y una vez encontrada la entregara como
;parametro de entrada a la función 'modificarResAccept' junto con el string de aceptación "Aceptada".
(define actualizarRespuestasAccept(lambda(listaRespuestas idRes)
                             (actualizar idRes getIdRes modificarResAccept "Aceptada"  listaRespuestas)))


                        

;Dom: respuestas, un entero(id de respuesta) y un booleano.
;Rec: respuestas actualizadas.
;Actualiza respuestas cuando se realiza un voto.
;La función reescribe respuestas usando la función 'actualizar', mientras lo hace, si encuentra la respuesta por su id,
;pasa la respuesta como argumento de entrada a la función 'modificarResVot' junto al booleano que modificara la respuesta agregando un voto
;(a favor o en contra) y seguira reescribiendo respuestas con la nueva respuesta actualizada.
(define actualizarResVot(lambda (listaRespuestas idRes booleano)
                          (actualizar idRes getIdRes modificarResVot booleano listaRespuestas)))


;Dom: respuestas.
;Rec: un string.
;La función utiliza la función 'mostrarElementos' en conjunto con la función 'mostrarRespuesta' para ir transformando todos las respuesta's
;en respuestas, en string y los une en un gran string ordenado.
(define mostrarRespuestas(lambda(listaRespuestas)
                           (mostrarElementos listaRespuestas mostrarRespuesta)))




;EJEMPLOS NECESARIOS PARA EJEMPLOS DE FUNCIONES MAIN:

(define respuestas1(respuestas a1 a2))
(define respuestas2(respuestas a3))
(define respuestas3 emptyAnswer)
;respuestas1
;respuestas2
;respuestas3
(provide respuestas1)
(provide respuestas2)
(provide respuestas3)