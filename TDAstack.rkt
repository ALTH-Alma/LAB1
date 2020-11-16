#lang racket

(require "TDAusuario.rkt")
(require "TDAusuarios.rkt")
(require "TDApreguntas.rkt")
(provide stack)
(provide stack?)
(provide getUsuarios)
(provide getPreguntas)
(provide getActivo)
(provide getCorrPreg)
(provide getCorrRes)
(provide agregarAactivo)


;TDA stack.
;Representación: lista(TDAusuarios lista de todos los usuarios del stack, TDAusuario el usuario con sesión iniciada (Activo),
;TDApreguntas lista de todas las preguntas del stack, int correlativo para asignar IDs a las preguntas que se formulan en el stack,
;int correlativo para asignar IDs a las respuestas que se formulan en el stack).

;Capa constructor.
;Dom: 2 enteros, un TDAusuario, un TDAusuarios y un TDApreguntas.
;Rec: un TDAstack.
(define emptyStack null)
(define stack(lambda(perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
               (if(and(usuarios? perfiles)(usuario? perfilActivo)(preguntas? listaPreguntas)(integer? correlativoPreg)(integer? correlativoRes))
                  (list perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
                  emptyStack
                  )
               )
  )

;Capa selector.
;Dom: todas las funciones de la capa selector reciben como entrada un TDAstack.
;____

;Rec: un TDAusuarios.
;Entrega la lista de TDAs usuario que contiene el stack.
(define getUsuarios(lambda(stack)(car stack)))

;Rec: un TDAusuario.
;Entrega un TDAusuario de la persona que tiene sesión activa en el stack.
(define getActivo(lambda(stack)(car(cdr stack))))

;Rec: un TDApreguntas.
;Entrega la lista de TDAs pregunta que contiene el stack.
(define getPreguntas(lambda(stack)(car(cdr(cdr stack)))))

;Rec: un entero.
;Entrega un número que índica el ID de la siguiente pregunta si esta se crea, se basa en el ID de la última pregunta.
(define getCorrPreg(lambda(stack)(car(cdr(cdr(cdr stack))))))

;Rec: un entero.
;Entrega un número que índica el ID de la siguiente respuesta si esta se crea, se basa en el ID de la última respuesta.
(define getCorrRes(lambda(stack)(car(cdr(cdr(cdr(cdr stack)))))))
;____

;Capa pertenencia.
;Dom: un lista, candidato a TDAstack.
;Rec: un booleano.
;La función entrega un true si el candidato resulato ser stack y false sino.
(define stack?(lambda(stack1)
                 (if(and(usuarios? (getUsuarios stack1))(usuario? (getActivo stack1))(preguntas? (getPreguntas stack1))
                        (integer? (getCorrPreg stack1))(integer?(getCorrRes stack1)))
                    true
                    false
                    )
                 )
  )

;Dom: un TDAstack y un string(nombre).
;Rec: un stack actualizado.
;La función agrega a un usuario a la sesión activa(iniciada) del stack, utiliza la función "getUsuarios" para encontrar al usuario
;por su nombre en el TDAusuarios del stack y lo agrega al perfil activo.
(define agregarAactivo(lambda(stack1 nombre)
                         (stack (getUsuarios stack1)(getUsuario nombre (getUsuarios stack1))(getPreguntas stack1)(getCorrPreg stack1)(getCorrRes stack1))))





;Ejemplo:
(agregarAactivo stack1 "Ana")

;Ejemplo:
(define stack1(stack usuarios1 emptyUser preguntas1 2 2))
;(define l2(list "------------------------------"))
;  l2
;usuarios1
;(usuarios? usuarios1)
;preguntas1
;(preguntas? preguntas1)
stack1

;Ejemplo:
;(getCorrPreg stack1)

;Ejemplo:
;(getUsuarios stack1)
