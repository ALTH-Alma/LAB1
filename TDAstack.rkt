#lang racket

(require "FuncionesGenerales.rkt")
(require "TDAusuario.rkt")
(require "TDAusuarios.rkt")
(require "TDApregunta.rkt")
(require "TDApreguntas.rkt")
(require "TDArespuesta.rkt")
(provide stack)
(provide stack?)
(provide getUsuarios)
(provide getPreguntas)
(provide getActivo)
(provide getCorrPreg)
(provide getCorrRes)
(provide agregarAactivo)
(provide actualizarStackPreg)
(provide actualizarStackVotPreg)
(provide actualizarStackVotRes)
(provide mostrarStack)


;TDA stack.
;Representación: lista(usuarios -lista de todos los usuarios del stack, usuario -el usuario con sesión iniciada (Activo),
;preguntas -lista de todas las preguntas del stack, int correlativo para asignar IDs a las preguntas que se formulan en el stack,
;int correlativo para asignar IDs a las respuestas que se formulan en el stack).
;Use esta representación de stack porque al ser listas dentro de listas era una forma facíl de trabajar el paradigma funcional y
;para implementar funciones recursivas. Además, resultaba conveniente para la estructura de stackoverflow, para organizar los usuarios,
;las preguntas y sus correspondientes respuestas.

;Capa constructor.
;Dom: usuarios, un usuario, preguntas y 2 enteros (correlativo preguntas y resepuestas).
;Rec: un stack.
(define emptyStack null)
(define stack(lambda(perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
               (if(and(usuarios? perfiles)(usuario? perfilActivo)(preguntas? listaPreguntas)(integer? correlativoPreg)(integer? correlativoRes))
                  (list perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
                  emptyStack
                  )
               )
  )

;Capa selector.
;Dom: todas las funciones de la capa selector reciben como entrada un stack.
;____

;Rec: usuarios.
;Entrega la lista de usuario's que contiene el stack.
(define getUsuarios(lambda(stack)(car stack)))

;Rec: un usuario.
;Entrega al usuario que tiene sesión activa en el stack.
(define getActivo(lambda(stack)(car(cdr stack))))

;Rec: preguntas.
;Entrega la lista de pregunta's que contiene el stack.
(define getPreguntas(lambda(stack)(car(cdr(cdr stack)))))

;Rec: un entero.
;Entrega un número que índica el ID de la siguiente pregunta si esta se crea, se basa en el ID de la última pregunta.
(define getCorrPreg(lambda(stack)(car(cdr(cdr(cdr stack))))))

;Rec: un entero.
;Entrega un número que índica el ID de la siguiente respuesta si esta se crea, se basa en el ID de la última respuesta.
(define getCorrRes(lambda(stack)(car(cdr(cdr(cdr(cdr stack)))))))
;____

;Capa pertenencia.
;Dom: un lista, candidato a stack.
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

;Dom: un stack y un string(nombre).
;Rec: un stack actualizado.
;La función agrega a un usuario a la sesión activa(iniciada) del stack, utiliza la función "getUsuarios" para encontrar al usuario
;por su nombre en usuarios del stack y lo agrega al perfil activo.
(define agregarAactivo(lambda(stack1 nombre)
                         (stack (getUsuarios stack1)(getUsuario nombre (getUsuarios stack1))(getPreguntas stack1)(getCorrPreg stack1)(getCorrRes stack1))))



;Dom: un stack y un pregunta.
;Rec: un stack actualizado.
;La función actualiza el stack agregando una nueva pregunta, aumentando su correlativo de preguntas y eliminando al usuario de sesión activa.
;Usa la función recursiva de cola "agregarElemento" para agregar la pregunta a preguntas y de esta forma actualizar el stack, pues es
;la forma más rápido de hacerlo.
(define actualizarStackPreg(lambda(stack1 pregunta)
                             (stack (getUsuarios stack1) emptyUser (agregarElemento (getPreguntas stack1) pregunta) (+ 1 (getCorrPreg stack1)) (getCorrRes stack1))))



;Dom: un stack, un booleano, una función(será la función getQuestion) y un entero (id de pregunta).
;Rec: un stack actualizado.
;Actualizar un stack cuando el voto a una pregunta.
;la función reescribe el stack modificando sus usuarios al cambiar la reputación del autor de la pregunta con la función 'actualizarUsuariosReputacionVotPreg',
;elimina al usuario de sesión activa y modifica las preguntas al agregar el voto en la pregunta con la función 'actualizarPreguntasVot'.
(define actualizarStackVotPreg(lambda(stack1 booleano funcion idPreg)
                                (stack
                                 ;se actualiza usuarios...
                                 ;la función recibe como parametro la lista de usuarios del stack, el nombre del autor de la pregunta y el booleano(true V+/false V-).
                                 (actualizarUsuariosReputacionVotPreg (getUsuarios stack1) (getAutorPreg ((funcion idPreg)(getPreguntas stack1))) booleano)
                                 ;se elimina usuario activo.
                                 emptyUser
                                 ;se actualizan preguntas...
                                 ;la función recibe como parametro la lista de preguntas del stack, el id de la pregunta y el booleano(true V+/false V-).
                                 (actualizarPreguntasVot (getPreguntas stack1) idPreg booleano)
                                 (getCorrPreg stack1)(getCorrRes stack1))
                                )
  )


;Dom: un stack, un booleano, una función(será la función getAnswer evaluada con el id de respuesta) y un entero (id de pregunta).
;Rec: un stack actualizado.
;Actualiza el stack cuando el voto esta destinado a una respuesta.
;la función reescribe el stack modificando sus usuarios al cambiar la reputación del autor de la pregunta y la del votador con la función 'actualizarUsuariosReputacionVotRes',
;elimina al usuario de sesión activa y modifica las preguntas al agregar el voto en una respuesta de las respuestas de una de sus pregunta's con la función 'actualizarPreguntasVot'.
(define actualizarStackVotRes(lambda(stack1 booleano funcion idPreg)
                               (stack
                                ;se actualiza usuarios...
                                ;la función recibe como parametro la lista de usuarios del stack, el nombre del autor de la pregunta, el nombre del votador (usuario activo)
                                ;y el booleano(true V+/false V-).
                                (actualizarUsuariosReputacionVotRes (getUsuarios stack1) (getAutorRes ((funcion idPreg)(getPreguntas stack1)))
                                                                    (getNomUser (getActivo stack1)) booleano)
                                ;se elimina usuario activo.
                                emptyUser
                                ;se actualizan preguntas...
                                 ;la función recibe como parametro la lista de preguntas del stack, el id de la pregunta, la respuesta y el booleano(true V+/false V-).
                                (actualizarPreguntasVotRes (getPreguntas stack1) idPreg ((funcion idPreg)(getPreguntas stack1)) booleano)
                                (getCorrPreg stack1)(getCorrRes stack1))
                               )
  )


;Dom: un stack.
;Rec: un string.
;La función muestra el stack como un string, usando string-append junto con las funciones 'mostrarUsuarios', 'mostrarPreguntas' para transformar sus elementos en
;strings y luego unirlos.
(define mostrarStack(lambda(stack1)
                      (string-append "Stack overflow:\n"
                            "PERFILES DE USUARIOS:\n"
                            (mostrarUsuarios (getUsuarios stack1))
                            "\nPREGUNTAS:\n"
                            (mostrarPreguntas (getPreguntas stack1)))))



;EJEMPLOS NECESARIOS PARA EJMPLOS DE FUNCIONES MAIN:

(define stackPrueba(stack usuarios1 emptyUser preguntas1 3 3))
(provide stackPrueba)
