#lang racket

(require "FuncionesGenerales.rkt")
(require "TDAdate.rkt")
(require "TDArecompensa.rkt")
(require "TDAcontenidoPregunta.rkt")
(require "TDArespuesta.rkt")
(require "TDArespuestas.rkt")
(require "TDAusuario.rkt")
(require "TDAusuarios.rkt")
(require "TDApregunta.rkt")
(require "TDApreguntas.rkt")
(require "TDAstack.rkt")

;DESARROLLO FUNCIONES:

;Función 2: register.
;Dom: un stack y 2 string (nombre y contraseña).
;Rec: una estructura stack actualizada. (con el nuevo usuario registrado)
;Función que permite registrar a un nuevo usuario en el stack, para esto el nombre ingresado
;por el usuario que desea registrarse no puede pertener a ningun usuario ya existente en el stack.
;Emplea recursion natural utilizando la función "agregarElemento" ubicada en el archivo Funcionesgenerales.rkt.

(define register(lambda(stack1 nombre contraseña)
                  (if (and (stack? stack1)(string? nombre)(string? contraseña)) ;si los datos ingresados son correctos.
                      (if (noExisteNombre? nombre (getUsuarios stack1)) ;se confirma que el nombre no pertenezca a ningun usario existente.
                          (stack (agregarElemento (getUsuarios stack1) (usuario nombre contraseña 0 emptyList)) (getActivo stack1)
                                 (getPreguntas stack1)(getCorrPreg stack1)(getCorrRes stack1)) ;se agrega el usuario y se actualiza stack.
                          stack1) ;Sino re retorna el stack tal cual.  
                      stack1) ;si los datos de entrada son incorrectos se retorna el stack tal cual.
                  )
  )


;Ejemplo:

;(register stack1 "Alma" "hi123")


;Función 3: login.
;Dom: un stack, dos strings (nombre y contraseña) y una operación(función).
;Rec:
;Si se autentifica el usuario: función currificada para la operacion de parametro de entrada,
;parcialmente evaluada con el stack actualizado(usuario autentificado y registrado en sesión activa).
;Sino: la operación.
;La función permite iniciar sesión a un usuario registrado y autentificado, además, permite la ejecución
;de comandos concretos dentro del stack (operaciones).
;Utiliza una función de orden superior y recursiva de cola llamada "get" utilizada en la función "getUsuario"
;de la función "agregarAactivo".

(define login(lambda(stack1 nombre contraseña operacion)
               (if (and (stack? stack1)(string? nombre)(string? contraseña)) ;si los datos ingresados son correctos...
                   (if(autentificar nombre contraseña (getUsuarios stack1)) ;y si se encuntra al usuario y su contraseña es correcta..
                      (operacion (agregarAactivo stack1 nombre)) ;se evalua la operación currificada con el stack actualizado(gracias a la función agregarAactivo). 
                      operacion ;sino se retorna la operación.
                      )
                   operacion ;si los datos ingresados son incorrectos se retorna la operación.
                   )
               )
  )
                  
;Ejemplo:
;(login stack1 "Ana" "A1234" ask)


;Función 4: ask.
;Dom: un stack.
;Rec:
;Retorna una función currificada que opera sobre los argumentos fecha y contendido pregunta + lista de etiquetas.
;El retorno final de la función es una versión actualizada del stack donde se registra la nueva pregunta, se elimina
;al usuario en la sesión activa del stack y aumenta el correlativo de pregunta.
;Función currificada que permite a un usuario con sesión iniciada en la plataforma realizar una nueva pregunta.
;Cada pregunta registra el autor de la misma (obtenido desde la sesión iniciada con login), fecha de publicación,
;la pregunta y 0 o más etiquetas (en este orden).
;La función usa la función actualizarSatckPreg que resive un stack y una nueva pregunta, para realizar la modificación.
                        
(define ask(lambda(stack1)
             (lambda(fecha)
               (lambda(contenido etiquetas)
                 (if (and (stack? stack1)(date? fecha)(contenidoPreg? contenido)(list? etiquetas)) ;si los datos ingresados son correctos.... actualizo stack.
                     (actualizarStackPreg stack1 (pregunta (getCorrPreg stack1)(getNomUser (getActivo stack1)) fecha contenido etiquetas "abierta" 0 0 0 emptyReward 0 emptyAnswers))
                     "Datos ingresados incorrectos" ;sino entrego un mensaje.
                     )
                 )
               )
             )
  )
  

;Ejemplo:
l
;(((login stack1 "Ana" "A1234" ask)(date 30 10 11))(contenidoPreg "FUNCIONA?" "quiero saber si esta operacion funciona correctamente")(list "prueba" "programa"))
  

;Función 6: reward.
;Dom: un stack.
;Rec:
;Retorna una función currificada que opera sobre los argumentos idPregunta y un entero(montoRecompensa).
;El retorno final de la función es una versión actualizada del stack, donde se agrega la recompensa a una
;pregunta, se descuenta el monto de la recompensa ofrecida al usuario en sesión activa (el monto queda
;retenido temporalmente en la pregunta) y se elimina al usuario en la sesión activa del stack.
;la función actualiza un stack agregando una respuesta y cambiando todo lo que ello conlleva
;(para ello usa las funciones 'actualizarUsuariosReputacion' y 'actualizarPreguntasRecompensa').

(define reward(lambda(stack1)
                (lambda(idPreg)
                  (lambda(montoRecompensa)
                    ;si existe la pregunta en la lista de preguntas, si el monto de recompensa es real (mayor que 0), el stack de entrada es un stack...
                    (if (and (existePregunta? idPreg (getPreguntas stack1))(> montoRecompensa 0)(stack? stack1))
                        ;y si el usuario activo puede ofrecer esa recompensa...
                        (if (puedeOfrecerRecompensa? (getActivo stack1) montoRecompensa)
                            ;se actualiza el stack y se realizan los cambios correspondientes.
                            (stack (actualizarUsuariosReputacion (getUsuarios stack1) (getNomUser (getActivo stack1)) (cons - montoRecompensa))
                                   emptyUser
                                   (actualizarPreguntasRecompensa (getPreguntas stack1) idPreg (recompensa (getNomUser (getActivo stack1)) montoRecompensa))
                                   (getCorrPreg stack1)(getCorrRes stack1))
                            stack1) ;sino se retorna el stack de entrada.
                        stack1) ;si los datos eran incorrectos se retorna el stack de entrada.
                    )
                  )
                )
  )

;Ejemplo:
;(((login stack1 "Ana" "A1234" reward) 1) 15)

;Función 7: answer.
;Dom: un stack.
;Rec:
;Retorna una función currificada que opera sobre los argumentos date(fecha), entero (idPreg) y un string(contenido respuesta)+ una lista de etiquetas.
;El retorno final de la función es una versión actualizada del stack, donde se agrega una respuesta a una pregunta, se elimina al usuario en la sesión
;activa del stack y se aumenta el correlativo de respuestas.
;La función agrega la respuesta gracias a la función 'actualizarPregNewAns' ubicada en TDApreguntas.rkt.
(define answer(lambda(stack1)
                (lambda(fecha)
                  (lambda(idPreg)
                    (lambda(contenidoRes etiquetas)
                      ;si los datos ingresados son correctos...
                      (if(and (integer? idPreg)(existePregunta? idPreg (getPreguntas stack1))(stack? stack1)(date? fecha)(string? contenidoRes)(list? etiquetas))
                         ;se actualiza el stack agregando la respuesta, eliminando al usuario de activo y aumentando el correlativo de preguntas.
                         (stack (getUsuarios stack1)
                                emptyUser
                                (actualizarPreguntasNewAns (getPreguntas stack1) idPreg
                                                           (respuesta (getCorrRes stack1) (getNomUser (getActivo stack1)) fecha contenidoRes etiquetas "" 0 0 0))
                                (getCorrPreg stack1)
                                (+ 1 (getCorrRes stack1)))
                         stack1);sino se retorna el stack tal cual.
                      )
                    )
                  )
                )
  )
                  
;Ejemplo:
;((((login stack1 "Ana" "A1234" answer)(date 12 12 2020)) 1) "Nose" (list "jahsjoiuu" "123"))

;Función 8: accept.
;Dom: un stack.
;Rec:
;retorna una función currificada que opera sobre los argumentos entero(id pregunta), entero(id de respuesta).
;El retorno final de la función es una versión actualizada del stack, donde se acepta una respuesta de una pregunta, se elimina al usuario en la sesión
;activa y se realizan todos los cambios que conlleva aceptar la respuesta. Estos son, se elimina la recompensa en la pregunta si es que exitia,
;el autor de la respuesta gana +15 puntos de reputación además del total del monto de recompensa y el autor de la pregunta gana 2 puntos de reputación.
(define accept(lambda(stack1)
                (lambda(idPreg)
                  (lambda(idRes)
                    ;si los datos ingresados son correctos...
                    (if(and (stack? stack1)(integer? idPreg)(integer? idRes))
                       ;y si la pregunta existe en preguntas del stack...
                       (if(and (existePregunta? idPreg (getPreguntas stack1))
                               ;, la respuesta existe en las respuestas de la pregunta...
                               (existeRespuesta? (getRespuestas ((getQuestion idPreg)(getPreguntas stack1))) idRes)
                               ;, el usuario activo es el autor de la pregunta...
                               (eqv? (getNomUser (getActivo stack1)) (getAutorPreg ((getQuestion idPreg )(getPreguntas stack1))))
                               ;y el usuario activo no es el autor de la respuesta...
                               (not(eqv? (getNomUser (getActivo stack1)) (getAutorRes (((getAnswer idRes)idPreg)(getPreguntas stack1))))))
                          ;entonces se puede aceptar la respuesta y se puede actualizar el stack
                          (stack (actualizarUsuarios2Reput (getUsuarios stack1)(getAutorRes (((getAnswer idRes)idPreg)(getPreguntas stack1)))(getNomUser (getActivo stack1))
                                                           ;el autor de la respuesta gana 15 + recompensa.
                                                           (cons + (+ 15 (getValorRecompensa(getRecompensa((getQuestion idPreg)(getPreguntas stack1))))))
                                                           ;el autor de la pregunta gana 2.
                                                           (cons + 2))
                                 emptyUser ;se elimina el usuario de sesión activa.
                                 (actualizarPreguntasAccept (getPreguntas stack1) idPreg idRes) ;se actualiza el estado de la respuesta "Aceptada".
                                 (getCorrPreg stack1)
                                 (getCorrRes stack1))
                          stack1); sino cumplio las condiciones se retorna el stack tal cual.
                       stack1); si los datos de entrada eran incorrectos se retorna el stack tal cual.
                    )
                  )
                )
  )


;Ejemplo:
;(((login stack1 "Maria" "Maria1999" accept)1)0)

;Función 10: vote.
                 
;Actualizar un stack cuando el voto esta destinado a una pregunta.
(define actualizarStackVotPreg(lambda(sta booleano funcion idPreg)
                                (stack (actualizarUsuariosReputacionVotPreg (getUsuarios sta) (getAutorPreg ((funcion idPreg)(getPreguntas sta))) booleano)
                                       emptyUser
                                       (actualizarPreguntasVot (getPreguntas sta) idPreg booleano)
                                       (getCorrPreg sta)(getCorrRes sta))))

  


;Dom: un TDA preguntas, 2 int: un identificadore de pregunta y uno de respuesta.
;Rec:TDA preguntas actualizado, con la pregunta actualizada(idPreg).
(define actualizarPreguntasVotRes(lambda(listaPreg idPreg res booleano)
                                   (if (null? listaPreg)
                                       emptylist
                                       (if(eqv? idPreg (getIdPreg (primerElemento listaPreg)))
                                          (cons
                                           (modificarPregVotRes(primerElemento listaPreg) (actualizarResVot(getRespuestas(primerElemento listaPreg)) (getIdRes res) booleano))
                                           (siguientesElementos listaPreg))
                                          (cons (primerElemento listaPreg) (actualizarPreguntasVotRes (siguientesElementos listaPreg) idPreg res booleano))
                                          ))))


;Actualiza el stack cuando el voto esta destinado a una respuesta  
(define actualizarStackVotRes(lambda(sta booleano funcion idPreg)
                               (stack (actualizarUsuariosReputacionVotRes (getUsuarios sta) (getAutorRes ((funcion idPreg)(getPreguntas sta)))
                                                                          (getNomUser (getActivo sta)) booleano)
                                      emptyUser
                                      (actualizarPreguntasVotRes (getPreguntas sta) idPreg ((funcion idPreg)(getPreguntas sta)) booleano)
                                      (getCorrPreg sta)(getCorrRes sta))))

                       
(define vote(lambda(sta)
              (lambda(funcion)
                (lambda(idPreg)
                  (lambda(booleano)
                    (if (eqv? getQuestion funcion)
                        (actualizarStackVotPreg sta booleano funcion idPreg)
                        (actualizarStackVotRes sta booleano funcion idPreg)))))))
;Ejemplo:

l
stack1
l
((((login stack1 "Maria" "Maria1999" vote)(getAnswer 1))0)false)


;Función 9: mostrar el stack como un string ordenado.

                                 
(define mostrarUsuario(lambda(user)
                        (list "Nombre usuario:"(getNomUser user)
                              "\nContraseña:"(getContraseña user)
                              "\nReputación:"(getReputacion user)"puntos"
                              "\nReferencias:"(mostrarElementosList (getReferencias user))"\n"
                              )))
;Ejemplo:
(mostrarUsuario u1)
(display (mostrarUsuario u1))

;Función para mostrar una lista de usuarios.
(define mostrarElementos(lambda(listaElementos funcionMostrar)
                         (if (null? listaElementos)
                             emptyList
                             (list "\n"
                                   (if (null? (siguientesElementos listaElementos))
                                       (cons (funcionMostrar (primerElemento listaElementos))(cons "\n" null))
                                       (cons (funcionMostrar (primerElemento listaElementos))(cons "\n" (mostrarElementos (siguientesElementos listaElementos) funcionMostrar))))))))

(define mostrarUsuarios(lambda(listaUsuarios)
                         (mostrarElementos listaUsuarios mostrarUsuario)))

  

;Ejemplo:
(mostrarUsuarios usuarios1)
(display (mostrarUsuarios usuarios1))


;Función para mostrar una lista de respuestas.
(define mostrarRespuestas(lambda(listaRes)
                           (mostrarElementos listaRes mostrarRespuesta)))

;Función para mostrar una pregunta.
(define mostrarPregunta(lambda(preg)
                         (list "ID Pregunta:"(getIdPreg preg)
                                "\nTitulo pregunta:"(getTituloPreg (getContenidoPreg preg))
                                "\nCuerpo pregunta:"(getCuerpoPreg (getContenidoPreg preg))
                                "\nAutor:"(getAutorPreg preg)
                                "\nFecha publicación:"(getFechaPreg preg)
                                "\nEtiquetas:"(mostrarElementosList (getEtiquetasPreg preg))
                                "\nEstado:"(getEstadoPreg preg)
                                "\nVisualizaciones:"(getVisualizacionesPreg preg)
                                "\nVotos a favor:"(getVfavorPreg preg)
                                "\nVotos en contra:"(getVcontraPreg preg)
                                "\nRecompensa:"(getValorRecompensa (getRecompensa preg))"puntos - Realizada por"(getOfertor (getRecompensa preg))
                                "\nReportes:"(getReportesPreg preg)
                                "\nRespuestas:\n"
                                (mostrarRespuestas (getRespuestas preg))
                                "\n")))

;Ejemplo:
(mostrarPregunta p1)
(display (mostrarPregunta p1))
(define mostrarPreguntas(lambda(listaPreg)
                           (mostrarElementos listaPreg mostrarPregunta)))

;Función que muestra todos las preguntas y sus respectivas respuestas de un usuario.
(define mostrarPregUsuario(lambda(nomUsuario listaPreg)
                            (if (null? listaPreg)
                                emptyList
                                (if (eqv? nomUsuario (getAutorPreg (primerElemento listaPreg)))
                                    (cons (mostrarPregunta (primerElemento listaPreg)) (mostrarPregUsuario nomUsuario (siguientesElementos listaPreg)))
                                    (mostrarPregUsuario nomUsuario (siguientesElementos listaPreg))))))
;Ejemplo:
(mostrarPregUsuario "Maria" preguntas1)
(display (mostrarPregUsuario "Maria" preguntas1))

(define mostrarElementosUsuarioActivo(lambda(usuario listaPreg)
                                       (list "Usuario activo:\n"
                                             (mostrarUsuario usuario)
                                             "\nPreguntas:\n"
                                             (mostrarPregUsuario (getNomUser usuario) listaPreg))))
;Ejemplo:
(mostrarElementosUsuarioActivo u1 preguntas1)
(display (mostrarElementosUsuarioActivo u1 preguntas1))

(define mostrarStack(lambda(sta)
                      (list "Stack overflow:\n"
                            "PERFILES DE USUARIOS:\n"
                            (mostrarUsuarios (getUsuarios sta))
                            "\nPREGUNTAS:\n"
                            (mostrarPreguntas (getPreguntas sta)))))
                      
                                             

(define stack->string(lambda(sta)
                       (if (not(null? (getActivo sta)))
                           (mostrarElementosUsuarioActivo (getActivo sta) (getPreguntas sta))
                           (mostrarStack sta))))
;Ejemplo:
(stack->string stack1)

;(list->string(stack->string stack1))











