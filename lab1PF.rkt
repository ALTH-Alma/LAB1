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

;Función 1: register.
;Dom: un TDAstack y 2 string (nombre y contraseña).
;Rec: una estructura stack actualizada. (con el nuevo usuario registrado)
;Función que permite registrar a un nuevo usuario en el stack, para esto el nombre ingresado
;por el usuario que desea registrarse no puede pertener a ningun usuario ya existente en el stack.
;Emplea recursion natural utilizando la función "agregarElemento" ubicada en el archivo Funcionesgenerales.rkt.

(define register(lambda(stack1 nombre contraseña)
                  (if (and (stack? stack1)(string? nombre)(string? contraseña)) ;si los datos ingresados son correctos.
                      
                      (if (noExisteNombre? nombre (getUsuarios stack1)) ;se confirma que el nombre no pertenezca a ningun usario existente.
                          (stack (agregarElemento (getUsuarios stack1) (usuario nombre contraseña 0 emptyList)) (getActivo stack1)
                                 (getPreguntas stack1)(getCorrPreg stack1)(getCorrRes stack1)) ;se agrega el usuario y se actualiza stack.
                          stack ;Sino re retorna el stack tal cual.
                          )
                      )
                  stack ;si los datos de entrada son incorrectos se retorna el stack tal cual.
                  )
  )


;Ejemplo:
(define l(list "-----------------------------------------------------------------"))
l
(register stack1 "Alma" "hi123")


;Función 2: login.
;Dom: un TDAstack, dos strings (nombre y contraseña) y una operación(función).
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



;FUNCION 3:funcion ask.

;Ejemplo:
(define l3(list "......................................"))
l3
    
(define actualizarStackPreg(lambda(sta pregunta)
                             (stack (getUsuarios sta) emptyUser (agregarElemento (getPreguntas sta) pregunta) (+ 1 (getCorrPreg sta)) (getCorrRes sta))))
  
  
  
                       
(define ask(lambda(stack)
             (lambda(fecha)
               (lambda(contenido etiquetas)
               (actualizarStackPreg stack (pregunta (getCorrPreg stack)(getNomUser (getActivo stack)) fecha contenido etiquetas "abierta" 0 0 0 emptyReward 0 emptyAsk))))))


;Ejemplo:
l
(((login stack1 "Ana" "A1234" ask)(date 30 10 11))(contenidoPreg "FUNCIONA?" "quiero saber si esta operacion funciona correctamente")(list "prueba" "programa"))
  

;Función 6: ofrecer recompensa:
(define puedeOfrecer(lambda(user montoRecompensa)
                                (if(>= (getReputacion user) montoRecompensa)
                                   true
                                   false
                                   )))

(define emptylist null)
(define actualizar(lambda(lista id getId modificar agregado)
                                  (if (null? lista)
                                      emptylist
                                      (if(eqv? id (getId (primerElemento lista)))
                                         (cons (modificar (primerElemento lista) agregado)(siguientesElementos lista))
                                         (cons (primerElemento lista) (actualizar (siguientesElementos lista) id getId modificar agregado))
                                         ))))

(define modificarReputacion(lambda(user modificacion) ;Donde moficacion es un par que indica(operacion.monto). Ejemplo: (- . 20), (+ . 10), etc.
                                     (usuario (getNomUser user) (getContraseña user) ((car modificacion) (getReputacion user) (cdr modificacion)) (getReferencias user))))

(define actualizarUsuariosReputacion(lambda(listaUsuarios nombre operacion)
                                      (actualizar listaUsuarios nombre getNomUser modificarReputacion operacion)))
;Ejemplo:
  l
  l
  l
(actualizarUsuariosReputacion usuarios1 "Ana" (cons - 5))


(define modificarPregRecompensa(lambda(ask recompensa)
                             (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                              (getEstadoPreg ask) (getVisualizacionesPreg ask) (getVfavorPreg ask) (getVcontraPreg ask)
                               recompensa (getReportesPreg ask) (getRespuestas ask))))

(define actualizarPreguntasRecompensa(lambda (listaPreguntas id recompensa)
                                       (actualizar listaPreguntas id getIdPreg modificarPregRecompensa recompensa)))

;Ejemplo:
(actualizarPreguntasRecompensa preguntas1 1 (recompensa "pepe" 20))

;Rec: una lista de preguntas, con la pregunta actualizada (agregada la recompensa).

(define reward(lambda(sta)
  (lambda(idPreg)
    (lambda(montoRecompensa)
     (if (puedeOfrecer (getUsuario (getNomUser (getActivo sta))(getUsuarios sta)) montoRecompensa)
        (stack (actualizarUsuariosReputacion (getUsuarios sta) (getNomUser (getActivo sta)) (cons - montoRecompensa))
               emptyUser
               (actualizarPreguntasRecompensa (getPreguntas sta) idPreg (recompensa (getNomUser (getActivo sta)) montoRecompensa))
               (getCorrPreg sta)(getCorrRes sta))
        sta
        )))))

;Ejemplo:
l
l
(((login stack1 "Ana" "A1234" reward) 1) 15)

;Funcion7:answer; responder una pregunta.------------------------------------------------------------------


;Dom: un TDApregunta(lista), respuestas.
;Rec: un TDApregunta actualizado, con la respuesta agregada a su TDA respuestas(lista).
(define agregarRespuestaApreg(lambda(ask answer)
                          (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                              (getEstadoPreg ask) (getVisualizacionesPreg ask) (getVfavorPreg ask) (getVcontraPreg ask)
                               (getRecompensa ask) (getReportesPreg ask)(agregarElemento (getRespuestas ask) answer))))

;Dom: TDApreguntas (lista de preguntas), int identificador de pregunta, TDApregunta nueva pregunta.
;Rec: un TDApreguntas actualizado, con la respuesta a la pregunta(id) agregada.
(define actualizarPreguntasNewAns(lambda(listaPreg idPreg nuevPreg)
                            (actualizar listaPreg idPreg getIdPreg agregarRespuestaApreg nuevPreg)))


;ejemplo:
;(actualizarPreguntasNewAns

(define answer(lambda(sta)
                (lambda(fecha)
                  (lambda(idPreg)
                     (lambda(contenidoRes etiquetas)
                  (stack (getUsuarios sta)
                          emptyUser
                          (actualizarPreguntasNewAns (getPreguntas sta) idPreg
                             (respuesta (getCorrRes sta) (getNomUser (getActivo sta)) fecha contenidoRes etiquetas "" 0 0 0))
                          (getCorrPreg sta)
                          (+ 1 (getCorrRes sta))))))))
                  
;Ejemplo:
l
((((login stack1 "Ana" "A1234" answer)(date 12 12 2020)) 1) "Nose" (list "jahsjoiuu" "123"))

;Función 8: accept, permite aceptar una pregunta.

(define getQuestion(lambda(id)
                     (lambda(listaPreg)
                     (get id getIdPreg listaPreg))))


(define esResDePreg(lambda(listaRespuestas idRes)
                    (if (null? listaRespuestas)
                         false
                        (if (eqv? idRes (getIdRes (primerElemento listaRespuestas)))
                            true
                            (esResDePreg (siguientesElementos listaRespuestas) idRes)
                            )
                        )
                     )
  )
;respuesta -->accep si, pregunta recom 0.
;Rec: una respuesta actualizada
(define modificarResAccept(lambda(res aceptacion)
                        (respuesta (getIdRes res) (getAutorRes res) (getFechaRes res) (getContenidoRes res) (getEtiquetasRes res) aceptacion (getVfavorRes res)
                                   (getVcontraRes res) (getReportesRes res))))
;Dom: lista de respuestas e identificador res
;Rec: Una lista de respuestas actualizada
(define actualizarRespuestasAccept(lambda(listaRes idRes)
                             (actualizar listaRes idRes getIdRes modificarResAccept "Aceptada")))

;Rec: una lista una pregunta y una lista de respuestas actualizada.
;Dom: un TDA pregunta actualizada, con sus respuestas actualizadas (respuesta aceptada) y sin recompensa.
(define modificarPregAccept(lambda(ask respuestasActualizada)
                          (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                              (getEstadoPreg ask) (getVisualizacionesPreg ask) (getVfavorPreg ask) (getVcontraPreg ask)
                               emptyReward (getReportesPreg ask) respuestasActualizada)))

;Dom: un TDA preguntas, 2 int: un identificadore de pregunta y uno de respuesta.
;Rec:TDA preguntas actualizado, con la pregunta actualizada(idPreg).
(define actualizarPreguntasAccept(lambda(listaPreg idPreg idRes)
                                  (if (null? listaPreg)
                                      emptylist
                                      (if(eqv? idPreg (getIdPreg (primerElemento listaPreg)))
                                         (cons
                                          (modificarPregAccept (primerElemento listaPreg) (actualizarRespuestasAccept(getRespuestas(primerElemento listaPreg)) idRes))
                                          (siguientesElementos listaPreg))
                                         (cons (primerElemento listaPreg) (actualizarPreguntasAccept (siguientesElementos listaPreg) idPreg idRes))
                                         ))))

                                 
                                                
;El autor de la pregunta, quien es el unico que puede aceptar respuestas a sus preguntas gana +2 de reputacion
;El autor de la respuesta que fue aceptada gana +15 por responder y +monto de recompensa en la pregunta.
(define actualizarUsuarios2Reput(lambda(listaUsuarios persona1 persona2 operacion1 operacion2)
                                  (if (null? listaUsuarios)
                                      emptyUser
                                      (if(eqv? persona2 (getNomUser (primerElemento listaUsuarios)))
                                         (cons (modificarReputacion (primerElemento listaUsuarios) operacion2)
                                               (actualizarUsuarios2Reput (siguientesElementos listaUsuarios)persona1 persona2 operacion1 operacion2))
                                         (if(eqv? persona1 (getNomUser (primerElemento listaUsuarios)))
                                            (cons (modificarReputacion (primerElemento listaUsuarios)operacion1)
                                                  (actualizarUsuarios2Reput (siguientesElementos listaUsuarios) persona1 persona2 operacion1 operacion2))
                                            (cons (primerElemento listaUsuarios)
                                                  (actualizarUsuarios2Reput (siguientesElementos listaUsuarios) persona1 persona2 operacion1 operacion2)))))))
;Ejemplo:
;(actualizarUsuariosAccept usuarios1 "Ana" "Maria" 10)


(define getAnswer(lambda(idRes)
                      (lambda(idPreg)
                        (lambda(listaPreg)
                        (get idRes getIdRes (getRespuestas ((getQuestion idPreg) listaPreg)))))))

;Ejemplo:
l
;(((getAnswer 1)0)preguntas1)
                        
                           
    
(define accept(lambda(sta)
                (lambda(idPreg)
                  (lambda(idRes)
                  (if(and (esResDePreg (getRespuestas ((getQuestion idPreg)(getPreguntas sta))) idRes) ; si la rspuesta corresponde a la pregunta
                          (eqv? (getNomUser (getActivo sta)) (getAutorPreg ((getQuestion idPreg )(getPreguntas sta))))) ;y es pregunta del usuario activo, puede acceptar
                     (stack (actualizarUsuarios2Reput (getUsuarios sta)(getAutorRes (((getAnswer idRes)idPreg)(getPreguntas sta)))(getNomUser (getActivo sta))
                                                      (cons + (+ 15 (getValorRecompensa(getRecompensa((getQuestion idPreg)(getPreguntas sta))))))
                                                      (cons + 2))
                            emptyUser
                            (actualizarPreguntasAccept (getPreguntas sta) idPreg idRes)
                            (getCorrPreg sta)
                            (getCorrRes sta))
                     sta)))))


;Ejemplo:
l
stack1
l
(((login stack1 "Maria" "Maria1999" accept)1)0)

;Función 10: vote: permite votar a favor o encontra de una pregunta o una respuesta.
;Actualizar usuarios.
(define actualizarUsuariosReputacionVotPreg(lambda(listaUsuarios nombre booleano)
                                             (if booleano
                                                 (actualizarUsuariosReputacion listaUsuarios nombre (cons + 10))
                                                 (actualizarUsuariosReputacion listaUsuarios nombre (cons - 2)))))

(define actualizarUsuariosReputacionVotRes(lambda(listaUsuarios autorRes votador booleano)
                                            (if booleano
                                                (actualizarUsuariosReputacion listaUsuarios autorRes (cons + 10))
                                                (actualizarUsuarios2Reput listaUsuarios votador autorRes (cons - 1) (cons - 2)))))
                                                                                     

;Actualizar una pregunta cuando se vota positivo o negativo, se suma un voto a donde corresponda
(define modificarPregVot(lambda(ask booleano)
                          (if booleano
                              (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                                        (getEstadoPreg ask) (getVisualizacionesPreg ask) (+ 1 (getVfavorPreg ask)) (getVcontraPreg ask)
                                        (getRecompensa ask) (getReportesPreg ask) (getRespuestas ask))
                              (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                                        (getEstadoPreg ask) (getVisualizacionesPreg ask) (getVfavorPreg ask) (+ 1 (getVcontraPreg ask))
                                        (getRecompensa ask) (getReportesPreg ask) (getRespuestas ask)))))

(define actualizarPreguntasVot(lambda(listaPreg idPreg booleano)
                                (actualizar listaPreg idPreg getIdPreg modificarPregVot booleano)))
                                
                   
;Actualizar un stack cuando el voto esta destinado a una pregunta.
(define actualizarStackVotPreg(lambda(sta booleano funcion idPreg)
                                (stack (actualizarUsuariosReputacionVotPreg (getUsuarios sta) (getAutorPreg ((funcion idPreg)(getPreguntas sta))) booleano)
                                       emptyUser
                                       (actualizarPreguntasVot (getPreguntas sta) idPreg booleano)
                                       (getCorrPreg sta)(getCorrRes sta))))

;Actualizar una respuesta cuando recibe un voto, positivo o negativo.
(define modificarResVot(lambda(res booleano)
                         (if booleano
                             (respuesta (getIdRes res) (getAutorRes res) (getFechaRes res) (getContenidoRes res) (getEtiquetasRes res) (getAceptacionRes res)
                                        (+ 1 (getVfavorRes res)) (getVcontraRes res) (getReportesRes res))
                             (respuesta (getIdRes res) (getAutorRes res) (getFechaRes res) (getContenidoRes res) (getEtiquetasRes res) (getAceptacionRes res)
                                        (getVfavorRes res) (+ 1 (getVcontraRes res)) (getReportesRes res)))))

  
;Dom: una lista de respuestas el id de la respuesta y un booleano.
;Rec: una lista de preguntas actualizada con el voto realizado.
(define actualizarResVot(lambda (listaRes idRes booleano)
                          (actualizar listaRes idRes getIdRes modificarResVot booleano)))

  
;Rec: una lista una pregunta y una lista de respuestas actualizada.
;Dom: un TDA pregunta actualizada, con sus respuestas actualizadas (con el voto agregado)
(define modificarPregVotRes(lambda(ask respuestasActualizada)
                             (pregunta (getIdPreg ask) (getAutorPreg ask) (getFechaPreg ask) (getContenidoPreg ask) (getEtiquetasPreg ask)
                                       (getEstadoPreg ask) (getVisualizacionesPreg ask) (getVfavorPreg ask) (getVcontraPreg ask)
                                       (getRecompensa ask) (getReportesPreg ask) respuestasActualizada)))
  


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
;(display (list "los usuarios del stack son:\n" (getUsuarios stack1)))
;Función que muestra los datos de un usuario.
(define mostrarElementosList(lambda(lista)
                              (if (null? lista)
                                  emptyList
                                  (if (null? (siguientesElementos lista))
                                      (cons (primerElemento lista)(cons "." null))
                                      (cons (primerElemento lista)(cons "," (mostrarElementosList (siguientesElementos lista))))))))

                                 
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











