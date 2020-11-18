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

;___________________________

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
                     (operacion stack1) ;sino se retorna la operación.
                      )
                   (operacion stack1) ;si los datos ingresados son incorrectos se retorna la operación.
                   )
               )
  )
                  
;____________________

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
                 (if (and (stack? stack1)(date? fecha)(contenidoPreg? contenido)(list? etiquetas)(not(null? (getActivo stack1)))) ;si los datos ingresados son correctos.... actualizo stack.
                     (actualizarStackPreg stack1 (pregunta (getCorrPreg stack1)(getNomUser (getActivo stack1)) fecha contenido etiquetas "Abierta" 0 0 0 emptyReward 0 emptyAnswers))
                     stack1 ;sino entrego un mensaje.
                     )
                 )
               )
             )
  )
  
;_________________

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
                    ;si existe la pregunta en la lista de preguntas, si el monto de recompensa es real (mayor que 0), el stack de entrada es un stack, existe usuario con sesión activa...
                    (if (and (existePregunta? idPreg (getPreguntas stack1))(> montoRecompensa 0)(stack? stack1)(not(null? (getActivo stack1))))
                        ;y si el usuario activo puede ofrecer esa recompensa y si no existe ya una recompensa por la pregunta...
                        (if (and (puedeOfrecerRecompensa? (getActivo stack1) montoRecompensa) (eqv? emptyReward (getRecompensa ((getQuestion idPreg)(getPreguntas stack1)))))
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

;___________________________

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
                      (if(and (integer? idPreg)(existePregunta? idPreg (getPreguntas stack1))(stack? stack1)(date? fecha)(string? contenidoRes)(list? etiquetas)(not(null? (getActivo stack1))))
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
                  
;__________________

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
                    (if(and (stack? stack1)(integer? idPreg)(integer? idRes)(not(null? (getActivo stack1))))
                       ;y si la pregunta existe en preguntas del stack...
                       (if(and (existePregunta? idPreg (getPreguntas stack1))
                               ;, la respuesta existe en las respuestas de la pregunta...
                               (existeRespuestaEnPreg? idRes idPreg (getPreguntas stack1))
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

;_________________________

;Función 9: string->stack.
;Dom: un stack.
;Rec: un string.
;La función retorna un stack convertido en string si no hay usuario en sesión activa y muestra a un usuario, sus elementos y preguntas si hay sesión activa y es él.

(define stack->string(lambda(stack1)
                       (if (stack? stack1) ;si el dato ingresado es correcto.
                           (if (not(null? (getActivo stack1))) ;y si existe un usuario activo.
                               ;se muestra al usuario y sus elementos como un string unido a todas las preguntas de las que sea autor.
                               (string-append "Usuario activo:\n"
                                              (mostrarUsuario (getActivo stack1))
                                              "\nPreguntas:\n"
                                              (mostrarPregUsuario (getNomUser (getActivo stack1)) (getPreguntas stack1)))
                               ;sino se muestra todo el stack como un string.
                               (mostrarStack stack1))
                           stack1);si el dato ingresados era incorrecto se retorna el dato.
                       )
  )

;________________________

;Función 10: vote.
;Dom: un stack.
;Rec:
;retorna una función currificada que opera sobre los argumentos función(getQustion o getAnswer evaluada con el id de respuesta), entero(id de pregunta)
;y un booleano (determina el tipo de voto, true(voto positivo), false(voto negativo)).
;El retorno final de la función es una versión actualizada del stack, donde se vota por una respuesta o pregunta, se elimina al usuario en la sesión
;activa y se realizan todos los cambios que conlleva votar. Estos son, agregar el voto en la pregunta o respuesta,
;si se vota a favor de una pregunta o respuesta el autor de ella gana +10 puntos de reputación, si se vota en contra de una pregunta el autor pierde 2
;puntos y si se vota en contra de una respuesta el autor pierde 2 puntos y el votador(usuario activo) pierde 1 punto.
                    
(define vote(lambda(stack1)
              (lambda(funcion)
                (lambda(idPreg)
                  (lambda(booleano)
                    (if(and(stack? stack1)(integer? idPreg)(boolean? booleano)(not(null? (getActivo stack1)))) ;si los datos ingresados son correctos...
                       (if (existePregunta? idPreg (getPreguntas stack1)) ; ,si la pregunta con ese id existe en las preguntas...
                           (if (eqv? getQuestion funcion) ;y si se ingreso un getQuestion...
                               ;se actualiza el stack con la función 'actualizarStackVotPreg'.
                               (actualizarStackVotPreg stack1 booleano funcion idPreg)
                               ;si el usuario no ingreso un getQuestion, se asume que ingreso un getAnswer..
                               (if (not(null? ((funcion idPreg)(getPreguntas stack1)))) ; y si la respuesta a la pregunta existe.
                                   ;se actualiza el stack con la función 'actualizarStackVotRes'.s
                                   (actualizarStackVotRes stack1 booleano funcion idPreg)
                                   stack1)) ;sino existia la respuesta 
                           stack1); sino existe la pregunta en el stack, se retorna el stack tal cual.s
                       stack1);si los datos ingresados era incorrectos se retorna el stack tal cual.
                    )
                  )
                )
              )
  )



;EJEMPLOS:

;2 REGISTER:
stackPrueba
l
(register stackPrueba "Alma" "hi123") ;1)se registra, usuario valido.
l
(register stackPrueba "Maria" "MLopez");2)no se registra, nombre existente.
l
(register stackPrueba "pedro" "P1997");3)se registra, usuario valido (existe Pedro pero con mayuscula).

;3 LOGIN:
l
(login stackPrueba "Ana" "A1234" ask);1)se inicia sesión.
l
(login stackPrueba "Ana" "A123" ask);2)no se inicia sesión (contraseña invalida).
l
(login stackPrueba "Carla" "A1234" ask);3)no se inicia sesión (usuario no existente).


;4 ASK:
l
;1)entrada valida, función genera la pregunta de manera correcta.
(((login stackPrueba "Ana" "A1234" ask)(date 30 10 22))(contenidoPreg "FUNCIONA?" "quiero saber si esta operacion funciona correctamente")(list "prueba" "programa")) 
l
;2)no realiza cambios, usuario no se autentifica, por lo que no existe usuario activo.
(((login stackPrueba "Maria" "Maria199" ask)(date 30 10 22))(contenidoPreg "FUNCIONA?" "quiero saber si esta operacion funciona correctamente")(list "prueba" "programa"))
l
;3)no realiza cambios, datos de entrada incorrectos.
(((login stackPrueba "Ana" "A1234" ask)(date 30 10 22))(contenidoPreg "FUNCIONA?" "quiero saber si esta operacion funciona correctamente") "prueba")

;6 REWARD:
l
;1) ofrece la recompensa, pues se inicia bien sesión, existe la pregunta, Ana tiene suficiente reputación y la pregunta no tenia recompensa previa.
(((login stackPrueba "Ana" "A1234" reward) 1) 15)
l
;2)no se realiza el cambio en el stack, pues la respuesta ya tenia recompensa ofrecida.
(((login stackPrueba "Ana" "A1234" reward) 0) 15)
l
;3)no se realiza el cambio en el stack, pues la respuesta no existe.
(((login stackPrueba "Ana" "A1234" reward) 5) 15)

;7 ANSWER:
;((((login stackPrueba "Ana" "A123" answer)(date 12 12 2020)) 1) "Nose" (list "jahsjoiuu" "123"))

;Ejemplo:
;(((login stackPrueba "Maria" "Maria199" accept)1)0)




;Ejemplo:

l
;stack1
l
;((((login stackPrueba "Maria" "Maria1999" vote)(getAnswer 1))0)false)



                                 











