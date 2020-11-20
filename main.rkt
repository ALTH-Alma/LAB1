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
;Sino: la función currifcada evaluada en el stack sin modificar.
;La función permite iniciar sesión a un usuario registrado y autentificado, además, permite la ejecución
;de comandos concretos dentro del stack (operaciones).
;Utiliza una función de orden superior y recursiva de cola llamada "get" utilizada en la función "getUsuario"
;de la función "agregarAactivo".

(define login(lambda(stack1 nombre contraseña operacion)
               (if (and (stack? stack1)(string? nombre)(string? contraseña)) ;si los datos ingresados son correctos...
                   (if(autentificar nombre contraseña (getUsuarios stack1)) ;y si se encuntra al usuario y su contraseña es correcta..
                      (operacion (agregarAactivo stack1 nombre)) ;se evalua la operación currificada con el stack actualizado(gracias a la función agregarAactivo). 
                     (operacion stack1) ;sino se retorna la operación con el stack sin actualizar.
                      )
                   (operacion stack1) ;si los datos ingresados son incorrectos se retorna la operación con el stack sin actualizar.
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
                 (if (and (stack? stack1)(date? fecha)(contenidoPreg? contenido)(list? etiquetas)(not(null? (getActivo stack1)))) ;si los datos ingresados son correctos....
                     ;actualizo stack.
                     (actualizarStackPreg stack1 (pregunta (getCorrPreg stack1)(getNomUser (getActivo stack1)) fecha contenido etiquetas "Abierta" 0 0 0 emptyReward 0 emptyAnswers))
                     (stackErrorDatos stack1) ;sino entrego se entrega el stack sin modificación.
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
                            (stackErrorDatos stack1)) ;sino se retorna el stack de entrada.
                        (stackErrorDatos stack1)) ;si los datos eran incorrectos se retorna el stack de entrada.
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
                         (stackErrorDatos stack1));sino se retorna el stack tal cual.
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
                         (stackErrorDatos stack1)); sino cumplio las condiciones se retorna el stack tal cual.
                       (stackErrorDatos stack1)); si los datos de entrada eran incorrectos se retorna el stack tal cual.
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
                               ; si el votador no es autor de la pregunta...
                               (if (not (eqv? (getNomUser (getActivo stack1)) (getAutorPreg ((funcion idPreg )(getPreguntas stack1)))))
                                   ;se actualiza el stack con la función 'actualizarStackVotPreg'.
                                   (actualizarStackVotPreg stack1 booleano funcion idPreg)
                                   (stackErrorDatos stack1)) ;si el votador es el autor no se realiza el voto.
                               ;si el usuario no ingreso un getQuestion, se asume que ingreso un getAnswer...
                               (if (not(null? ((funcion idPreg)(getPreguntas stack1)))) ; y si la respuesta a la pregunta existe...
                                   ;y el autor de la respuesta no es el votador...
                                   (if (not (eqv? (getNomUser (getActivo stack1)) (getAutorRes ((funcion idPreg )(getPreguntas stack1)))))
                                       ;se actualiza el stack con la función 'actualizarStackVotRes'.
                                       (actualizarStackVotRes stack1 booleano funcion idPreg)
                                       (stackErrorDatos stack1)); ;si el votador es el autor no se realiza el voto.
                                   (stackErrorDatos stack1))); si no existe la respuesta en el stack, se retorna el stack tal cual.
                           (stackErrorDatos stack1)); si no existe la pregunta en el stack, se retorna el stack tal cual.
                       (stackErrorDatos stack1));si los datos ingresados eran incorrectos se retorna el stack tal cual.
                    )
                  )
                )
              )
  )
  

;SECCIÖN EJEMPLOS:

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
;2)no se realiza el cambio en el stack, pues la pregunta ya tenia recompensa ofrecida.
(((login stackPrueba "Ana" "A1234" reward) 0) 15)
l
;3)no se realiza el cambio en el stack, pues la pregunta no existe y el monto supera la reputación de Ana.
(((login stackPrueba "Ana" "A1234" reward) 5) 100)


;7 ANSWER:
l
;1)se agrega una respuesta, pues se inicia sesión correctamente, los datos de entrada son correctos y existe la pregunta.
((((login stackPrueba "Pedro" "P340" answer)(date 12 12 2020)) 1) "Puedes usar QPalette" (list "imagen" "gráfica"))
l
;2)no se agrega la respuesta, pues la pregunta que se desea contestar no existe.
((((login stackPrueba "Pedro" "P340" answer)(date 12 12 2020)) 5) "Puedes usar QPalette" (list "imagen" "gráfica"))
l
;3)no se agrega la respuesta, pues no se inicia sesión correctamente y los datos de entragados no son correctos
((((login stackPrueba "pedrito" "P34" answer)"12/12/2020") 1) "Puedes usar QPalette" (list "imagen" "gráfica"))


;8 ACCEPT:
l
;1)se acepta la pregunta, pues el usuario inicia sesión exitosamente, los datos de entrada son correctos, existe la pregunta,
;existe la respuesta y el usuario activo es autor de la pregunta y no es autor de la respuesta.
(((login stackPrueba "Maria" "Maria1999" accept)0)1)
l
;2)no se acepta la pregunta pues la respuesta no corresponde a la pregunta.
(((login stackPrueba "Maria" "Maria1999" accept)0)2)
l
;3)no se acepta la pregunta pues el usuario activo no es autor de la pregunta.
(((login stackPrueba "Maria" "Maria1999" accept)1)2)


;9 STACK->STRING:
l
;1)la función genera el string de todo el stack.
(stack->string stackPrueba)
;Así se ve con display.
(display (stack->string stackPrueba))
l
;2)la función genera el string de los elementos de usuario activo y sus preguntas.
(login stackPrueba "Maria" "Maria1999" stack->string)
;Así se ve con display.
(display (login stackPrueba "Maria" "Maria1999" stack->string))
l
;2)la función genera el string de los elementos de usuario activo y sus preguntas.
(login stackPrueba "Ana" "A1234" stack->string)
;Así se ve con display.
(display (login stackPrueba "Ana" "A1234" stack->string))


;VOTE:
l
;1)se realiza el voto, pues todas las entradas son correctas, existe la pregunta y la respuesta, el usuario inicia sesión exitosamente y no es el autor de la respuesta.
((((login stackPrueba "Maria" "Maria1999" vote)(getAnswer 1))0)false)
l
;2)se realiza el voto, pues todas las entradas son correctas, existe la pregunta, el usuario inicia sesión exitosamente y no es autor de la pregunta.
((((login stackPrueba "Maria" "Maria1999" vote)getQuestion)1)true)
l
;3)no se realiza el voto, pues no existe la respuesta.
((((login stackPrueba "Maria" "Maria1999" vote)(getAnswer 9))1)false)











