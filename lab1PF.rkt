#lang racket
;TDAs stackoverflow.
;_______________________________________________________________________________________________--
;TDA date.
;Representacion: lista( int dia, int mes, int año)
;Capa constructor.
(define emptyDate null)
(define date(lambda( dia mes año)
              (if(and (integer? dia)(integer? mes)(integer? año))
                 (list dia mes año)
                 emptyDate)))
(date 1 2 2020)
;Capa selector.
(define getDia(lambda(date)(car date)))
(define getMes(lambda(date)(car(cdr date))))
(define getAño(lambda(date)(car(cdr(cdr date)))))

;Capa pertenencia.
(define date?(lambda(fecha)
               (if(and(list? fecha)(integer? (getDia fecha))(integer? (getMes fecha))(integer? (getAño fecha)))
                  true
                  false)))

(date? (date 2 3 2020))

;TDA reportes.
;Representación: par(int nReportes, lista de string reportes)
;Capa constructor.
(define emptyReport (cons 0 null))
(define reportes(lambda( numero listaReportes)
                  (if(and(integer? numero)(list? listaReportes))
                     (cons numero listaReportes)
                     null
                     )
                  )
)
;Ejemplo:
(define l1(list "spam" "ofensivo" "incorrecto"))
(define r1(reportes 3 l1))
r1
;Capa selector.
(define getNumReportes(lambda(reportes)(car reportes)))
(define getReportes(lambda(reportes)(cdr reportes)))
;Ejemplo:
(getNumReportes r1)
(getReportes r1)

;Capa pertenencia.
(define reportes?(lambda(reportes)
                   (if (null? reportes)
                       true
                       (if (and (pair? reportes) (integer? (getNumReportes reportes))(list? (getReportes reportes)))
                           true
                           false
                           )
                       )
                   )
  )
;Ejemplos:
(reportes? r1)
(reportes? (cons 0 '()))
(list? '())

;Capa modificador.
(define modificarLista(lambda(lista elemento)
                        (if (null? lista)
                            (cons elemento null)
                            (cons (car lista) (modificarLista (cdr lista) elemento))
                            )
                        )
 )

(define modificarReportes(lambda(reportes reporte)
                        (if (string? reporte)
                            (and (+ 1 (getNumReportes reportes)) (modificarLista (getReportes reportes) reporte))
                            null
                            )
                        )
)                       
;Ejemplo:                        
(define r2(modificarReportes r1 "ofensivo"))
r2
;__________________________________________________________________________
;TDA recompensa.
;Representación: lista de dos elementos
;Par (string ofertor,int valorRecompensa)
;Capa constructor.
(define emptyReward null)
(define recompensa(lambda(ofertor vRecompensa)
                    (if(and (string? ofertor)(integer? vRecompensa))
                       (cons ofertor vRecompensa)
                       null
                       )
                    )
  )
;Ejemplo:
(define recompensa1(recompensa "Alma" 10))
recompensa1

;Capa selector.
(define getOfertor(lambda(recompensa)(car recompensa)))
(define getValorRecompensa(lambda(recompensa)(cdr recompensa)))
;Ejemplos:
(getOfertor recompensa1)
(getValorRecompensa recompensa1)

;Capa pertenencia.
(define recompensa?(lambda(recompensa)
                     (if (null? recompensa)
                         true
                         (if(and (pair? recompensa)(string? (getOfertor recompensa))(integer? (getValorRecompensa recompensa)))
                            true
                            false
                            )
                         )
                     )
  )

;Ejemplo:
(recompensa? recompensa1)
(recompensa? (cons 12 "hola"))

;____________________________________________________________________________

;TDA respuesta.
;Representación: lista (int idRespuesta, string contenidoRes, string autor, string fecha, string aceptación(si/no), int votosAfavor,
;int votosEnContra, TDAreportes reporte)
;Capa constructor.
(define emptyAnswer null)
(define respuesta(lambda( idRespuesta autor fecha contenidoRes etiquetas aceptacion vFavor vContra reportesRes)
                 (if(and(integer? idRespuesta)(string? autor)(date? fecha)(string? contenidoRes)(list? etiquetas)
                   (string? aceptacion)(integer? vFavor)(integer? vContra)(reportes? reportesRes))
                     (list idRespuesta autor fecha contenidoRes etiquetas aceptacion vFavor vContra reportesRes)
                     emptyAnswer
                    )
                 )
 )
 ;Ejemplo:
(define emptyList null)
(define a1(respuesta 0 "Ana" (date 2 3 2020) "Los 12 meses" emptyList "Rechazada" 10 2 r1))
(define a2(respuesta 1 "Juan" (date 1 2 2020) "solo 1 mes, febrero" emptyList "" 2 9 r1))
a1
a2

;Capa selector.
(define getIdRes(lambda(respuesta)(car respuesta)))
(define getAutorRes(lambda(respuesta)(car(cdr respuesta))))
(define getFechaRes(lambda(respuesta)(car(cdr(cdr respuesta)))))
(define getContenidoRes(lambda(respuesta)(car(cdr(cdr(cdr respuesta))))))
(define getEtiquetasRes(lambda(respuesta)(car(cdr(cdr(cdr(cdr respuesta)))))))
(define getAceptacionRes(lambda(respuesta)(car(cdr(cdr(cdr(cdr(cdr respuesta))))))))
(define getVfavorRes(lambda(respuesta)(car(cdr(cdr(cdr(cdr(cdr(cdr respuesta)))))))))
(define getVcontraRes(lambda(respuesta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr respuesta))))))))))
(define getReportesRes(lambda(respuesta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr respuesta)))))))))))
;Ejemplo:
(getContenidoRes a1)
(getReportesRes a1)
(getVcontraRes a1)
(getAutorRes a1)
(getAceptacionRes a1)

;Capa pertenencia.
(define respuesta?(lambda(lista)
                    (if (null? lista)
                        true
                        (if(and(list? lista)(integer? (getIdRes lista))(string? (getContenidoRes lista))(string? (getAutorRes lista) )(date? (getFechaRes lista))
                               (string? (getAceptacionRes lista))(integer? (getVfavorRes lista))(integer? (getVcontraRes lista))(reportes? (getReportesRes lista)))
                           true
                           false
                           )
                        )
                    )
  )
;Ejemplo:
(respuesta? a1)

;TDA respuestas.
;Representacion: lista de TDAs respuestas.
;Capa constructor.
(define (respuestas . respuesta) respuesta)
;Ejemplo:
(define sinRespuestasAun null)
(define respuestas1(respuestas a1 a2))
respuestas1

;Capa selector.
(define primerElemento(lambda(lista)(car lista)))
(define siguientesElementos(lambda(lista)(cdr lista)))
;Ejemplo:
(primerElemento respuestas1)

;Capa pertenencia.
(define es?(lambda(eso? lista)
                     (if (and (eso? (primerElemento lista))(null? (siguientesElementos lista)))
                         true
                        (if (and (eso? (primerElemento lista)) (list? (siguientesElementos lista)))
                            (es? eso? (siguientesElementos lista))
                            false
                            )
                        )
                     )
  )

(define respuestas?(lambda(lista)
                     (if (null? lista)
                         true
                         (es? respuesta? lista))))
;Ejemplo:
(respuestas? respuestas1)
;__________________________________________________________________________________________________________

;TDA usuario.
;Rpresentación: lista( string nombre, string contraseña, int reputación, lista de referencias)
;Capa constructor.
(define emptyUser null)
(define usuario(lambda( nombre contraseña reputacion referencias)
                 (if(and (string? nombre)(string? contraseña)(integer? reputacion)(list? referencias))
                    (list nombre contraseña reputacion referencias)
                    emptyUser
                    )
                 )
 )
;Ejemplo:
(define refe(list "python" "c++"))
(define u1(usuario "Maria" "Maria1999" 20 refe))
(define u2(usuario "Ana" "A1234" 30 refe))
u1
u2
;Capa selector.

(define getNomUser(lambda(usuario)(car usuario)))
(define getContraseña(lambda(usuario)(car(cdr usuario))))
(define getReputacion(lambda(usuario)(car(cdr(cdr usuario)))))
(define getReferencias(lambda(usuario)(car(cdr(cdr(cdr usuario))))))
;Ejemplo:
(getReferencias u1)
(getReputacion u1)
(getNomUser u1)

;Capa pertenencia.
(define usuario?(lambda(usuario)
                  (if (null? usuario)
                      true
                      (if(and (list? usuario)(string? (getNomUser usuario))(string? (getContraseña usuario))
                      (integer? (getReputacion usuario))(list? (getReferencias usuario)))
                         true
                         false
                         )
                      )
                  )
  )
                     


;TDA usuarios.
;Representacion: lista de usuarios.
;Capa constructor.
(define emptyUsers null)
(define (usuarios . usuario) usuario)
;Ejemplo:
(define usuarios1 (usuarios u1 u2))

;La capa selector se reutiliza 
;(define primerElemento(lambda(lista)(car lista)))
;(define siguientesElementos(lambda(lista)(cdr lista)))
;Capa pertenencia
(define usuarios? (lambda(lista)
                    (if (null? lista)
                        true
                        (es? usuario? lista))))

;Ejemplo:
(usuarios? usuarios1)

;TDA contenido.
;Representacion: par(string titulo, string cuerpo)
;Capa constructor.
(define contenidoVacio null)
(define contenidoPreg(lambda(titulo cuerpo)
                       (if(and(string? titulo)(string? cuerpo))
                          (cons titulo cuerpo)
                          contenidoVacio
                          )))
;Capa selector.
(define getTituloPreg (lambda(contenido)(car contenido)))
(define getCuerpoPreg(lambda(contenido)(cdr contenido)))
;Capa pertenencia.
(define contenidoPreg?(lambda(contenido)
                        (if(and(pair? contenido)(string? (getTituloPreg contenido))(string? (getTituloPreg contenido)))
                           true
                           false)))

;TDA Pregunta.
;Representaciòn: lista de elementos relevantes de un pregunta.
;list (int idPregunta, TDA contenido, string autor, string fechaPublicacion,
;string estado(abierta/cerrada), int nVisualizaciones, int votAFavor,
;int votEnContra, lista etiquetas, TDArecompensa recompensa, TDAreportes reportes, TDA respuestas)

(define emptyAsk null)

(define pregunta(lambda
   (idPregunta autor fechaPublicacion contenido etiquetas estado nVisualizaciones votAfavor votEnContra  recompensa reportes respuestas)
                  (if(and(integer? idPregunta)(string? autor)(date? fechaPublicacion)(contenidoPreg? contenido)(list? etiquetas)
                  (string? estado)(integer? nVisualizaciones)(integer? votAfavor)(integer? votEnContra)(recompensa? recompensa)
                  (reportes? reportes)(respuestas? respuestas))
                     (list idPregunta autor fechaPublicacion contenido etiquetas estado nVisualizaciones votAfavor votEnContra recompensa reportes respuestas)
                     emptyAsk
                     )
                  )
  )
;Ejemplo:
(define p1(pregunta 0  "Maria" (date 29 2 2020) (cons "Duda" "¿cuantos meses tienen 29 dias?") '() "abierta" 10 3 2  (recompensa "Maria" 10) r1 respuestas1))
(define p2(pregunta 1 "Ana" (date 29 12 2020) (cons "kahdja" "¿jahjajjkaldlsajahn?")(list "lkal") "abierta" 20 5 9 emptyReward r1 emptyAnswer)) 
p1
p2
;Capa selector.
(define getIdPreg(lambda(pregunta)(car pregunta)))
(define getAutorPreg(lambda(pregunta)(car(cdr pregunta))))
(define getFechaPreg(lambda(pregunta)(car(cdr(cdr pregunta)))))
(define getContenidoPreg(lambda(pregunta)(car(cdr(cdr(cdr pregunta))))))
(define getEtiquetasPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr pregunta)))))))
(define getEstadoPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr pregunta))))))))
(define getVisualizacionesPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))
(define getVfavorPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))
(define getVcontraPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))))
(define getRecompensa(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))))
(define getReportesPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))))))
(define getRespuestas(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))))))
(define o(list "oooooooooooooooooooooooo"))
o
(getRespuestas p1)
(getRespuestas p2)
  
;Capa pertenencia.
  (define pregunta?(lambda(ask)
                     (if(null? ask)
                        true
                        (if(and(list? ask)(integer? (getIdPreg ask))(contenidoPreg? (getContenidoPreg ask))(string? (getAutorPreg ask))
                               (date? (getFechaPreg ask))(string? (getEstadoPreg ask))(integer? (getVisualizacionesPreg ask))
                               (integer? (getVfavorPreg ask))(integer? (getVcontraPreg ask))(list? (getEtiquetasPreg ask))
                               (recompensa? (getRecompensa ask))(reportes? (getReportesPreg ask))(respuestas? (getRespuestas ask)))
                           true
                           false
                           )
                        )
                     )
    )
;Ejemplo:
(pregunta? p1)

(getIdPreg p1)

(getContenidoPreg p1)
(string? (getAutorPreg p1))
(getAutorPreg p1)
(date? (getFechaPreg p1))
(getFechaPreg p1)
(string? (getEstadoPreg p1))
(getEstadoPreg p1)
(integer? (getVisualizacionesPreg p1))
(getVisualizacionesPreg p1)
(integer? (getVfavorPreg p1))
(getVfavorPreg p1)
(integer? (getVcontraPreg p1))
(getVcontraPreg p1)
(list? (getEtiquetasPreg p1))
(getEtiquetasPreg p1)
(integer? (getRecompensa p1))
(getRecompensa p1)
(reportes? (getReportesPreg p1))
(getReportesPreg p1)
(respuestas? (getRespuestas p1))
(getRespuestas p1)







;TDA preguntas.
;Representacion: lista de preguntas.
;Capa constructor.
(define (preguntas . pregunta) pregunta)
;Ejemplo:
(define preguntas1(preguntas p1 p2))
preguntas1

;La capa selector se reutiliza 
;(define primerElemento(lambda(lista)(car lista)))
;(define siguientesElementos(lambda(lista)(cdr lista)))
;Capa pertenencia
(define preguntas? (lambda(lista)
                    (es? pregunta? lista)))
;Ejemplo:
(preguntas? preguntas1)


;TDA stack.
;Representación: lista(TDAusuarios todos, TDAusuarios activos, TDApreguntas lista de preguntas)
;Capa constructor.
(define emptyStack null)
(define stack(lambda(perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
               (if(and(usuarios? perfiles)(usuario? perfilActivo)(preguntas? listaPreguntas)(integer? correlativoPreg)(integer? correlativoRes))
                  (list perfiles perfilActivo listaPreguntas correlativoPreg correlativoRes)
                  emptyStack
                  )
               )
  )

;Largo lista
;funcion necesaria
(define largo(lambda(lista n)
              (if(null? lista)
                  n
                  (largo (cdr lista) (+ n 1))
                  )
              )
  )

;Ejemplo:
(define stack1(stack usuarios1 '() preguntas1 2 2))
(define l2(list "------------------------------"))
  l2
usuarios1
(usuarios? usuarios1)
preguntas1
(preguntas? preguntas1)
stack1
;Capa selector.
(define getUsuarios(lambda(stack)(car stack)))
(define getActivo(lambda(stack)(car(cdr stack))))
(define getPreguntas(lambda(stack)(car(cdr(cdr stack)))))
(define getCorrPreg(lambda(stack)(car(cdr(cdr(cdr stack))))))
(define getCorrRes(lambda(stack)(car(cdr(cdr(cdr(cdr stack)))))))
;Ejemplo:
(getCorrPreg stack1)

;Ejemplo:
;(getUsuarios stack1)


;Desarrollo funciones obligatorias:
;Función1: register.
(define noExisteNombre?(lambda(nombre listaUsuarios)
                        (if (null? listaUsuarios)
                            true
                            (if(eqv? nombre (getNomUser(primerElemento listaUsuarios)))
                               false
                               (noExisteNombre? nombre (siguientesElementos listaUsuarios))
                               )
                            )
                        )
  )

;Dom: una lista y un elemento.
;Rec: una lista.
;La funcion agrega un elemento al final de una lista.
;Utiliza recursion natural.
(define agregarElemento(lambda (lista elemento)
             (if (null? lista)
                 (cons elemento null)
                 (cons (car lista) (agregarElemento (cdr lista) elemento))
                 )
             )
  )
                            

(define register(lambda(stack nombre contraseña)
                  (if (noExisteNombre? nombre (getUsuarios stack))
                      (usuarios (agregarElemento (getUsuarios stack) (usuario nombre contraseña 0 '())) (getActivo stack)(getPreguntas stack))
                      stack
                      )
                  )
  )
                      
;Ejemplo:
(define l(list "-----------------------------------------------------------------"))
l
(register stack1 "Alma" "hi123")


;Función: login.

;Función para autentificar a un usuario
(define autentificar(lambda(nombre contraseña listaUsuarios)
                        (if (null? listaUsuarios) ;Se llego al final de la lista
                            false ;No existe usuario/nombre equivocado
                            (if(eqv? nombre (getNomUser(primerElemento listaUsuarios))) ;Encontro al usuario
                               (if(eqv? contraseña (getContraseña (primerElemento listaUsuarios)))
                               true ;Existe usuario y su clave es correcta
                               false) ;Existe usuario pero su clave no es correcta.
                              (autentificar nombre contraseña (siguientesElementos listaUsuarios)); aun no encuentra al usuario sigue avanzando en la lista
                               )
                            )
                        )
)
;Ejemplo:
(autentificar "Ana" "A1234" (getUsuarios stack1))
(autentificar "Ana" "A123" (getUsuarios stack1)) 
(autentificar "Ana" "" (getUsuarios stack1))
(autentificar "Maria" "A1234" (getUsuarios stack1))


(define get(lambda(id getId lista)
                          (if(eqv? id (getId (primerElemento lista)))
                             (primerElemento lista)
                             (get id getId (siguientesElementos lista))
                             )
                          )
)
(define getUsuario(lambda(nombre listaUsuarios)
                    (get nombre getNomUser listaUsuarios))) ;Funcion de orden superior (toma como parametro otra funcion)
;Ejemplo:
;(getUsuario "Ana" usuarios1)
(define agregarAactivo(lambda(sta nombre)
                         (stack (getUsuarios sta)(getUsuario nombre (getUsuarios sta))(getPreguntas sta)(getCorrPreg sta)(getCorrRes sta))))

;Ejemplo:
(agregarAactivo stack1 "Ana")

;Funcion map:

;login
(define login(lambda(stack nombre contraseña operacion)
               (if(autentificar nombre contraseña (getUsuarios stack))
                  (operacion (agregarAactivo stack1 nombre))
                  operacion
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
               (actualizarStackPreg stack (pregunta (getCorrPreg stack)(getNomUser (getActivo stack)) fecha contenido etiquetas "abierta" 0 0 0 emptyReward emptyReport emptyAsk))))))


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
                             (respuesta (getCorrRes sta) (getNomUser (getActivo sta)) fecha contenidoRes etiquetas "" 0 0 emptyReport))
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



                                        
                            
                                     
                        
                        

                            











