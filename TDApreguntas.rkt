#lang racket

(require "FuncionesGenerales.rkt")
(require "TDArespuesta.rkt")
(require "TDArespuestas.rkt")
(require "TDApregunta.rkt")
(require "TDArecompensa.rkt")
(provide preguntas?)
(provide actualizarPreguntasRecompensa)
(provide actualizarPreguntasNewAns)
(provide actualizarPreguntasAccept)
(provide actualizarPreguntasVot)
(provide actualizarPreguntasVotRes)
(provide getQuestion)
(provide getAnswer)
(provide existePregunta?)
(provide existeRespuestaEnPreg?)
(provide mostrarPreguntas)
(provide mostrarPregUsuario)
;TDA preguntas.
;Representación: una lista de pregunta's (pregunta1, pregunta2,....,..,., preguntaN).

;Capa constructor.
;Dom: una o más pregunta's.
;Rec: preguntas.
(define emptyAsks null)
(define (preguntas . pregunta) pregunta)



;Capa selector.
;Utiliza las funciones "primerElemento" y "siguientesElementos" encontradas en el archivo
;"funcionesGenerales" como funciones de capa selector.


;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Entrega un true si las lista entregada corresponde a preguntas y sino un false.
;Esta función utiliza la función recursiva "es?" ubicada "funcionesGenerales" para ir
;verificando si cada elemento de la lista corresponde a una pregunta y de esta forma saber
;si son preguntas.
(define preguntas? (lambda(lista)
                    (es? pregunta? lista)
                     )
  )

;Dom: preguntas, un entero(identificador), y una recompensa.
;Rec: pregntas actualizadas.
;La función actualiza preguntas, modificando una de sus preguntas al agregarle una recompensa.
;Usa la función recursiva 'actualizar' que se encuentra en funcionesGenerales.rkt.
;actualizar encontrara la pregunta en la lista de preguntas(en base a su id) y una vez encontrada la entregara como
;parametro de entrada a la función 'modificarPregRecompensa' junto con la recompensa que se quiere agregar.
(define actualizarPreguntasRecompensa(lambda (listaPreguntas id recompensa)
                                       (actualizar id getIdPreg modificarPregRecompensa recompensa listaPreguntas)))


;Dom: un entero (identificador)
;Rec:
;función currificada que opera sobre preguntas.
;retorno final una pregunta.
;La función encuentra y entrega una pregunta según su id en una lista de preguntas.
;Usa la función recursiva de cola 'get' ubicada en FuncionesGenerales.rkt
(define getQuestion(lambda(id)
                     (lambda(listaPreguntas)
                     (get id getIdPreg listaPreguntas))))



;Dom: un entero (id de respuesta).
;Rec:
;una función currificada para un entero (id de pregunta a la que corresponde la respuesta) y preguntas.
;el recorrido final es una respuesta.
;La función busca una respuesta basada en su id, en una pregunta obtenida por su id, en preguntas (lista ingresada).
;Usa la función recursiva de cola 'get' ubicada en FuncionesGenerales.rkt para buscar la respuesta en respuestas de
;la pregunta, mientras que la pregunta es obtenida por la función getQuestion en preguntas.
(define getAnswer(lambda(idRes)
                      (lambda(idPreg)
                        (lambda(listaPreguntas)
                        (get idRes getIdRes (getRespuestas ((getQuestion idPreg) listaPreguntas)))))))


;Dom: un entero(identificador) y preguntas.
;Rec: un booleano.
;La función confirma la existencia de la pregunta en la lista segun su id,
;si se encuentra la pregunta true (existe), sino false.
;Usa la función 'getQuestion' para obtener la pregunta, si retorna una null
;significa que la pregunta no existe.
(define existePregunta?(lambda(id listaPreguntas)
                        (if (null? ((getQuestion id)listaPreguntas))
                            false
                            true)))

;Dom: 2 enteros (identificador de una respuesta y de una pregunta) y preguntas.
;Rec: un booleano.
;La función busca una respuesta por su id en un una pregunta que se encuntra en preguntas.
;Usa la función getAnswer para obtener la respuesta, y si esta retorna un null significa que la respuesta
;no existia en la pregunta.
(define existeRespuestaEnPreg?(lambda(idRes idPreg listaPreguntas)
                     (if (null? (((getAnswer idRes)idPreg))listaPreguntas)
                         false
                         true)))

;Dom: preguntas, un entero (identificador de pregunta) y una respuesta.
;Rec: preguntas actualizadas, con la respuesta a la pregunta(identificada por su id) agregada.
;La función actualiza una lista de preguntas, actualizando una de sus preguntas, la cual actualiza su lista de respuestas
;agregando una nueva respuesta.
;Usa la función actualizar, la que encontrara la pregunta en la lista de preguntas(en base a su id) y una vez encontrada la entregara como
;parametro de entrada a la función 'agregarRespuestaApreg' junto con la nueva pregunta.
(define actualizarPreguntasNewAns(lambda(listaPreguntas idPreg nuevRespuesta)
                            (actualizar idPreg getIdPreg agregarRespuestaApreg nuevRespuesta listaPreguntas)))


;Dom: preguntas y 2 int (un identificador de pregunta y uno de respuesta)
;Rec: preguntas actualizadas.
;La función actualiza preguntas cuando se acepta una de las respuestas de una de sus preguntas y lo hace de la siguiente manera:
;Utiliza recursión natural para ir reescribiendo las preguntas, hasta encontrar la pregunta que se desea actualizar
;en base a su id, cuando enuentra la pregunta, la entrega como parametro de entrada a la función 'modificarPregAccept',
;entrega también como parametro una lista actualizada de respuestas que se forma por la función 'actualizarRespuestasAccept'
;que toma como parametro de entrada las respuestas de la pregunta encontrada y el id de la respuesta que se desea aceptar, una
;vez hecho esto se continuan copiando las respuesta's restantes con cons.
(define actualizarPreguntasAccept(lambda(listaPreguntas idPreg idRes)
                                   (if (null? listaPreguntas) ;si se llego al final de las preguntas (la pregunta no existe o era una lista vacía, sin preguntas)...
                                       emptyList ;se retorna una lista vacia
                                       (if(eqv? idPreg (getIdPreg (primerElemento listaPreguntas))) ;si se encontro la pregunta..
                                          ;se actualiza y se une a los demás elementos
                                          (cons
                                           (modificarPregAccept (primerElemento listaPreguntas) (actualizarRespuestasAccept(getRespuestas(primerElemento listaPreguntas)) idRes))
                                           (siguientesElementos listaPreguntas))
                                          ;sino se une la pregunta con los resultados de la llamada recursiva de la función actualizarPreguntasAccept.
                                          (cons (primerElemento listaPreguntas) (actualizarPreguntasAccept (siguientesElementos listaPreguntas) idPreg idRes))
                                          )
                                       )
                                   )
  )

;Dom: preguntas, un entero(id de pregunta) y un booleano.
;Rec: preguntas actualizadas.
;Actualiza preguntas cuando se realiza un voto.
;La función reescribe preguntas usando la función 'actualizar', mientras lo hace, si encuentra la pregunta por su id,
;pasa la pregunta como argumento de entrada a la función 'modificarPregVot' junto al booleano que modificara la pregunta agregando un voto
;(a favor o en contra) y seguira reescribiendo preguntas con la nueva pregunta actualizada.
(define actualizarPreguntasVot(lambda(listaPreguntas idPreg booleano)
                                (actualizar idPreg getIdPreg modificarPregVot booleano listaPreguntas)))



;Dom: preguntas, 1 entero (identificadore de pregunta), una respuesta y un booleano.
;Rec: preguntas actualizadas.
;La función actualiza las preguntas cuando se modifica una pregunta al agregar un voto a una respuesta en sus respuestas.
;Va reescribiendo preguntas, mientras lo hace, cuando encuentra la pregunta por su id, la entrega como parametro de entrada
;a la función 'modificarPregVotRes' junto con la lista de respuestas actualizadas (las que se actualizan con la función
;'actualizarResVot a las que se le entrega como parametro de entrada las respuestas de la pregunta encontrada el id de
;la respuesta y el booleano(determina voto + o voto -)), de esta forma se actualiza la pregunta y se continua reescribiendo
;lo que queda de preguntas.
;Usa recursión natural ya que facilita el reescribir preguntas al usar cons.
(define actualizarPreguntasVotRes(lambda(listaPreguntas idPreg res booleano)
                                   (if (null? listaPreguntas) ;si se llega al final o era una lista vacía..
                                       emptyList ;se retorna una lista vacía.
                                       (if(eqv? idPreg (getIdPreg (primerElemento listaPreguntas))) ;si se encontro la pregunta.
                                          (cons ;actualiza la pregunta y la une a lo que queda de respuestas
                                           (modificarPregVotRes(primerElemento listaPreguntas) (actualizarResVot(getRespuestas(primerElemento listaPreguntas)) (getIdRes res) booleano))
                                           (siguientesElementos listaPreguntas))
                                          ;sino se une el pregunta con los resultados de la llamada recursiva de la función.
                                          (cons (primerElemento listaPreguntas) (actualizarPreguntasVotRes (siguientesElementos listaPreguntas) idPreg res booleano))
                                          )
                                       )
                                   )
  )


;Dom: preguntas.
;Rec: un string.
;La función utiliza la función 'mostrarElementos' en conjunto con la función 'mostrarPregunta' para ir transformando todos las pregunta's
;en preguntas, en string y los une en un gran string ordenado.
(define mostrarPreguntas(lambda(listaPreguntas)
                           (mostrarElementos listaPreguntas mostrarPregunta)))


;Dom: un string(nombre de un usuario) y preguntas.
;Rec: un string.
;La función muestra todos las preguntas y sus respectivas respuestas de un usuario identificado por su nombre.
;Usa recursión natural para ir recorriendo la lista de preguntas y encontrando las preguntas cuyo autor es el usuario, para luego transformalas
;en string con la función 'mostrarPregunta' y unirla a las demás preguntas encontradas con la llamada recursiva de la función y de string-append.
(define mostrarPregUsuario(lambda(nomUsuario listaPreguntas)
                            (if (null? listaPreguntas) ;si se llego al final de la lista se retorna "".
                                ""
                                (if (eqv? nomUsuario (getAutorPreg (primerElemento listaPreguntas))) ;si se encuentra una pregunta del usuario..
                                    ;se transforma a string y se une a los resultados de la llamada recursiva.
                                    (string-append (mostrarPregunta (primerElemento listaPreguntas)) "\n" (mostrarPregUsuario nomUsuario (siguientesElementos listaPreguntas)))
                                    ;sino se sigue buscando.
                                    (mostrarPregUsuario nomUsuario (siguientesElementos listaPreguntas)))
                                )
                            )
  )


;EJEMPLOS NECESARIOS PARA EJMPLOS DE FUNCIONES MAIN:
(provide preguntas1)
(define preguntas1(preguntas p1 p2 p3))
;preguntas1