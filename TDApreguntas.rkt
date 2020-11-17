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
(provide getQuestion)
(provide getAnswer)
(provide existePregunta?)

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
;Entrega un true si las lista entregada corresponde a un TDA preguntas y sino un false.
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

;Ejemplo:
;(((getAnswer 1)0)preguntas1)



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



;Dom: preguntas, un entero (identificador de pregunta) y una respuesta.
;Rec: preguntas actualizadas, con la respuesta a la pregunta(identificada por su id) agregada.
;La función actualiza una lista de preguntas, actualizando una de sus preguntas, la cual actualiza su lista de respuestas
;agregando una nueva respuesta.
;Usa la función actualizar, la que encontrara la pregunta en la lista de preguntas(en base a su id) y una vez encontrada la entregara como
;parametro de entrada a la función 'agregarRespuestaApreg' junto con la nueva pregunta.
(define actualizarPreguntasNewAns(lambda(listaPreg idPreg nuevRespuesta)
                            (actualizar idPreg getIdPreg agregarRespuestaApreg nuevRespuesta listaPreg)))


;Dom: preguntas y 2 int (un identificador de pregunta y uno de respuesta)
;Rec: preguntas actualizadas.
;La función actualiza preguntas cuando se acepta una de las respuestas de una de sus preguntas y lo hace de la siguiente manera:
;Utiliza recursión natural para ir reescribiendo las preguntas, hasta encontrar la pregunta que se desea actualizar
;en base a su id, cuando enuentra la pregunta, la entrega como parametro de entrada a la función 'modificarPregAccept',
;entrega también como parametro una lista actualizada de respuestas que se forma por la función 'actualizarRespuestasAccept'
;que toma como parametro de entrada las respuestas de la pregunta encontrada y el id de la respuesta que se desea aceptar, una
;vez hecho esto se continuan copiando las respuesta's restantes con cons.
(define actualizarPreguntasAccept(lambda(listaPreg idPreg idRes)
                                   (if (null? listaPreg) ;si se llego al final de las preguntas (la pregunta no existe o era una lista vacía, sin preguntas)...
                                       emptyList ;se retorna una lista vacia
                                       (if(eqv? idPreg (getIdPreg (primerElemento listaPreg))) ;si se encontro la pregunta..
                                          ;se actualiza y se une a los demás elementos
                                          (cons
                                           (modificarPregAccept (primerElemento listaPreg) (actualizarRespuestasAccept(getRespuestas(primerElemento listaPreg)) idRes))
                                           (siguientesElementos listaPreg))
                                          ;sino se une la pregunta con los resultados de la llamada recursiva de la función actualizarPreguntasAccept.
                                          (cons (primerElemento listaPreg) (actualizarPreguntasAccept (siguientesElementos listaPreg) idPreg idRes))
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
(define actualizarPreguntasVot(lambda(listaPreg idPreg booleano)
                                (actualizar idPreg getIdPreg modificarPregVot booleano listaPreg)))

;Ejemplo:
;(actualizarPreguntasRecompensa preguntas1 1 (recompensa "pepe" 20))

;Ejemplo:
;(preguntas? preguntas1)
;Ejemplo:
(provide preguntas1)
(define preguntas1(preguntas p1 p2 p3))
;preguntas1