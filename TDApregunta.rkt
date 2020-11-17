#lang racket
(require "FuncionesGenerales.rkt")
(require "TDAcontenidoPregunta.rkt")
(require "TDAdate.rkt")
(require "TDArecompensa.rkt")
(require "TDArespuesta.rkt")
(require "TDArespuestas.rkt")

(provide pregunta)
(provide pregunta?)
(provide modificarPregRecompensa)
(provide modificarPregAccept)
(provide getIdPreg)
(provide getAutorPreg)
(provide getRecompensa)
(provide getRespuestas)
(provide agregarRespuestaApreg)
(provide modificarPregVot)
(provide modificarPregVotRes)

;TDA Pregunta.
;Representación: lista(int id pregunta, string nombre del autor, TDAdate fecha de publicación, TDAcontenidoPreg contenido, lista de etiquetas,
;string estado pregunta(Abierta/Cerrada), int número de visualizaciones, int votos a favor, int votos en contra, TDArecompensa recompensa,
;int reportes, TDArespuestas respuestas).

;Capa contructor.
;Dom: 5 enteros, 2 strings, 1 lista, un TDAcontenidoPreg, un TDAdate, un TDArecompensa y un TDA respuestas.
;Rec: un TDA pregunta.
(define emptyAsk null)
(define pregunta(lambda
   (idPregunta autor fechaPublicacion contenido etiquetas estado nVisualizaciones votAfavor votEnContra recompensa reportes respuestas)
                  (if(and(integer? idPregunta)(string? autor)(date? fechaPublicacion)(contenidoPreg? contenido)(list? etiquetas)
                  (string? estado)(integer? nVisualizaciones)(integer? votAfavor)(integer? votEnContra)(recompensa? recompensa)
                  (integer? reportes)(respuestas? respuestas))
                     (list idPregunta autor fechaPublicacion contenido etiquetas estado nVisualizaciones votAfavor votEnContra recompensa reportes respuestas)
                     emptyAsk
                     )
                  )
 )


;Capa selector.
;Dom: todas las funciones de la capa selector tienen por entrada un TDApregunta.
;______

;Rec: un entero.
;Entrega el identificador de una pregunta.
(define getIdPreg(lambda(pregunta)(car pregunta)))

;Rec: un string.
;Entrega el nombre del autor de una pregunta.
(define getAutorPreg(lambda(pregunta)(car(cdr pregunta))))

;Rec: un TDAdate.
;Entrega la fecha de publicación de una pregunta.
(define getFechaPreg(lambda(pregunta)(car(cdr(cdr pregunta)))))

;Rec: un TDAcontenidoPreg.
;Entrega contenido de una pregunta (su titulo y cuerpo).
(define getContenidoPreg(lambda(pregunta)(car(cdr(cdr(cdr pregunta))))))

;Rec: una lista.
;Entrega la lista de etiquetas de una pregunta.
(define getEtiquetasPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr pregunta)))))))

;Rec: un string.
;Entrega el estado de una pregunta.
(define getEstadoPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr pregunta))))))))

;Rec: un entero.
;Entrega el número de visualizaciones de una pregunta.
(define getVisualizacionesPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))

;Rec: un entero.
;Entrega el número de votos a favor de una pregunta.
(define getVfavorPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))

;Rec: un entero.
;Entrega el número de votos en contra de una pregunta.
(define getVcontraPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))))

;Rec: un TDArecompensa.
;Entrega la información de una recompensa ofrecida por la pregunta (nombre del autor de la recompensa y el monto de ésta).
(define getRecompensa(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))))

;Rec: un entero.
;Entrega los reportes de una pregunta.
(define getReportesPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))))))

;Rec: un TDArespuestas.
;Entrega las respuestas entregadas a la pregunta.
(define getRespuestas(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))))))
;_____

  
;Capa pertenencia.
;Dom: una lista, posible pregunta.
;Rec: un booleano.
;Entrega un true si la lista resulto siendo una pregunta y un false sino.
(define pregunta?(lambda(preg)
                   (if(null? preg)
                      true
                      (if(and(list? preg)(integer? (getIdPreg preg))(contenidoPreg? (getContenidoPreg preg))(string? (getAutorPreg preg))
                             (date? (getFechaPreg preg))(string? (getEstadoPreg preg))(integer? (getVisualizacionesPreg preg))
                             (integer? (getVfavorPreg preg))(integer? (getVcontraPreg preg))(list? (getEtiquetasPreg preg))
                             (recompensa? (getRecompensa preg))(integer? (getReportesPreg preg))(respuestas? (getRespuestas preg)))
                         true
                         false
                         )
                      )
                   )
  )

;FUNCIONES EXTRAS:

;Dom: una pregunta y una recompensa.
;Rec: una pregunta actualizada.
;La función actualiza una pregunta agregandole una recompensa.
(define modificarPregRecompensa(lambda(preg recompensa)
                             (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                              (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (getVcontraPreg preg)
                               recompensa (getReportesPreg preg) (getRespuestas preg))))



;Dom: una pregunta y una respuesta.
;Rec: una pregunta actualizada, con la respuesta agregada en su lista de respuestas.
;La función agrega una respuesta a la lista de respuestas de una pregunta, utilizando la
;función recursiva 'agregarElemento'.
(define agregarRespuestaApreg(lambda(preg nuevRespuesta)
                          (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                              (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (getVcontraPreg preg)
                               (getRecompensa preg) (getReportesPreg preg)(agregarElemento (getRespuestas preg) nuevRespuesta))))


;Dom: una pregunta y respuestas actualizadas.
;Rec: una pregunta actualizada.
;La función produce el cambio en preguntas cuando alguien acepta una respuesta, esto es eliminar recompensa de la pregunta
;(ya que la persona que responde la respuesta aceptada se la lleva) y con la respuesta marcada como aceptada.
;a esta funcion llegan las respuestas actualizadas por lo que solo reemplaza las anteriores respuestas por la nueva.
(define modificarPregAccept(lambda(preg respuestasActualizadas)
                          (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                              (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (getVcontraPreg preg)
                               emptyReward (getReportesPreg preg) respuestasActualizadas)))


;Dom: una pregunta y un booleano.
;Rec: una pregunta actualizada.
;La función reescribe una pregunta agregandole un voto a favor o en contra según el booleano.
;Si el booleano es true se suma 1 voto a favor y si es false se suma un voto en contra.
(define modificarPregVot(lambda(preg booleano)
                          (if booleano
                              (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                                        (getEstadoPreg preg) (getVisualizacionesPreg preg) (+ 1 (getVfavorPreg preg)) (getVcontraPreg preg)
                                        (getRecompensa preg) (getReportesPreg preg) (getRespuestas preg))
                              (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                                        (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (+ 1 (getVcontraPreg preg))
                                        (getRecompensa preg) (getReportesPreg preg) (getRespuestas preg)))))



;Rec: una pregunta y respuestas actualizadas.
;Dom: una pregunta actualizada.
;La función actualiza una pregunta, al cambiar sus respuestas, y el cambio es que se agrega un voto a una de sus respuesta's.
;Se reescribe la pregunta pero se cambian las respuestas anteriores por las respuestas actualizadas.
(define modificarPregVotRes(lambda(preg respuestasActualizadas)
                             (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                                       (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (getVcontraPreg preg)
                                       (getRecompensa preg) (getReportesPreg preg) respuestasActualizadas)))

;Ejemplos:
(provide p1)
(provide p2)
(provide p3)
(define p1(pregunta 0  "Maria" (date 29 2 2020) (cons "Duda" "¿cuantos meses tienen 29 dias?") emptyList "abierta" 10 3 2  (recompensa "Maria" 10) 1 respuestas1))
(define p2(pregunta 1 "Ana" (date 29 12 2020) (cons "kahdja" "¿jahjajjkaldlsajahn?")(list "lkal") "abierta" 20 5 9 emptyReward 0 emptyAnswer))
(define p3(pregunta 2  "Maria" (date 30 2 2020) (cons "Año" "¿cuantos dias tiene un año?") emptyList "abierta" 1 1 0  (recompensa "Ana" 5) 3 respuestas2))
;(getRespuestas p1)
;(getRespuestas p2)
;(pregunta? p1)