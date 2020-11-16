#lang racket
(require "FuncionesGenerales.rkt")
(require "TDAcontenidoPregunta.rkt")
(require "TDAdate.rkt")
(require "TDArecompensa.rkt")
(require "TDArespuesta.rkt")
(require "TDArespuestas.rkt")


(provide pregunta?)
(provide modificarPregRecompensa)

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

;Dom: una pregunta y una recompensa.
;Rec: una pregunta actualizada.
;La función actualiza una pregunta agregandole una recompensa.
(define modificarPregRecompensa(lambda(preg recompensa)
                             (pregunta (getIdPreg preg) (getAutorPreg preg) (getFechaPreg preg) (getContenidoPreg preg) (getEtiquetasPreg preg)
                              (getEstadoPreg preg) (getVisualizacionesPreg preg) (getVfavorPreg preg) (getVcontraPreg preg)
                               recompensa (getReportesPreg preg) (getRespuestas preg))))






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