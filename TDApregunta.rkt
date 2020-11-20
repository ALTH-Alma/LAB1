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
(provide mostrarPregunta)

;TDA Pregunta.
;Representación: lista(int id pregunta, string nombre del autor, date fecha de publicación, contenidoPreg contenido, lista de etiquetas,
;string estado pregunta(Abierta/Cerrada), int número de visualizaciones, int votos a favor, int votos en contra, una recompensa recompensa,
;int reportes, respuestas respuestas).

;Capa contructor.
;Dom: 5 enteros, 2 strings, 1 lista, un contenidoPreg, un date, una recompensa y respuestas.
;Rec: una pregunta.
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
;Dom: todas las funciones de la capa selector tienen por entrada una pregunta.
;______

;Rec: un entero.
;Entrega el identificador de una pregunta.
(define getIdPreg(lambda(pregunta)(car pregunta)))

;Rec: un string.
;Entrega el nombre del autor de una pregunta.
(define getAutorPreg(lambda(pregunta)(car(cdr pregunta))))

;Rec: un date.
;Entrega la fecha de publicación de una pregunta.
(define getFechaPreg(lambda(pregunta)(car(cdr(cdr pregunta)))))

;Rec: un contenidoPreg.
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

;Rec: una recompensa.
;Entrega la información de una recompensa ofrecida por la pregunta (nombre del autor de la recompensa y el monto de ésta).
(define getRecompensa(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta))))))))))))

;Rec: un entero.
;Entrega los reportes de una pregunta.
(define getReportesPreg(lambda(pregunta)(car(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr(cdr pregunta)))))))))))))

;Rec: respuestas.
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



;Dom: una pregunta.
;Rec: un string.
;Función que ordena todos los elementos de una pregunta utilizando la función string-append, strings y las funciónes date->string, number->string, mostrarElementosList
;y mostrarRespuestas para transformar sus elementos en strings. Muestra una pregunta estructurada.
;Posteriormente se utiliza para mostrar un stack o parte de un stack en un gran string que puede verse ordenado con la función display.
(define mostrarPregunta(lambda(preg)
                         (string-append "ID Pregunta: "(number->string (getIdPreg preg))
                                "\nTitulo pregunta: "(getTituloPreg (getContenidoPreg preg))". "
                                "\nCuerpo pregunta: "(getCuerpoPreg (getContenidoPreg preg))". "
                                "\nAutor: "(getAutorPreg preg)". "
                                "\nFecha publicación: "(date->string (getFechaPreg preg))
                                "\nEtiquetas: "(mostrarElementosList (getEtiquetasPreg preg))
                                "\nEstado: "(getEstadoPreg preg)". "
                                "\nVisualizaciones: "(number->string (getVisualizacionesPreg preg))
                                "\nVotos a favor: "(number->string (getVfavorPreg preg))
                                "\nVotos en contra: "(number->string (getVcontraPreg preg))
                                "\nRecompensa: "(recompensa->string (getRecompensa preg))
                                "\nReportes: "(number->string (getReportesPreg preg))
                                "\n\nRespuestas:\n"
                                (mostrarRespuestas (getRespuestas preg))
                                "\n")
                         )
  )



;EJEMPLOS NECESARIOS PARA EJEMPLOS DE FUNCIONES MAIN:

(provide p1)
(provide p2)
(provide p3)
(define p1(pregunta 0  "Maria" (date 29 2 2020) (cons "¿Por qué es considerado una mala práctica utilizar variables globales?" "¿Realmente son perjudiciales?")
                    emptyList "Abierta" 10 3 2 (recompensa "Maria" 10) 1 respuestas1))
(define p2(pregunta 1 "Ana" (date 29 10 2020) (cons "¿Cómo poner una imagen de fondo en?" "Me gustaría saber ¿Cómo pongo una imagen de fondo a la ventana creada
 con PyQT5? Muchos me dicen que use Designer, pero estoy evitando usarlo. ¿Conocen alguna manera?")(list "python""interfaz-gráfica""imagen") "Abierta" 20 5 2 emptyReward 0 respuestas2))
(define p3(pregunta 2  "Maria" (date 30 2 2020) (cons "¿Cómo puedo subir un archivo de ms Project a PHP para poder leerlo y mostrarlo en pantalla?"
                                                     "NO tengo ni idea de como hacerlo y no tengo nada que mostrar si alguien me puede ayudar se lo agradecería.") emptyList "Abierta" 1 1 0  emptyReward 2 respuestas3))

