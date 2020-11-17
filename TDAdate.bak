#lang racket
(provide date)
(provide date?)

;TDA date.
;Se utiliza para establecer fechas de publicación de preguntas y respuestas. 
;Representación: lista( int dia, int mes, int año).

;Capa constructor.
;Dom: 3 enteros, un día, un mes y un año.
;Rec: un TDA date (día mes año)
(define emptyDate null)
(define date(lambda( dia mes año)
              (if(and (integer? dia)(<= dia 31)(integer? mes)(<= mes 12)(integer? año))
                 (list dia mes año)
                 emptyDate)))

;Capa selector.
;Dom: las 3 funciones tienen por entrada un TDA date.
;____

;Rec: un entero.
;Función que entrega un número correspondiente a un día.
(define getDia(lambda(fecha)(car fecha)))

;Rec: un entero.
;Función que entrega un número correspondiente a un mes.
(define getMes(lambda(fecha)(car(cdr fecha))))

;Rec: un entero.
;Función que entrega un número correspondiente a un año.
(define getAño(lambda(fecha)(car(cdr(cdr fecha)))))
;______

;Capa pertenencia.
;Dom: un lista.
;Rec: un booleano.
;Función que entrega un true si la lista corresponde a un TDA date, sino un false.
(define date?(lambda(fecha)
               (if(and(list? fecha)(integer? (getDia fecha))(<= (getDia fecha) 31)(integer? (getMes fecha))(<= (getMes fecha) 12)(integer? (getAño fecha)))
                  true
                  false)))