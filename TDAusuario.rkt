#lang racket

(provide usuario)
(provide usuario?)
(provide emptyUser)

;TDA usuario.
;Representación: lista(string nombre, string contraseña, int reputación, lista de referencias)

;Capa constructor.
;Dom: 2 strings, un entero y una lista.
;Rec: un TDA usuario.
(define emptyUser null)
(define usuario(lambda(nombre contraseña reputacion referencias)
                 (if(and (string? nombre)(string? contraseña)(integer? reputacion)(list? referencias))
                    (list nombre contraseña reputacion referencias)
                    emptyUser
                    )
                 )
 )

;Capa selector.
;Dom: todas las funciones de la capa selector reciben como argumento un TDA usuario.
;______

;Rec: un string.
;Entrega el nombre del usuario.
(define getNomUser(lambda(usuario)(car usuario)))

;Rec: un string.
;Entrega la contraseña del usuario.
(define getContraseña(lambda(usuario)(car(cdr usuario))))

;Rec: un entero.
;Entrega la reputacción de un usuario.
(define getReputacion(lambda(usuario)(car(cdr(cdr usuario)))))

;Rec: una lista.
;Entrega una lista de las referencias del usuario.
(define getReferencias(lambda(usuario)(car(cdr(cdr(cdr usuario))))))
;________

;Ejemplo:
;(getReferencias u1)
;(getReputacion u1)
;(getNomUser u1)

;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Esta función entrega un true si la lista evaluada corresponde a un TDA usuario.
(define usuario?(lambda(lista)
                  (if (null? lista)
                      true
                      (if(and (list? lista)(string? (getNomUser lista))(string? (getContraseña lista))
                              (integer? (getReputacion lista))(list? (getReferencias lista)))
                         true
                         false
                         )
                      )
                  )
)


;Ejemplo:
(provide u1)
(provide u2)
(define refe(list "python" "c++"))
(define u1(usuario "Maria" "Maria1999" 20 refe))
(define u2(usuario "Ana" "A1234" 30 refe))
u1
u2