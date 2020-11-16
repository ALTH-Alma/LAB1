#lang racket

(provide usuario)
(provide usuario?)
(provide emptyUser)
(provide getNomUser)
(provide getContraseña)
(provide puedeOfrecerRecompensa?)
(provide modificarReputacion)

;TDA usuario.
;Representación: lista(string nombre, string contraseña, int reputación, lista de referencias)

;Capa constructor.
;Dom: 2 strings, un entero y una lista.
;Rec: un usuario.
(define emptyUser null)
(define usuario(lambda(nombre contraseña reputacion referencias)
                 (if(and (string? nombre)(string? contraseña)(integer? reputacion)(list? referencias))
                    (list nombre contraseña reputacion referencias)
                    emptyUser
                    )
                 )
 )

;Capa selector.
;Dom: todas las funciones de la capa selector reciben como argumento un usuario.
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

;Dom: un usuario y un entero.
;Rec: un booleano.
;La función revisa si un usuario puede ofrecer una recompensa,en base a la reputación del usuario y al
;monto que se quiere ofrecer como recompensa.
;Si el monto es <= a la recompensa, puede ofrecer (true) y sino no (false).
(define puedeOfrecerRecompensa?(lambda(user montoRecompensa)
                                (if(>= (getReputacion user) montoRecompensa)
                                   true
                                   false
                                   )
                                )
  )

;Dom: un usuario y una modificación.
;La modificación corresponde a un par que indica (operacion.monto). Ejemplo: (- . 20), (+ . 10), etc.
;Rec: un usuario actualizado.
;La función modifica al usuario cambiando la retutación de éste, puede ser sumando o restado puntos para su reputación.
(define modificarReputacion(lambda(user modificacion)
                             ;si los datos ingresados son correctos...
                             (if (and(pair? modificacion)(or (eqv? - (car modificacion))(eqv? + (car modificacion)))(integer? (cdr modificacion))(>= (cdr modificacion) 0))
                                 ;se realiza la modificacion
                                 (usuario (getNomUser user) (getContraseña user) ((car modificacion) (getReputacion user) (cdr modificacion)) (getReferencias user))
                                 user ;sino se retorna el el usuario tal cual.
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