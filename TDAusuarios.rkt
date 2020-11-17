#lang racket
(require "FuncionesGenerales.rkt")
(require "TDAusuario.rkt")

(provide usuarios?)
(provide noExisteNombre?)
(provide autentificar)
(provide getUsuario)
(provide actualizarUsuariosReputacion)
(provide actualizarUsuarios2Reput)
(provide actualizarUsuariosReputacionVotPreg)
(provide actualizarUsuariosReputacionVotRes)

;TDA usuarios.
;Representación: una lista de TDAs usuario (usuario1, usuario2,....,..,., usuarioN).

;Capa constructor.
;Dom: TDAs usuario.
;Rec: usuarios.
(define emptyUsers null)
(define (usuarios . usuario) usuario)


;Capa selector.
;Utiliza las funciones "primerElemento" y "siguientesElementos" encontradas en el archivo
;"funcionesGenerales" como funciones de capa selector.


;Capa pertenencia.
;Dom: una lista.
;Rec: un booleano.
;Entrega un true si las lista entregada corresponde a un TDA usuarios y sino un false.
;Esta función utiliza la función recursiva "es?" ubicada "funcionesGenerales" para ir
;verificando si cada elemento de la lista corresponde a un usuario y de esta forma saber
;si es un TDA usuarios.
(define usuarios? (lambda(lista)
                    (if (null? lista)
                        true
                        (es? usuario? lista)
                        )
                    )
  )

;FUNCIONES EXTRAS:

;Dom: un string nombre y usuarios (lista de usuario's).
;Rec: un booleano.
;La función busca si existe ya algún usuario en la lista que poseea el nombre que se da como argumento de entrada,
;si existe se retorna un false y sino un true.
;Usa recursion de cola, porque permite ir recorriendo toda la lista de usuarios y comparar el nombre de ellos
;con el nombre de entrada, usando eqv?, getNomUser, primerElemento y siguientesElementos, sin dejar estados pendiente.
(define noExisteNombre?(lambda(nombre listaUsuarios)
                         (if (null? listaUsuarios) ;Si la lista esta vacia o se llego al final de una, no existe usuario con el nombre.
                             true
                             (if(eqv? nombre (getNomUser(primerElemento listaUsuarios))) ;si se encuentra usuario con el nombre
                                false ;falso
                                (noExisteNombre? nombre (siguientesElementos listaUsuarios)) ;sino, se continua buscando.
                                )
                             )
                         )
  )


;Dom: 2 string (nombre y contraseña)y usuarios (lista de usuario's).
;Rec: un booleano.
;Función que autentifica a un usuario, es decir, se busca el usuario por su nombre en una lista
;de usuarios y se comprueba que su contraseña conincida con la contraseña ingresada, si se encuentra
;al usuario y su contraseña coincide se retorna un true sino un false.
;La función utiliza recursión de cola porque permite buscar al usuario de forma rápida en la lista.
(define autentificar(lambda(nombre contraseña listaUsuarios)
                      (if (null? listaUsuarios) ;si se llego al final de la lista o la lista estaba vacía.
                          false ;No existe usuario/ o nombre equivocado.
                          (if(eqv? nombre (getNomUser(primerElemento listaUsuarios))) ;si se encontro al usuario.
                             (if(eqv? contraseña (getContraseña (primerElemento listaUsuarios))) ;la clave coincide?
                                true ;Existe usuario y su clave es correcta
                                false) ;Existe usuario pero su clave no es correcta.
                             (autentificar nombre contraseña (siguientesElementos listaUsuarios)) ;si aun no encuentra al usuario sigue buscando en la lista.
                             )
                          )
                      )
  )

;Dom: un string (nombre) y usuarios (lista de usuario's).
;Rec: un usuario.
;La función busca a un usuario por su nombre y cuando lo encuentra lo retorna.
;Utiliza la función recursiva y de oreden superior "get", combinada con getNomUser.
(define getUsuario(lambda(nombre listaUsuarios)
                    (get nombre getNomUser listaUsuarios)
                    )
  )

;Dom: usuarios, un string (nombre), y una operacion (la operacion corresponde a un par que indica (suma/resta.monto). Ejemplo: (- . 20), (+ . 10), etc.).
;Rec: usuarios actualizados.
;La función actualiza una lista de usuarios(TDAusuarios) y lo que modifica es la reputacion de un de sus usuarios, segun los indique la operación.
; utiliza la función recursiva "actualizar" ubicada en funciones generales.
(define actualizarUsuariosReputacion(lambda(listaUsuarios nombre operacion)
                                      (actualizar nombre getNomUser modificarReputacion operacion listaUsuarios)))


;Dom: usuarios, 2 strings (2 nombre de usuarios), y 2 operaciones (las operaciones corresponde a un par que indica (suma/resta.monto).
;Rec: usuarios actualizados.
;La función actualiza la reputación de usuarios identificados por su nombre, y la actualización dependera de la operación que se ingrese (aumenta o disminuye su reputación).
;Usa recursión natural para reescribir usuarios y cuando encuentra los usuarios por su nombre, realiza el cambio en la reputación usando la función 'modificarReputacion'.
;finaliza cuando se sobreescribe todo usuarios.
(define actualizarUsuarios2Reput(lambda(listaUsuarios persona1 persona2 operacion1 operacion2)
                                  (if (null? listaUsuarios) ;cuando se llega al final de la lista se retorna una lista vacía.
                                      emptyUser
                                      (if(eqv? persona2 (getNomUser (primerElemento listaUsuarios))) ;si se encuentra a la persona2..
                                         ;se aplica la moficación operacion2 y se une a las respuestas de la llamada recursiva de la función.
                                         (cons (modificarReputacion (primerElemento listaUsuarios) operacion2) 
                                               (actualizarUsuarios2Reput (siguientesElementos listaUsuarios)persona1 persona2 operacion1 operacion2))
                                         (if(eqv? persona1 (getNomUser (primerElemento listaUsuarios))) ; si se encontro a la persona1...
                                            ;se aplica la modificación operación1 y se une a las respuestas de la llamada recursiva.
                                            (cons (modificarReputacion (primerElemento listaUsuarios)operacion1)
                                                  (actualizarUsuarios2Reput (siguientesElementos listaUsuarios) persona1 persona2 operacion1 operacion2))
                                            ;si no es el usuario, simplemente se une a las respuestas de la llamasa recursiva.
                                            (cons (primerElemento listaUsuarios)
                                                  (actualizarUsuarios2Reput (siguientesElementos listaUsuarios) persona1 persona2 operacion1 operacion2)))
                                         )
                                      )
                                  )
  )


;Dom: usuarios, un string (nombre autor de una pregunta) y un booleano.
;Rec: usuarios actualizados.
;La función actualiza usuarios (lista de usuario's) cuando se vota (a favor o en contra) de una pregunta.
;El booleano representara: true(voto positivo) y false(voto negativo).
;Cuando alguien vota a favor, el autor de la pregunta gana 10 puntos de reputación.
;Cuando alguien vota en contra, el autor pierde 2 puntos de reputacción.
;Usa la función 'actualizarUsuariosReputacion' para realizar los cambios.
(define actualizarUsuariosReputacionVotPreg(lambda(listaUsuarios nombre booleano)
                                             (if booleano
                                                 (actualizarUsuariosReputacion listaUsuarios nombre (cons + 10)) ;voto a favor.
                                                 (actualizarUsuariosReputacion listaUsuarios nombre (cons - 2))))) ;voto en contra.

;Dom: usuarios, 2 strings (nombre autor de una respuesta y nombre de una persona que vota por esa respuesta) y un booleano.
;Rec: usuarios actualizados.
;La función actualiza usuarios (lista de usuario's) cuando se vota (a favor o en contra) de una respuesta.
;El booleano representara: true(voto positivo) y false(voto negativo).
;Cuando alguien vota a favor, el autor de la respuesta gana 10 puntos de reputación.
;Cuando alguien vota en contra, el autor pierde 2 puntos de reputacción y la persona que voto en contra pierde 1 punto de reputación.
;Usa la función 'actualizarUsuariosReputacion' para realizar los cambios en voto positivo y la función 'actualizarUsuarios2Reput' para voto negativos.
(define actualizarUsuariosReputacionVotRes(lambda(listaUsuarios autorRes votador booleano)
                                            (if booleano
                                                (actualizarUsuariosReputacion listaUsuarios autorRes (cons + 10)) ;voto positivo.
                                                (actualizarUsuarios2Reput listaUsuarios votador autorRes (cons - 1) (cons - 2))))) ;voto negativo.



;Ejemplo:
;(actualizarUsuariosAccept usuarios1 "Ana" "Maria" 10)

;Ejemplo:
;(actualizarUsuariosReputacion usuarios1 "Ana" (cons - 5))


;Ejemplo:
;(getUsuario "Ana" usuarios1)
;Ejemplo:
;(autentificar "Ana" "A1234" (getUsuarios stack1))
;(autentificar "Ana" "A123" (getUsuarios stack1)) 
;(autentificar "Ana" "" (getUsuarios stack1))
;(autentificar "Maria" "A1234" (getUsuarios stack1))



;Ejemplo:
(provide usuarios1)
(define usuarios1 (usuarios u1 u2))