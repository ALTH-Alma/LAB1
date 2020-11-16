#lang racket
(require "FuncionesGenerales.rkt")
(require "TDAusuario.rkt")

(provide usuarios?)
(provide noExisteNombre?)
(provide autentificar)
(provide getUsuario)

;TDA usuarios.
;Representación: una lista de TDAs usuario (usuario1, usuario2,....,..,., usuarioN).

;Capa constructor.
;Dom: TDAs usuario.
;Rec: TDA usuarios.
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
;Dom: un string nombre y un TDAusuarios (lista de usuarios).
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


;Dom: 2 string (nombre y contraseña) y un TDAusuarios (lista de usuarios).
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

;Dom: un string (nombre) y un TDAusuarios (lista de usuarios).
;Rec: un TDAusuario.
;La función busca a un usuario por su nombre y cuando lo encuentra lo retorna.
;Utiliza la función recursiva y de oreden superior "get", combinada con getNomUser.
(define getUsuario(lambda(nombre listaUsuarios)
                    (get nombre getNomUser listaUsuarios)
                    )
  )




;Ejemplo:
;(getUsuario "Ana" usuarios1)
;Ejemplo:
(autentificar "Ana" "A1234" (getUsuarios stack1))
(autentificar "Ana" "A123" (getUsuarios stack1)) 
(autentificar "Ana" "" (getUsuarios stack1))
(autentificar "Maria" "A1234" (getUsuarios stack1))



;Ejemplo:
(provide usuarios1)
(define usuarios1 (usuarios u1 u2))