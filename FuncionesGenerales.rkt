#lang racket
;Este archivo contiene funciones generales que se utilizan en varios de los TDAs y funciones implementadas.
(provide es?)
(provide get)
(provide primerElemento)
(provide siguientesElementos)
(provide agregarElemento)
(provide mostrarElementosList)
(provide emptyList)

(define emptyList null)

;Dom: una lista.
;Rec: un elemento de la lista.
;Esta función entrega el primer elemento de la lista.
(define primerElemento(lambda(lista)(car lista)))

;Dom: una lista.
;Rec: la cola de una lista.
;Esta función entrega los siguientes elementos de una lista.
(define siguientesElementos(lambda(lista)(cdr lista)))

;Dom: una pregunta-> una función que determina pertenencia de un elemento, y una lista de elementos.
;Rec: un booleano.
;Esta función va recorriendo la lista y verificando la pertenencia de los elementos de la lista, si algun elemento
;no cumple con la pertenencia se retorna un false, si encambio se llego al final de la lista y todos los elementos
;cumplieron con la pertenencia se retorna un true.
;Utiliza recursión de cola, ya que de esta forma es más útil comprobar el contenido de una lista de una vez y no es
;necesario dejar estados pendientes.
(define es?(lambda(eso? lista)
                     (if (and (eso? (primerElemento lista))(null? (siguientesElementos lista)))
                         true
                        (if (eso? (primerElemento lista))
                            (es? eso? (siguientesElementos lista))
                            false
                            )
                        )
                     )
  )


;Dom: una lista y un elemento.
;Rec: una lista.
;La funcion agrega un elemento al final de una lista.
;Utiliza recursion natural, pues va dejando cons pendiente a medida que realiza las llamadas recursivas.
;Se usa recursión natural porque pense que seria la forma más conveniente de reescribir una lista para agregarle otro elementos.
(define agregarElemento(lambda (lista elemento)
             (if (null? lista)
                 (cons elemento null)
                 (cons (car lista) (agregarElemento (cdr lista) elemento))
                 )
             )
  )
     
;Dom: un entero (identificador), una función get(función que selecciona un elemento de una lista) y una lista.
;Rec: un elemento de la lista.
;La función permite buscar y obtener un elemento según su identificador(id), en una lista de elementos gracias a la función get.
;Es una función de orden superior que además utiliza recursión de cola, pues facilita el ir recorriendo toda la lista.
(define get(lambda(id getId lista)
             (if (null? lista) ;si se llego al final de la lista o es una lista vacia, el objeto no existia--> error.
                 "Error"
                 (if(eqv? id (getId (primerElemento lista))) ;si el elemento principal es el buscado...
                    (primerElemento lista) ;se retorna
                    (get id getId (siguientesElementos lista)) ;sino se sigue buscando en los demas elementos de la lista.
                    )
                 )
             )
  )


;Dom: una lista.
;Rec: uns lista ordenada con comas y punto.
;La función muestra una lista con comas para separar cada elemento de la lista.
;Usa recursión natural, ya que esta permite rehacer la lista agragando los caracteres
;y dejando todos los cons como estados pendientes.
(define mostrarElementosList(lambda(lista)
                              (if (null? lista)
                                  emptyList
                                  (if (null? (siguientesElementos lista))
                                      (cons (primerElemento lista)(cons "." null))
                                      (cons (primerElemento lista)(cons "," (mostrarElementosList (siguientesElementos lista))))))))