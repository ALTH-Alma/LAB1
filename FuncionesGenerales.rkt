#lang racket
;Este archivo contiene funciones generales que se utilizan en varios de los TDAs y funciones implementadas.
(provide es?)
(provide get)
(provide actualizar)
(provide primerElemento)
(provide siguientesElementos)
(provide agregarElemento)
(provide mostrarElementosList)
(provide mostrarElementos)
(provide emptyList)
(provide l)

(define emptyList null)
(define l(list "-----------------------------------------------------------------"))


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
     
;Dom: un identificador(puede ser un string, número, etc), una función getId(función que retorna el id de un elemento en una lista) y una lista.
;Rec: un elemento de la lista.
;La función permite buscar y obtener un elemento según su identificador(id), en una lista de elementos gracias a la función get.
;Es una función de orden superior que además utiliza recursión de cola, pues facilita el ir recorriendo toda la lista.
(define get(lambda(id getId lista)
             (if (null? lista) ;si se llego al final de la lista o es una lista vacia, el objeto no existia--> entrega una lista vacia.
                 emptyList
                 (if(eqv? id (getId (primerElemento lista))) ;si el elemento principal es el buscado...
                    (primerElemento lista) ;se retorna
                    (get id getId (siguientesElementos lista)) ;sino se sigue buscando en los demas elementos de la lista.
                    )
                 )
             )
  )

;Dom: un identificador(puede ser string, número, etc), una función getId(función que retorna el id de un elemento de una lista),
;un función modificar(puede ser cualquier función que pida como entrada un elemento de la lista y un dato extra(agregado), el
;agregado(puede ser una operación o elemento: número, string, etc); los dos elementos anteriores dependeran de donde y para que
;se utilice la fución actualizar, finalmente, recibe la lista de elementos.
;Rec: una lista actualizada.
;Esta función es similar a la función map o filter, utiliza recursión natural, pues reescribe una lista modificando un de sus
;elementos, y de esta forma es más simple.
(define actualizar(lambda(id getId modificar agregado lista)
                    (if (null? lista) ;Si se llego al final de la lista o la lista estaba vacia se retorna un null.
                        emptyList
                        
                        (if(eqv? id (getId (primerElemento lista))) ;si el elemento que se esta evaluando es el elemento buscado...
                           
                           ;se le aplica la función(se modifica) y se copia lo que queda de lista tal cual.
                           (cons (modificar (primerElemento lista) agregado)(siguientesElementos lista))
                           
                           ;sino se une el elemento evaluado a los elementos que continuaran siendo evaluados por el llamado recursivo de actualizar.
                           (cons (primerElemento lista) (actualizar id getId modificar agregado (siguientesElementos lista))) 
                           )
                        )
                    )
  )

;Dom: una lista de strings.
;Rec: un string de la lista ordenada con comas y punto.
;Si la lista entregada estaba vacia retorna un string "No tiene."
;La función genera un string con comas para separar cada elemento de la lista.
;Usa recursión natural, ya que esta permite rehacer la lista agragando los caracteres
;y dejando todos los string-append como estados pendientes.
(define mostrarElementosList(lambda(lista)
                              (if (null? lista)
                                  "No tiene."
                                  (if (null? (siguientesElementos lista))
                                      (string-append (primerElemento lista) ".")
                                      (string-append (primerElemento lista)(string-append ", " (mostrarElementosList (siguientesElementos lista))))
                                      )
                                  )
                              )
  )

;Dom: una lista de elementos y una función mostrar (esta función toma un elemento de la lista, lo ordena y lo entrega como string,
;varia según la lista que se ingrese).
;Rec: un string. Si la lista que se ingresa estaba vacía, se entrega un string que lo señala: "Por el momento no tiene".
;La función genera un string de la lista de elementos que se le entrega de forma.
;Usa recursión natural para ir uniendo los elementos transformados a string por la función mostrar, dejando pendiente los string-append
;de las llamadas recursivas. Se usa esta recursión por que es la forma más facíl de reescribir la lista.
;Es como la fución map.
(define mostrarElementos(lambda(listaElementos funcionMostrar)
                          (if (null? listaElementos)
                              "Por el momento no tiene.\n"
                              (string-append "\n"
                                             (if (null? (siguientesElementos listaElementos))
                                                 (string-append (funcionMostrar (primerElemento listaElementos))" \n")
                                                 (string-append (funcionMostrar (primerElemento listaElementos)) " \n" (mostrarElementos (siguientesElementos listaElementos) funcionMostrar)))
                                             )
                              )
                          )
  )

