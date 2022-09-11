#lang plai
;Alexia Rodríguez Miranda 316293611
;Alexys Gomez Elizalde 316086189

;1 Función que filtra una lista dado un predicado
; Precondiciones: Una lista y un predicado
; Postcondiciones: Una lista con solo elementos que cumplan el predicado. 
(define (filtra-lista ls p)
  (match ls ;; usamos match para ver que patrón coincidía
    ['() '()] ;; si es una lista vacía devolvemos la vacía
    [(cons x xs) (if (p x) (cons x (filtra-lista xs p)) ;; si es una lista con elementos, si x in lista cumple con p concatenamos x y hacemos la llamada recursiva.
                     (filtra-lista xs p))]));; si x no cumple entonces solo hacemos la llamada recursiva.

;2 Función que devuelve una lista con los tipos de los elementos en la lista original.
; precondiciones: una lista
; poscondiciones: una lista con los tipos de los elementos de la lista recibida.
(define (tipos-ls ls)
  (map tipo ls)) ;; usamos map para aplicarle nuestra función auxiliar a la lista recibida.

;; función auxiliar que define el tipo del elemento recibido.
(define (tipo a)
  (cond
    [(number? a) "number"]
    [(boolean? a) "boolean"]
    [(char? a) "character"]
    [(string? a) "string"]
    [(symbol? a) "symbol"]
    [(list? a) "list"]
    [else "no se conoce el tipo"]))

;3 Función que determina si un número es raro, ie. si al elevar cada dígito del número a la potencia dada por su longitud y después sumarlos, es igual al original.
; precondiciones: un número.
; poscondiciones: booleano que dice si el número es raro.
; notita: vamos separando cada digito, dividendo el número entre diez y el residuo es el dígito que buscamos trabajar. 
(define (raro? n)
       (= (foldr + 0 (map (lambda (x) (expt x (length (digitos n)))) (digitos n))) n)) ;; usamos la función fold para sumar los dígitos y la lambda para indicar que es para cualquier/todo dígito.

; función auxuliar para obtener el dígito.
(define (digitos n)
  (if (zero? n) '()
  (cons (modulo n 10) (digitos (quotient n 10)))))

;4 Función que verifica si la cadena esta ordenada en forma descendente.
;  Precondiciones: Una cadena; potencialmente vacía.
;  Poscondiciones: Un booleano cuyo valor de verdad indica si la cadena dada es un palíndromo (#t) o no lo es
;  (#f).

(define descendente? (lambda nums
  (if (< (length nums) 2)
      #t
      (if(> (first nums) (second nums))
         (apply
          descendente? (rest nums))
          #f
      )
   )
))

;5 Función que verifica si una cadena es un palíndromo.
;  Precondiciones: Una cadena; potencialmente vacía.
;  Poscondiciones: Un booleano cuyo valor de verdad indica si la cadena dada es un palíndromo (#t) o no lo es
; (#f).


(define (palindromo? a)
  (if (empty? a)
      #t
      (equal? (reversa a) a) ;usamos nuestra función auxiliar.
      )
  )

;función auxiliar para hacer la reversa de la cadena
(define (reversa s)
    (cond ((null? s) s)
          ((string? s) (list->string (reverse (string->list s))))))


;6 Función que nos indica si un número es primo o no.
;  Precondiciones: Un número entero.
;  Poscondiciones: Un booleano cuyo valor de verdad indica si el número es primo (#t) o no (#f).

(define (primo? num) 
(cond 
  [(and (= (modulo num 2) 0) (not (= num 2))) #f]
  [(and (= (modulo num 3) 0) (not (= num 3))) #f]
  [(and (= (modulo num 4) 0) (not (= num 4))) #f] 
  [(and (= (modulo num 5) 0) (not (= num 5))) #f]
  [(and (= (modulo num 6) 0) (not (= num 6))) #f]
  [(and (= (modulo num 7) 0) (not (= num 7))) #f] 
  [(and (= (modulo num 8) 0) (not (= num 8))) #f] 
  [(and (= (modulo num 9) 0) (not (= num 9))) #f] 
[else #t]))



;8 Función que calcula el promedio, la moda y la mediana de una lista.
;  Precondiciones: Una lista numérica.
;  Poscondiciones: Un primer número que representa el promedio de los elementos en la lista, un segundo
; número que representa la moda y un último número que representa la mediana.

(define (prom-mod-med ls)
  (values (prom ls) (med ls)) ;sacamos los valores de las funciones auxiliares
  ) 
;función auxiliar para calcular el promedio
(define (prom ls)
  (cond
    [(empty? ls) 0]
    [else (/ (sum ls) (length ls)) ] 
  )
)

;función auxiliar para calcular la suma de todos los valores de la lista
(define (sum x)
  (if (null? x)
    0
    (+ (car x) (sum (cdr x)))))

;función auxiliar para calcular la mediana
(define (med ls)
  (list-ref (sort ls <) (floor(/ (length ls) 2)) )
  )
