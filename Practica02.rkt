#lang plai

;;Ejercicio 1: Constructor de tipos Figura.
;; Precondiciones: numeros, dependiendo del tipo que se quiera.
;; Postcondiciones: un objeto de ese tipo.

(define-type Figura
  [triangulo (a number?) (b number?) (c number?) ]
  [rectangulo (a number?) (b number?)]
  [rombo (l number?) (D number?) (d number?)]
  [paralelogramo (a number?) (b number?) (h number?)]
  [elipse (a number?) (b number?)]
  )


