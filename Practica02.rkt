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

(define pi 3.1415926535)

(define (perimetro f)
  (match f
    [(triangulo a b c) (+ a b c)]
    [(rectangulo a b) (+ a b a b)]
    [(rombo l D d) (* l 4)]
    [(paralelogramo a b h) (+ a b a b)]
    [(elipse a b) (* (* 2 pi) (sqrt(/ (+ (* a a)(* b b) ) 2 )))]
    [else "No es una figura definida"]
   ))

(define (area f)
  (match f
    [(triangulo a b c) "No se puede calcular sin la altura"]
    [(rectangulo a b) (* a b)]
    [(rombo l D d) (/ (* D d) 2)]
    [(paralelogramo a b h) (* b h)]
    [(elipse a b) (* pi (* a b))]
    [else "No es una figura definida"]
   ))


#|
(define-type Tren
  [locomotora (p number?)]
  [vagon-pasajeros  (cap number?)]
  [vagon-restaurante (mesas number?) (personal number?)]
  [vagon-dormitorio  (camas number?)]
  )|#



