#lang plai
;Gomez Elizalde Alexys 316086189
;Rodríguez Miranda Alexia 316293611

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

;definición de pi
(define pi 3.1415926535)

;Función perimetro
(define (perimetro f)
  (match f
    [(triangulo a b c) (+ a b c)]
    [(rectangulo a b) (+ a b a b)]
    [(rombo l D d) (* l 4)]
    [(paralelogramo a b h) (+ a b a b)]
    [(elipse a b) (* (* 2 pi) (sqrt(/ (+ (* a a)(* b b) ) 2 )))]
    [else "No es una figura definida"]
   ))

;Función area
(define (area f)
  (match f
    ;por formula de Heron
    [(triangulo a b c) (sqrt(* (/ (+ a b c) 2) (- (/ (+ a b c) 2) a) (- (/ (+ a b c) 2) b) (- (/ (+ a b c) 2) c)))]
    [(rectangulo a b) (* a b)]
    [(rombo l D d) (/ (* D d) 2)]
    [(paralelogramo a b h) (* b h)]
    [(elipse a b) (* pi (* a b))]
    [else "No es una figura definida"]
   ))



(define-type Vagon
  [locomotora (p positive-integer?)]
  [vagon-pasajeros  (cap positive-integer?)]
  [vagon-restaurante (mesas positive-integer?) (personal positive-integer?)]
  [vagon-dormitorio  (camas positive-integer?)]
  )

(define-type Tren
  ;;tren que solo esta conformado por un vagon, este no debería de darse, pero lo ponemos por practicidad, excluyendo las locomotoras.
  [tren-pasajeros (vagon vagon-pasajeros?)]
  [tren-restaurante (vagon vagon-restaurante?)]
  [tren-dormitorio (vagon vagon-dormitorio?)]
  ;; tren que tiene una locomotora al inicio del tren, otra al final, y en el medio otro tren
  [tren-ft (loci locomotora?)
        (resto Tren?)
        (locd locomotora?)]
  ;; tren que tiene una locomotora al final
  [tren-t (treni Tren?)  
          (locd locomotora?)]
  ;;tren que tiene una locomotora al inicio
  [tren-f (loci locomotora?)
          (trenf Tren?)]
)

#| Calcula el número de pasajeros máximo que pueden abordar
el tren.
(define (num-pasajeros tren)
  (cond
    [(tren-v? tren)
     (let( [vagon (tren-v-vagon tren)] )
       (if (vagon-pasajeros? vagon)
           (vagon-pasajeros-cap vagon)
           0)
     )]
    
    [(tren? tren)
     (let ( [trenau (tren-trenaux tren)] )
       ()
           )
       
       ]
      
    ))
|#

;Calcula el porcentaje de la potencia de arrastre utilizada del tren.
(define (arraste-usado tren)
  (/(* (ac-potloc tren) 100) (cuenta-vags-noloc tren))
  )

(define (ac-potloc tren)
            (cond
              [(tren-v? tren)
               (let ([vagon (tren-v-vagon tren)])
                  (if (locomotora? vagon)
                      (locomotora-p vagon)
                      0))]
                  )
              [(tren? tren)
               (let (
                    [loci (tren-loci tren)]
                    [locd (tren-locd tren)]
                    [restot (tren-resto)])
                     (+ (locomotora-p loci)
                        (locomotora-p locd)
                        (ac-potloc restot))
                 )]
              [(tren-i? tren)]
              [(tren-t? tren)])
             

(define (cuenta-vags-noloc tren)
  (cond
    [(tren-v? tren) (let (
      [vagon (tren-v-vagon tren)])
      (if (locomotora? vagon)
          0
          1))]
    [(tren? tren) (let (
          [loci (tren-loci tren)]
          [locd (tren-locd tren)]
          [restot (tren-resto)]
          ))]
    [(tren-t? tren) (let (
      [vagon (tren-t-vagon tren)]
      [resto (trent-t-resto tren)])
      (+ (cuenta-vags-noloc restot)
         (if (locomotora? vagon)
             0
             1)))]
    [(tren-f? tren) (let (
        [vagon (tren-f-vagon tren)]
        [restot (tren-f-resto tren)])
        (+(cuenta-vags-noloc restot)
          (if (locomotora? vagon)
              0
              1)))]))
  
