#lang plai
;;Ejercicio 1:  
;; precondiciones: Una expresión simbolica.
;; postcondiciones: El arbol de sintaxis abstracta.
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [with (bindings (listof binding?)) (body AST?)] ;; para construir la lista necesitas utilizar (list (binding...)(binding...)).
  [with* (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]  
  [app (fun AST?) (args (listof AST?))] ;; asumí que args y los params de la función tienen la misma aridad, ya que de otra forma habría variables libres y no tendría interp
)

(define-type Binding
  [binding(id symbol?) (value AST?)]
)
;;Ejercicio 1: Definir la función parse que convierte algo de los tipos originales de racket a un elemento de nuestro lenguaje 
;; precondiciones: Una expresión simbolica.
;; postcondiciones: El arbol de sintaxis abstracta.

(define (parse sexp)
  (match sexp
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [(list 'with l body)
     (with (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(list 'with* l body)
     ('with* (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(? list?) (let ([g (lambda (x) (match x
                                      ['+ +]
                                      ['- -]
                                      ['* *]
                                      ['/ /]
                                      ['expt (λ elem (foldr expt (car elem) (cdr elem)))]
                                      ['modulo modulo]
                                      ['add1 add1]
                                      ['sub1 sub1]
                                      [else (error "operacion fuera del lenguaje")]))])
                 (op (g (first sexp))  (map (lambda (x) (parse x)) (cdr sexp))))]
    [_ (error "Sintaxis Incorrecta")])
)


;;Ejercicio 2
;;precondiciones: Una fwae expresión representada por algo de tipo AST, un simbolo (symbol?) y otra expresión fwae como valor.
;;postconsiciones: una expresión FWAE representada como AST con el símbolo sustituido por la segunda expresión FWAE.

;; notita aparte: no se como este representado de hecho la aplicaión de funciones aquí pero, cuando lo usas es de la forma: function-list(lambda (x) (cond-fl))(list)
;; es como compactar la estructura d ela lambda: lambda (x) (cuerpo de la función) (lista/params) con funcion-a-aplicar (cond) (lista/params)
(define (subst fwae-expr sub-id value)
  (cond
    [(id? fwae-expr) (if (equal? (id-i fwae-expr) sub-id)
                         value
                         fwae-expr)]
    [(num? fwae-expr) fwae-expr]
    [(bool? fwae-expr) fwae-expr]
    [(op? fwae-expr) (op (op-f fwae-expr) (map (lambda (x) (subst x sub-id value)) (op-args fwae-expr))) ]
    [(with? fwae-expr)  (with (map (lambda (x) (binding (binding-id x) (subst (binding-value x) sub-id value))) (with-bindings fwae-expr)) (if (null? (filter (lambda (x) (eq? (binding-id x) sub-id)) (with-bindings fwae-expr)))
                                                                                  (subst (with-body fwae-expr) sub-id value)
                                                                                  (with-body fwae-expr))) ]
    [(with*? fwae-expr)  (with* (map (lambda (x) (binding (binding-id x) (subst (binding-value x) sub-id value))) (with-bindings fwae-expr)) (if (null? (filter (lambda (x) (eq? (binding-id x) sub-id)) (with-bindings fwae-expr)))
                                                                                  (subst (with-body fwae-expr) sub-id value)
                                                                                  (with-body fwae-expr))) ]
    [(fun? fwae-expr) (fun (fun-params fwae-expr) (if (filter ((lambda (x) (eq? x sub-id)) (fun-params fwae-expr))) ;; sustituimos solo si el sub-id es igual al parametro pasado.
                                                      (subst (fun-body fwae-expr) sub-id value)
                                                      (fun-body fwae-expr)))]
    )
  )