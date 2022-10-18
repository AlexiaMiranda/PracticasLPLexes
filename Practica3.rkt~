#lang plai
(define-type AST
  [id (i symbol?)]
  [num (n number?)]
  [bool (b boolean?)]
  [op (f procedure?) (args (listof AST?))]
  [with (bindings (listof binding?)) (body AST?)]
  [with∗ (bindings (listof binding?)) (body AST?)]
  [fun (params (listof symbol?)) (body AST?)]
  [app (fun AST?) (args (listof AST?))]
)

(define-type Binding
  [binding(id symbol?) (value AST?)]
)


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
