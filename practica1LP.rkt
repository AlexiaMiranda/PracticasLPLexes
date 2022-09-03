#lang plai
(define (filtra-lista ls p)
  (match ls
    ['() '()]
    [(cons x xs) (if (p x) (cons x (filtra-lista xs p))
                     (filtra-lista xs p))]))


(define (tipos-ls ls)
  (map tipo ls))

(define (tipo a)
  (cond
    [(number? a) "number"]
    [(boolean? a) "boolean"]
    [(char? a) "character"]
    [(string? a) "string"]
    [(symbol? a) "symbol"]
    [(list? a) "list"]
    [else "no se conoce el tipo"]))

;(define (raro? n)
;  (+(map (expt (car(reverse (digitos n)) raro? cdr (reverse (digitos n))) (lenght (digitos n))) (reverse (digitos n))))

(define (raro? n)
       (= (foldr + 0 (map (lambda (x) (expt x (length (digitos n)))) (digitos n))) n))

(define (digitos n)
  (if (zero? n) '()
  (cons (modulo n 10) (digitos (quotient n 10)))))

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

(define (palindromo? a)
  (if (empty? a)
      #t
      (equal? cadena-invertida a) a)
      )
  )

(define (cadena-invertida seq)
    (cond ((null? seq) seq)
          ((string? seq) (list->string (reverse (string->list seq))))))