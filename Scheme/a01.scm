#lang racket
(define (factorial x)
  (if (= x 0) 1 (* x (factorial(- x 1)))))


(define (s-exp exp)
  (cond
    ((number? exp) exp)
    ((symbol? exp) exp)
    ((sum? exp) (s-sum (s-exp (cadr exp)) (s-exp (caddr exp))))
    ((mul? exp) (s-mul (s-exp (cadr exp)) (s-exp (caddr exp))))




    (else "Whoops! I don't understand this expression.")
    )
  )

(define (s-sum x y)
  (cond
    ((and (number? x) (= x 0)) y)
    ((and (number? y) (= y 0)) x)
    ((and (number? x) (number? y)) (+ x y))
    ((and (not (number? x)) (number? y)) (list '+ y x))
    (else (list '+ x y))
  )
)

(define (s-mul x y)
  (cond
    ((and (number? x) (= x 0)) 0)
    ((and (number? y) (= y 0)) 0)
    ((and (number? x) (number? y)) (* x y))
    ((and (not (number? x)) (number? y)) (list '* y x))
    (else (list '* x y))
  )
)

(define (sum? x)
  (and (pair? x) (eq? (car x) '+))
  )

(define (mul? x)
  (and (pair? x) (eq? (car x) '*))
  )