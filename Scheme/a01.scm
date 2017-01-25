#lang racket

(define (s-exp exp)
  (cond
    ((number? exp) exp)
    ((symbol? exp) exp)
    ((sum? exp) (s-sum (s-exp (cadr exp)) (s-exp (caddr exp))))
    ((mul? exp) (s-mul (s-exp (cadr exp)) (s-exp (caddr exp))))
    ((sub? exp) (s-minus (s-exp (cadr exp)) (s-exp (caddr exp))))




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
    ((and (number? x) (= x 1)) y)
    ((and (number? y) (= y 1)) x)
    ((and (number? x) (number? y)) (* x y))
    ((and (not (number? x)) (number? y)) (list '* y x))
    (else (list '* x y))
  )
)

(define (s-minus x y)
  (cond
    
    ((and (number? x) (= x 0))
     (if (number? y) (negate y) (list '- y )))
    ((and (number? y) (= y 0)) x )
    ((and (number? x) (number? y)) (- x y))
    ((and (null? y) (number? x)) (negate x))
    ((and (not (number? x)) (number? y)) (list '+ (negate y) x))

  (else (list '+ (negate x) y))
  )
)

(define (negate x)
  (* -1 x)
  )

(define (sum? x)
  (and (pair? x) (eq? (car x) '+))
  )

(define (mul? x)
  (and (pair? x) (eq? (car x) '*))
  )

(define (sub? x)
  (and (pair? x) (eq? (car x) '-))
  )

;;;
;;;Sample tests
;;;(display " -> ")
(display "(+ 1 2) -> ")
(s-exp '(+ 1 2))

(display "(+ a 2) -> ")
(s-exp '(+ a 2))

(display "(* 2 3) -> ")
(s-exp '(* 2 3))

(display "(* a 3) -> ")
(s-exp '(* a 3))

(display "(- 3 2) -> ")
(s-exp '(- 3 2))

(display "(- a 2) -> ")
(s-exp '(- a 2))

(display "(+ a (+ b c)) -> ")
(s-exp '(+ a (+ b c)))

(display "(* a (* b c)) -> ")
(s-exp '(* a (* b c)))