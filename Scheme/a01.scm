;;; Boniface Sindala and Andrew Markley
;;; CS401
;;; Assignment 1

(define (s-exp exp)
  (let (( x (eval exp)))
    (if (equal? exp x )
        exp
        (s-exp x)
    )
  )
)

(define (eval exp)
  (cond
      ;;Fully simplified
      ((number? exp) exp)

      ;;Undefined variable
      ((symbol? exp) exp)

      ;;Nested summation, second sum at index 2
      ((and (sum? exp) (> (length exp) 2) (sum? (caddr exp)) (not (pair? (cadr exp))))
       (s-sum3 (cadr exp) (cadr (caddr exp)) (caddr (caddr exp))))

      ;;Nested summation, second sum at index 1
      ((and (sum? exp) (> (length exp) 2) (sum? (cadr exp)) (not (pair? (caddr exp))))
       (s-sum3 (cadr (cadr exp)) (caddr (cadr exp)) (caddr exp)))

      ;;Nested multiplication, second mul at index 2
      ((and (mul? exp) (> (length exp) 2) (mul? (caddr exp)) (not (pair? (cadr exp))))
       (s-mul3 (cadr exp) (cadr (caddr exp)) (caddr (caddr exp))))

      ;;Nested multiplication, second mul at index 1
      ((and (mul? exp) (> (length exp) 2) (mul? (cadr exp)) (not (pair? (caddr exp))))
       (s-mul3 (cadr (cadr exp)) (caddr (cadr exp)) (caddr exp)))

      ;;Nested mul with sum at index 1
      ((and (mul? exp) (sum? (cadr exp)))
       (s-mul-sum (caddr exp) (cadr exp)))

      ;;Nested mul with sum at index 2
      ((and (mul? exp) (sum? (caddr exp)))
       (s-mul-sum (cadr exp) (caddr exp)))

      ;;Nested mul with sub at index 1
      ((and (mul? exp) (sub? (cadr exp)))
       (s-mul-sub (caddr exp) (cadr exp)))

      ;Nested mul with sub at index 2
      ((and (mul? exp) (sub? (caddr exp)))
       (s-mul-sub (cadr exp) (caddr exp)))
    
      ;;Simple addition
      ((and (sum? exp) (not (pair? (cadr exp))) (not (pair? (caddr exp))))
       (s-sum (s-exp (cadr exp)) (s-exp (caddr exp))))

      ;;Simple multiplication
      ((and (mul? exp) (not (pair? (cadr exp))) (not (pair? (caddr exp))))
       (s-mul (s-exp (cadr exp)) (s-exp (caddr exp))))

      ;;Simple subtraction
      ((and (sub? exp) (not (pair? (cadr exp))) (not (pair? (caddr exp))))
       (s-minus (s-exp (cadr exp)) (s-exp (caddr exp))))

      (else exp)
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

(define (s-sum3 x y z)
  (cond
    ((and (symbol? x) (symbol? y) (symbol? z) (list '+ (s-sum x y) z)))
    ((and (number? x) (symbol? y) (number? z) (list '+ (s-sum x z) y)))
    (else "Nope")
  )
)

(define (s-mul-sum var add)
  (list '+ (s-mul var (cadr add)) (s-mul var (caddr add)))
  )

(define (s-mul-sub var sub)
  (list '- (s-mul var (cadr sub)) (s-mul var (caddr sub)))
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

(define (s-mul3 x y z)
  (cond
    ((and (symbol? x) (symbol? y) (symbol? z) (list '* (s-mul x y) z)))
    ((and (number? x) (symbol? y) (number? z) (list '* (s-mul x z) y)))
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
    ((and (number? x) (symbol? y)) (list '+ (list '- y) x))
    (else (list '+ (negate y) x))
  )
)

(define (negate x)
  (cond
     ((number? x) (* -1 x))
     (else (list '- x))
  )
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

;1
(display "\n(+ 1 2) -> ")
(display (s-exp '(+ 1 2)))

;2
(display "\n(+ a 2) -> ")
(display (s-exp '(+ a 2)))

;3
(display "\n(* 2 3) -> ")
(display (s-exp '(* 2 3)))

;4
(display "\n(* a 3) -> ")
(display (s-exp '(* a 3)))

;5
(display "\n(- 3 2) -> ")
(display (s-exp '(- 3 2)))

;6
(display "\n(- a 2) -> ")
(display (s-exp '(- a 2)))

;7
(display "\n(+ a (+ b c)) -> ")
(display (s-exp '(+ a (+ b c))))

;8
(display "\n(* a (* b c)) -> ")
(display (s-exp '(* a (* b c))))

;9
(display "\n(+ (+ 1 a) 2) -> ")
(display (s-exp '(+ (+ 1 a) 2)))

;10
(display "\n(* (* 2 a) 3) -> ")
(display (s-exp '(* (* 2 a) 3)))

;11
(display "\n(* (+ 2 a) 3) -> ")
(display (s-exp '(* (+ 2 a) 3)))

;12
(display "\n(* 2 (+ 3 a)) -> ")
(display (s-exp '(* 2 (+ 3 a))))

;13
(display "\n(* (+ a b) 2)) -> ")
(display (s-exp '(* (+ a b) 2)))

;14
(display "\n(* 3 (+ a b)) -> ")
(display (s-exp '(* 3 (+ a b))))

;15
(display "\n(* (- a b) 2)) -> ")
(display (s-exp '(* (- a b) 2)))

;16
(display "\n(* 2 (- a b)) -> ")
(display (s-exp '(* 2 (- a b))))

;17
(display "\n(* (+ a b) c)) -> ")
(display (s-exp '(* (+ a b) c)))

;18
(display "\n(* a (+ b c)) -> ")
(display (s-exp '(* a (+ b c))))

;19
(display "\n(* (- a b) c) -> ")
(display (s-exp '(* (- a b) c)))

;20
(display "\n(* a (- b c)) -> ")
(display (s-exp '(* a (- b c))))