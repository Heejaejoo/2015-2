#lang racket

(require "chal.rkt")


;to use eval
(define ns (make-base-namespace))

(define (test p)
  (define a (myeval p))
  (define b (eval p ns))
  (if (equal? a b)
    (printf "O ")
    (let ()
      (printf "X\n  yours: ")
      (write a) (newline)
      (printf "  ans: ")
      (write b) (newline)
      )))

(fprintf (current-output-port) " \nt1-10\n")

(define t1 '#t)
(test t1)

(define t2 ''())
(test t2)

(define t3 '(+ 1 2))
(test t3)

(define t4 '(+ (* 3 5) (- 6 1)))
(test t4)

(define t5 '(if #f (+ 1 2) (* 3 5)))
(test t5)

(define t6 '(cons 1 2))
(test t6)

(define t7 '(cons (cons 1 2) (cons 3 4)))
(test t7)

(define t8 '(cons (+ 1 2) (* 3 4)))
(test t8)

(define t9 '(car (cons (+ 1 2) (* 3 4))))
(test t9)

(define t10  '(let ((p (cons 1 (cons 2 '())))) (cons 0 p)))
(test t10)

(fprintf (current-output-port) " \nt11-20\n")

(define t11  '(letrec ((p (cons 1 (cons 2 '())))) (cons 0 p)))
(test t11)

(define t12  '((lambda (x) (+ x 1)) 3))
(test t12)

(define t13 '(letrec ((f (lambda (x) (if (= x 0) 1 (* (f (- x 1)) x) )))) (f 5)))
(test t13)

(define t14 '((lambda (x) (if (= x 0) 1 (*((lambda (y) (if (= y 0) 1 (* (- y 1) y) )) (- x 1)) x) )) 5))
(test t14)

(define t14_ '((lambda (x) (if (= x 0) 1 (*((lambda (x) (if (= x 0) 1 (* (- x 1) x) )) (- x 1)) x) )) 5))
(test t14_)



(define t16 '(let ([x 3] [z 1]) ((lambda (x y) (+ x z)) (+ z 1) (+ x 2))))
(test t16)

(define t17 '(letrec ([x 1] [z (* x 2)] [y (+ z 1)]) (if #t ((lambda (x b) (- x b)) y x) (- y x))))
(test t17)

(define t18 '(let ((p (cons 1 (cons 2 '())))) (cons 0 p)))
(test t18)

(define t19 '((car (cons (lambda (x y) (+ x y)) 2)) 2 3))
(test t19)

(define t20 '(let ([x 1] [y 3]) ((lambda (f g) (+ (f x) (g y))) (lambda (y) (* y x)) (lambda (x) (+ x y)))))
(test t20)

(fprintf (current-output-port) " \nt21-30\n")

(define t21 '(let ([x (let ([y 2]) y)]) x))
(test t21)

(define t22 '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 3)))
(test t22)

(define t23 '(letrec ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5)))
(test t23)

(define t24 '(let ((fac (lambda (n) (letrec ((fac-aux (lambda (m r) (if (= m 0) r (fac-aux (- m 1) (* m r)))))) (fac-aux n 1))))) (fac 3)))
(test t24)

(define t25 '(((lambda (f) (lambda (x) (f x))) (lambda (x) (+ x 1))) 2))
(test t25)

(define t26 '((lambda (c) (((lambda (f) (lambda (x) (f x))) (lambda (x) (* c (+ x 1)))) 2)) 2))
(test t26)

(define t27 '(let ([g (lambda (x y) (+ x y))]) (((lambda (f) (lambda (x y) (f x y))) g) 2 3)))
(test t27)

(define t28 '(letrec ((f (lambda (x) (+ x 2)))) (f 5)))
(test t28)

(define t29 '(let ((power2 (lambda (n) (letrec ((power2-aux (lambda (m r) (if (= m 0) r (power2-aux (- m 1) (* 2 r)))))) (power2-aux n 1))))) (power2 3)))
(test t29)

(define t30 '(let ((f (lambda (x) (+ x 3)))) (f 10)))
(test t30)

(fprintf (current-output-port) " \nt31-40\n")

(define t31 '(let ((x 3) (y (+ 2 2)) (b 2) (c 5)) (let ((x x) (y (+ 1 x))) (+ x y))))
(test t31)

(define t32 '(letrec () 3))
(test t32)

(define t33 '(let ([f (lambda () 3)]) (f)))
(test t33)

(define t34 '(let ([x 1] [y 3]) ((lambda (x b) (- x b)) y x)))
(test t34)

(define t35 '((cdr (cons 2 (lambda (x y) (+ x y)))) 2 3))
(test t35)

(define t36 '(let ([f (lambda (f) (+ f 3))]) (f 2)))
(test t36)

(define t37 '(let ((x 3) (y (+ 2 2)) (b 2) (c 5)) (let ((x (+ x y)) (y (+ 1 x))) (+ x y))))
(test t37)

(define t38 '((lambda () 2)))
(test t38)


;;;;;;;;;;;;;;;;;;;;; Error case ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error e)
  (with-handlers (((lambda (x) (string? x)) (lambda (x) x))) (myeval e)))

(fprintf (current-output-port) " \n\nError test\n")

(fprintf (current-output-port) " \n - undefined - \n")


(define e1 '(letrec ((f (lambda (x) g))(a (f 1))(g (lambda (x) 1))) 1)) ;undefined
(define e2 '(let ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5))) ;undefined
(define e3  '(let ((x 1) (y (+ x 1))) y)) ;undefined
(define e4 '(letrec ((x (+ y 1)) (y 1)) x)) ;undefined
(define e5 '(letrec ((b a) (a 10)) b)) ;undefined
(define e6 '(let ((x 3) (y (+ 2 2)) (b 2) (c 5)) (letrec ((x (+ x y)) (y (+ 1 x))) (+ x y)))) ; undefined
(define e7 '(letrec ((x 3) (y (+ 2 2)) (b 2) (c 5)) (letrec ((x (+ x y)) (y (+ 1 x))) (+ x y)))) ;undefined
(define e8 '(let ([x 2]) (letrec ([x (+ x 1)]) (+ x 2)))) ; undefined 
(define e9 '(let ([x 2]) (letrec ([y (+ x 1)] [x (+ y 1)]) (+ x y)))) ;undefined
(define e10 '(let ((y 1)) (letrec ((udd (lambda (x) (+ x z)))) (let ((z 10)) (udd 8))))) ;undefined
(error e1)
(error e2)
(error e3)
(error e4)
(error e5)
(error e6)
(error e7)
(error e8)
(error e9)
(error e10)

(fprintf (current-output-port) " \n - application: not a procedure - \n")

(define e11 '((let ([h (lambda (x y) (+ x y))] [a 1] [b 2]) (h a b)))) ; application: not a procedure;
(error e11)

(fprintf (current-output-port) " \n - bad syntax - \n")
(define e12 '(letrec ([1 2]) (+ 1 3))) ;bad syntax
(error e12)

(fprintf (current-output-port) " \n - duplicate - \n")
(define e13 '(let ([x 1] [x 2]) x))
(error e13)

