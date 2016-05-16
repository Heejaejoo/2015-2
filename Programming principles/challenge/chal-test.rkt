
#lang racket

(require "chal.rkt")

(printf (if (= (myeval '(if #t 0 1)) 0) "O\n" "X\n"))

(printf (if (= (myeval '(let ((x 3)) (+ x 1))) 4) "O\n" "X\n"))

;to use eval
(define ns (make-base-namespace))

(define (test p)
  (define a (myeval p))
  (define b (eval p ns))
  (if (equal? a b)
    (printf "O\n")
    (let ()
      (printf "X\n  yours: ")
      (write a) (newline)
      (printf "  ans: ")
      (write b) (newline)
      )))

(define t0 '3)
(test t0)

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

(define t15 '(letrec ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5)))
(test t15)

(define t16 '(letrec ((local-even? (lambda (n)
                        (if (= n 0) #t
                            (local-odd? (- n 1)))))
         (local-odd? (lambda (n)
                       (if (= n 0) #f
                           (local-even? (- n 1))))))
  (cons (local-even? 23) (local-odd? 23))))
(test t16)

(define t17 '(((lambda (f) (lambda (x) (f x))) (lambda (x) (+ x 1))) 2))
(test t17)

(define t18 '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 3)))
(test t18)

(define t19 '(letrec ((f (lambda (x) (if (= x 0) 0 (+ x (f (- x 1))))))) (f 5)))
(test t19)

(define t20 '(let ((fac (lambda (n) (letrec ((fac-aux (lambda (m r) (if (= m 0) r (fac-aux (- m 1) (* m r)))))) (fac-aux n 1))))) (fac 3)))
(test t20)

(define t21 '(let ((x 1) (y 2)) (let ((x (+ x y)) (y (+ 3 x))) (* x y))))
(test t21)

(define t22 '(let ((power2 (lambda (n) (letrec ((power2-aux (lambda (m r) (if (= m 0) r (power2-aux (- m 1) (* 2 r)))))) (power2-aux n 1))))) (power2 3)))
(test t22)

(define t23 '((lambda (c) (((lambda (f) (lambda (x) (f x))) (lambda (x) (* c (+ x 1)))) 2)) 2))
(test t23)
;neoguri
(define t24 '(let ([x 2] [f (lambda (x) (+ x 3))]) (let ([x 5] [g (lambda (x) (f x))]) (g x))))
(test t24)

