#lang racket

(require "hw1-2.rkt")
(provide crazy2mul)

(define (crazy2mul lhs rhs)
	(if (null? rhs) null
            (crazy2add (list_mul lhs (car rhs)) (cons 'z (crazy2mul lhs (cdr rhs))))))
         
(define (list_mul lhs num)
  (case num
    [(z) '(z)]
    [(p) lhs]
    [(n) (cons (sign_change (car lhs)) (if (null? (cdr lhs)) '() (list_mul (cdr lhs) num)))]))

(define (sign_change num)
  (case num
    [(z) 'z]
    [(p) 'n]
    [(n) 'p]
    ))
