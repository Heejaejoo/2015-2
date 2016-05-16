#lang racket

(provide crazy2val)

(define (crazy2val lst)
  (if (null? lst) 0
      (if (equal? (car lst) 'z)
          (+ 0 (* 2 (crazy2val (cdr lst))))
          (if (equal? (car lst) 'n) (+ -1 (* 2 (crazy2val (cdr lst)))) (+ 1 (* 2 (crazy2val (cdr lst))))))))

