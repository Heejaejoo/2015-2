#lang racket

(provide iter)

(define (iter n f) 
 (lambda (x)
   (if (= n 0) x
          (if (> n 0) (f ((iter (- n 1) f) x))
              (f ((iter (+ n 1) f) x))))))
