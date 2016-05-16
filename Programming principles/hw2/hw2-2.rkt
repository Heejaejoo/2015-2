#lang racket

(provide zipperN)

(define (zipperN l)
  (if (null? l) '()
      (append (list_1st l) (zipperN (list_left l)))))

(define (list_1st l)
  (if (null? l) '()
      (remove '() (cons (if (null? (car l)) null (car (car l))) (list_1st (cdr l))))))

(define (list_left l)
  (if (null? l) '()
      (remove '() (cons (if (null? (car l)) null (cdr (car l))) (list_left (cdr l))))))
