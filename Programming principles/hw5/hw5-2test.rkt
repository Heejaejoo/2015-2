#lang racket

(provide empty link case-list)

(define empty 'empty)

(define (link v l)
  (list v l))

(define (case-list f1 f2 l)
   (if (equal? l empty) (f1 '())
          (f2 (car l) (cadr l))))

