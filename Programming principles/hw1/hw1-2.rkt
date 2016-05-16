#lang racket

(provide crazy2add)

(define (crazy2add lhs rhs)
  (if (and (null? lhs) (null? rhs)) null
      (if (null? lhs) rhs
          (if (null? rhs) lhs
               (cons (car (list_num_adder lhs (if (pair? rhs) (car rhs) rhs))) (crazy2add (cdr (list_num_adder lhs (if (pair? rhs) (car rhs) rhs))) (if (pair? rhs) (cdr rhs) null))))
              )))

(define (list_num_adder lhs num)
  (if (equal? num 'z) lhs
      (if (null? lhs) (cons num '())
          (cons (car (crazy2add_mini (if (pair? lhs) (car lhs) lhs) num)) (list_num_adder (if (pair? lhs) (cdr lhs) null) (cdr (crazy2add_mini (if (pair? lhs) (car lhs) lhs) num)))))))


 (define (crazy2add_mini val1 val2)
   (case (cons val1 val2)
     [((p . p)) '(z . p)]
     [((p . n) (n . p) (z . z)) '(z . z)]
     [((p . z) (z . p)) '(p . z)]
     [((n . z) (z . n)) '(n . z)]
     [((n . n)) '(z . n)]))
     
       