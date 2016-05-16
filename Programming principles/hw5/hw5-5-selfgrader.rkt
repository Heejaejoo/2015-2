#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw5-4.rkt")
(require "hw5-5.rkt")

(define B black)
(define W white)
(define basic (glue B B B W))
(define (turn pattern i)
  (if (<= i 0)
      pattern
      (turn (rotate pattern) (- i 1))))
(define compound1
  (glue basic (turn basic 1) (turn basic 2) (turn basic 3)))
(define compound2
  (rotate (glue basic basic (rotate basic) (rotate basic))))
(define compound3
  (glue compound1 compound2 (turn compound1 2) (turn compound2 2)))
(define cp-array-1
  '(array (B B W B W W B B) (W B W W B B B W) (B B W W W W W B) (B W W B W B W W) (W W W B B W B W) (W B W B W W B B) (W W B W B W W B) (W B B B W B W W)))
(define cp-array-2
  '(array (W B W W B W B W) (B W W B W W B W) (B W W W B W B W) (W B W B W W W W) (B B B B B W B W) (W B B W W B W B) (W W B W B W W W) (W B W W W W B W)))
(define cp-array-3
  '(array (B W W W W W B B) (W B B W W B B W) (W B W W B B B W) (B W W B W W W W) (B W B B B W B B) (B B B W W B W W) (B W W W B W B W) (W W W W B W W W)))
(define cp-array-4
  '(array (W W B W B W W W) (B W B W W W B W) (B W W W W B W B) (W B W B W B W W) (W B B B B B W W) (B W B W W W W W) (B W B W B W B W) (B B B W W W W W)))
(define cp-array-5
  (glue-array-from-tree (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 1)))
                                                 (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 2))))
                                                 (append '(tree) (list-ref (array-to-tree cp-array-1) 3))
                                                 (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 4)))))
(define cp-array-6
  (glue-tree-from-tree (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 3)))
                                                (rotate (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-3) 1)))))
                                                (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-2) 2))))
                                                (append '(tree) (list-ref (array-to-tree cp-array-4) 4))))
(define cp-array-7
  (glue-tree-from-array (rotate cp-array-1) (rotate (rotate cp-array-3)) (rotate (rotate (rotate cp-array-2))) cp-array-4))
(define cp-array-8
  (glue-array-from-tree (array-to-tree (rotate cp-array-3)) (array-to-tree (rotate (rotate cp-array-4))) (rotate (array-to-tree (rotate cp-array-2))) (rotate (rotate (array-to-tree cp-array-1)))))


;;; beautiful test

(sgoutput (lambda () (equal? 0 (size B))))
(sgoutput (lambda () (equal? 0 (size W))))
(sgoutput (lambda () (equal? 1 (size basic))))
(sgoutput (lambda () (equal? 2 (size compound1))))
(sgoutput (lambda () (equal? 2 (size compound2))))
(sgoutput (lambda () (equal? 3 (size compound3))))
(sgoutput (lambda () (equal? 3 (size cp-array-1))))
(sgoutput (lambda () (equal? 3 (size (array-to-tree cp-array-4)))))
(sgoutput (lambda () (equal? 3 (size cp-array-6))))
(sgoutput (lambda () (equal? 4 (size (array-to-tree cp-array-8)))))
(sgoutput (lambda () (equal? #t (beautiful compound1))))
(sgoutput (lambda () (equal? #f (beautiful compound2))))
(sgoutput (lambda () (equal? #t (beautiful compound3))))
(sgoutput (lambda () (equal? #f (beautiful cp-array-3))))
(sgoutput (lambda () (equal? #f (beautiful (array-to-tree cp-array-5)))))
(sgoutput (lambda () (equal? #f (beautiful (tree-to-array cp-array-7)))))
(sgoutput (lambda () (equal? #f (beautiful cp-array-8)))) 