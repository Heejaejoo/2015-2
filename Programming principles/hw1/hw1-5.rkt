#lang racket
(require "hw1-4.rkt")
(provide model make-branch make-mobile)
(provide weight is-balanced?)

(define (model n)
 (leaf n))

(define (make-branch n m)
  (cons n (leaf m)))

(define (make-mobile b1 b2)
  (node (list b1 b2)))

(define (weight m)
  (if (is-leaf? m) (leaf-val m)
      (+ (weight (leaf-val (cdr (nth-child m 0)))) (weight (leaf-val (cdr (nth-child m 1))))))
)

(define (is-balanced? m)
  (if (is-leaf? m) #t
      (if (and
           (and (is-balanced? (leaf-val (cdr (nth-child m 0))))
               (is-balanced? (leaf-val (cdr (nth-child m 1)))))
           (equal? (* (car (nth-child m 0)) (weight (leaf-val (cdr (nth-child m 0)))))
                (* (car (nth-child m 1)) (weight (leaf-val (cdr (nth-child m 1))))
                   )))
          #t #f)))
