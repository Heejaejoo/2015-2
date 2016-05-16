#lang racket
(require "hw1-4.rkt")
(provide zero one not-c and-c or-c)
(provide is-zero? is-one? is-not? is-and? is-or? sub-circuit)

(define zero
  (leaf 0))
(define one
  (leaf 1))
(define (not-c c)
  (node (list (leaf 'not) c)))
(define (and-c c1 c2)
  (node (list (leaf 'and) c1 c2)))
(define (or-c c1 c2)
  (node (list (leaf 'or) c1 c2)))

(define (is-zero? c)
  (equal? (leaf-val c) 0))
(define (is-one? c)
  (equal? (leaf-val c) 1))

(define (is-not? c)
  (equal? (leaf-val (nth-child c 0)) 'not))
(define (is-and? c)
  (equal? (leaf-val (nth-child c 0)) 'and))
(define (is-or? c)
  (equal? (leaf-val (nth-child c 0)) 'or))

(define (sub-circuit c n)
  (nth-child c (+ n 1)))

(define c1 zero)
(define c2 one)
(define c3 (not-c c1))
(define c4 (and-c c2 c3))
(define c5 (not-c c4))
(define c6 (or-c c3 c5))
