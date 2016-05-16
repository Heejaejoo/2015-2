#lang racket
(provide leaf node)
(provide is-empty-tree? is-leaf? leaf-val nth-child)

(define (leaf n)
  (cons n 'leaf))

(define (node l)
 (cons l 'node))

(define (is-empty-tree? t)
  (if (and (or (equal? 'leaf (cdr t)) (equal? 'node (cdr t))) (equal? (car t) '())) #t #f))

(define (is-leaf? t)
  (if (equal? (cdr t) 'leaf) #t #f))

(define (leaf-val t)
  (if (null? t) null (car t)))

(define (nth-child t n)
  (list-ref (car t) n))
