#lang racket
(require "hw1-6.rkt")
(provide output)

(define (output c)
  (cond
    [(is-zero? c) 0]
    [(is-one? c) 1]
    [(is-not? c) (if (equal? (output (sub-circuit c 0)) 0) 1 0)]
    [(is-and? c) (if (and (equal? (output (sub-circuit c 0)) 1) (equal? (output (sub-circuit c 1)) 1)) 1
                      0)]
    [(is-or? c) (if (and (equal? (output (sub-circuit c 0)) 0) (equal? (output (sub-circuit c 1)) 0)) 0
                    1)]))
