#lang racket

(require "hw3-2-library.rkt")

(provide maze-check)

(define (maze-check maze start end)
  (define (add-room room set)
    (if (is-member? room set)
        set
        (if (is-member? end set)
            set
            (add-all add-room (can-enter room maze) (add-element room set)))))
  (is-member? end (add-all add-room (can-enter start maze) (add-element start empty-set)))  
  )

(define (add-all func roomlist set)
    (if (null? roomlist) set
      (add-all func (cdr roomlist) (func (car roomlist) set))))