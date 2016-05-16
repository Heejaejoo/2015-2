#lang racket

;;; If these statements are omitted, your submission will be graded 0.

(provide equal)
(provide size)
(provide beautiful)


; You can use the definitions and functions defined in hw5-4.rkt:
; black, white, glue-array-from-tree, glue-array-from-array,
; rotate-array, neighbor-array, pprint-array, is-array?,
; glue-tree-from-tree, glue-tree-from-array, rotate-tree,
; neighbor-tree, pprint-tree, is-tree?, array-to-tree, tree-to-array,
; glue, rotate, neighbor, pprint
(require "hw5-4.rkt")

;;; interfaces
(define (equal f g) ; equal: form * form -> form
   (cond [(and (symbol? f) (symbol? g)) (equal? f g)]
          [(not (is-array? f)) (equal (tree-to-array f) g)]
          [(not (is-array? g)) (equal f (tree-to-array g))]
          [else (andmap equal? f g)]))
          
(define (size f) ; size: form -> int
  (cond [(equal? 'B f) 0]
        [(equal? 'W f) 0]
        [(is-array? f) (size (array-to-tree f))]
        [else (+ (size (if (list? (list-ref (cdr f) 0)) (cons 'tree (list-ref (cdr f) 0)) (list-ref (cdr f) 0))) 1)]))
 (define (beautiful-sym f)
    (equal f (rotate (rotate f))))
(define (beautiful f) ; beautiful: form -> bool
 
  (define (beautiful-neighbor f)
    (let ([max (size f)]) 
    (define (list-gen f lst)
        (cond
          [(equal? f 'B) lst]
          [(equal? f 'W) lst]
          [(is-array? f) (list-gen (array-to-tree f) lst)]
          [else (append (list-gen (if (list? (list-ref (cdr f) 0)) (cons 'tree (list-ref (cdr f) 0)) (list-ref (cdr f) 0)) (append (list 0) lst))
                          (list-gen (if (list? (list-ref (cdr f) 1)) (cons 'tree (list-ref (cdr f) 1)) (list-ref (cdr f) 1)) (append (list 1) lst))
                          (list-gen (if (list? (list-ref (cdr f) 2)) (cons 'tree (list-ref (cdr f) 2)) (list-ref (cdr f) 2)) (append (list 2) lst))
                          (list-gen (if (list? (list-ref (cdr f) 3)) (cons 'tree (list-ref (cdr f) 3)) (list-ref (cdr f) 3)) (append (list 3) lst)))]))
      (define (checker loc f)
        (and (> (neighbor loc f) 1) (< (neighbor loc f) 6)))
      (define (multiple-chk f check lst)
        (cond [(null? lst) #t]
              [else (if (check (take lst max) f) (multiple-chk f check (list-tail lst max)) #f)]
      ))(multiple-chk f checker (list-gen f '()))))
  (or (beautiful-sym f) (beautiful-neighbor f)))

