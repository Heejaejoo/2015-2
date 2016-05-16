#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

; Play around the interface of hw3-3-library.rkt.
; After executing this file, see hw3-3.ps.
; To read .ps files, you will need
;  - GhostScript: http://www.ghostscript.com/download/gsdnld.html
;  - Ghostview: http://pages.cs.wisc.edu/~ghost/gsview/

; some test code:
(define maze1 (init-maze 4 3))
(define maze2 (open-s 2 1 maze1))
(maze-pp maze2)

(define (walk-possible-to nextroom n m)
       (if (or (> (car nextroom) n) (< (car nextroom) 0)) #f
           (if (or (> (cdr nextroom) m) (< (cdr nextroom) 0)) #f #t)))

(define (last-val list)
       (cdr list))
     
(define (add-room list room)
       (cons list room))
       
(define (number-of-list list)
  (define (number-count lst cnt)
    (if (integer? (cdr lst))
        cnt
        (number-count (car lst) (+ 1 cnt))))
  (number-count list 1))

(define (same? r1 r2)
  (equal? r1 r2))
     
(define (open-maze n1 m1 x maze)
       (case x
         [(0) (open-n n1 m1 maze)]
         [(1) (open-ne n1 m1 maze)]
         [(2) (open-se n1 m1 maze)]
         [(3) (open-s n1 m1 maze)]
         [(4) (open-sw n1 m1 maze)]
         [(5) (open-nw n1 m1 maze)]
         )
       )
(define (next-n? n1 m1 d)
       (if (even? m1)
           (if (= d 0) n1 (+ n1 1))
           (if (= d 0) (- n1 1) m1)))
     
(define (in-the-list? list room)
       (if (integer? (cdr list)) (same? list room)
           (if (same? (cdr list) room) #t
               (in-the-list? (car list) room))))
(define (next-room n1 m1 x)
       (case x
         [(0) (cons (- n1 1) m1)]
         [(1) (cons (next-n? n1 m1 0) (+ m1 1))]
         [(2) (cons (next-n? n1 m1 1) (+ m1 1))]
         [(3) (cons (+ n1 1) m1)]
         [(4) (cons (next-n? n1 m1 1) (- m1 1))]
         [(5) (cons (next-n? n1 m1 0) (- m1 1))]
         ))

(define (no-where-to-go? n1 m1 list n m)
         (and [not (and (not (in-the-list? list (next-room n1 m1 0))) (walk-possible-to (next-room n1 m1 0) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 1))) (walk-possible-to (next-room n1 m1 1) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 2))) (walk-possible-to (next-room n1 m1 2) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 3))) (walk-possible-to (next-room n1 m1 3) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 4))) (walk-possible-to (next-room n1 m1 4) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 5))) (walk-possible-to (next-room n1 m1 5)) n m)]))
 (define (open-possible? n1 m1 list x n m)
       (case x
         [(0) (and (not (in-the-list? list (next-room n1 m1 0))) (walk-possible-to (next-room n1 m1 0) n m))]
         [(1) (and (not (in-the-list? list (next-room n1 m1 1))) (walk-possible-to (next-room n1 m1 1) n m))]
         [(2) (and (not (in-the-list? list (next-room n1 m1 2))) (walk-possible-to (next-room n1 m1 2) n m))]
         [(3) (and (not (in-the-list? list (next-room n1 m1 3))) (walk-possible-to (next-room n1 m1 3) n m))]
         [(4) (and (not (in-the-list? list (next-room n1 m1 4))) (walk-possible-to (next-room n1 m1 4) n m))]
         [(5) (and (not (in-the-list? list (next-room n1 m1 5))) (walk-possible-to (next-room n1 m1 5) n m))]
       ))
     
(define (mazeGen n m)
  (define entrance (cons 0 0))
   (define exit (cons (- n 1) (- m 1)))
   (let ((maze (init-maze n m)))
      (let ([initlist entrance] [base (open-s (car exit) (cdr exit) (open-n (car entrance) (cdr entrance) maze))])
        (define (maze-walker list n1 m1 maze n m)
          (let ([x (random 6)])
            (if (= (number-of-list list) (* n m))
                'TODO'
                (if (no-where-to-go? n1 m1 list n m)
                            (maze-walker list (car (last-val list)) (cdr (last-val list)) maze n m)
                            (if (open-possible? n1 m1 list x n m)
                                (maze-walker (add-room list (next-room n1 m1 x)) (car (next-room n1 m1 x)) (cdr (next-room n1 m1 x)) (open-maze n1 m1 x maze) n m)
                                (maze-walker (list n1 m1 maze n m)))))))
          (maze-walker initlist (car entrance) (cdr entrance) base n m)
        
)))
