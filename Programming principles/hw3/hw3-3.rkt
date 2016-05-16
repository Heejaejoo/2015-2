#lang racket

(require "hw3-3-library.rkt")

(provide mazeGen)

; Play around the interface of hw3-3-library.rkt.
; After executing this file, see hw3-3.ps.
; To read .ps files, you will need
;  - GhostScript: http://www.ghostscript.com/download/gsdnld.html
;  - Ghostview: http://pages.cs.wisc.edu/~ghost/gsview/

; some test code:


(define (walk-possible-to nextroom n m)
       (if (or (>= (car nextroom) n) (< (car nextroom) 0)) #f
           (if (or (>= (cdr nextroom) m) (< (cdr nextroom) 0)) #f #t)))

(define (last-val list n1 m1)
       (if (integer? (cdr (car list))) (car list)
           (if (same? (cdr list) (cons n1 m1)) (cdr (car list))
           (last-val (car list) n1 m1))))
     
(define (add-room list room)
       (cons list room))
       
(define (number-of-list list)
  (define (number-count lst cnt)
    (if (integer? (cdr lst))
        cnt
        (number-count (car lst) (+ 1 cnt))))
  (number-count list 1))

(define (same? r1 r2)
  (and (equal? (car r1) (car r2)) (equal? (cdr r1) (cdr r2))))
     
(define (open-maze n1 m1 x maze)
       (case (+ 0 x)
         [(0) (open-n n1 m1 maze)]
         [(1) (open-ne n1 m1 maze)]
         [(2) (open-se n1 m1 maze)]
         [(3) (open-s n1 m1 maze)]
         [(4) (open-sw n1 m1 maze)]
         [(5) (open-nw n1 m1 maze)]
         )
       )
     
(define (in-the-list? list room)
       (if (integer? (cdr list)) (same? list room)
           (if (same? (cdr list) room) #t
               (in-the-list? (car list) room))))

(define (next-room n1 m1 x)
       (case (+ 0 x)
         [(0) (cons (- n1 1) m1)]
         [(1) (cons (if (even? m1) (if (odd? x) n1 (+ n1 1)) (if (odd? x) (- n1 1) n1)) (+ m1 1))]
         [(2) (cons (if (even? m1) (if (odd? x) n1 (+ n1 1)) (if (odd? x) (- n1 1) n1)) (+ m1 1))]
         [(3) (cons (+ n1 1) m1)]
         [(4) (cons (if (even? m1) (if (odd? x) n1 (+ n1 1)) (if (odd? x) (- n1 1) n1)) (- m1 1))]
         [(5) (cons (if (even? m1) (if (odd? x) n1 (+ n1 1)) (if (odd? x) (- n1 1) n1)) (- m1 1))]
         ))

(define (no-where-to-go? n1 m1 list n m)
         (and [not (and (not (in-the-list? list (next-room n1 m1 0))) (walk-possible-to (next-room n1 m1 0) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 1))) (walk-possible-to (next-room n1 m1 1) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 2))) (walk-possible-to (next-room n1 m1 2) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 3))) (walk-possible-to (next-room n1 m1 3) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 4))) (walk-possible-to (next-room n1 m1 4) n m))]
             [not (and (not (in-the-list? list (next-room n1 m1 5))) (walk-possible-to (next-room n1 m1 5) n m))]))

 (define (open-possible? n1 m1 list x n m)
       (case (+ 0 x)
         [(0) (and (not (in-the-list? list (next-room n1 m1 0))) (walk-possible-to (next-room n1 m1 0) n m))]
         [(1) (and (not (in-the-list? list (next-room n1 m1 1))) (walk-possible-to (next-room n1 m1 1) n m))]
         [(2) (and (not (in-the-list? list (next-room n1 m1 2))) (walk-possible-to (next-room n1 m1 2) n m))]
         [(3) (and (not (in-the-list? list (next-room n1 m1 3))) (walk-possible-to (next-room n1 m1 3) n m))]
         [(4) (and (not (in-the-list? list (next-room n1 m1 4))) (walk-possible-to (next-room n1 m1 4) n m))]
         [(5) (and (not (in-the-list? list (next-room n1 m1 5))) (walk-possible-to (next-room n1 m1 5) n m))]
       ))
     
(define (mazeGen n m)
   (let ((maze (init-maze n m)))
    (let ([entrance (cons 0 (random m))] [exit (cons (- n 1) (random m))])
      (let ([base (open-s (car exit) (cdr exit) (open-n (car entrance) (cdr entrance) maze))] [initlist entrance])
        (define (maze-walker list n1 m1 maze n m)
          (let ([x (random 6)])
            (if (= (number-of-list list) (* n m)) maze
                (if (no-where-to-go? n1 m1 list n m) (maze-walker list (car (last-val list n1 m1)) (cdr (last-val list n1 m1)) maze n m)
                    (if (open-possible? n1 m1 list x n m)
                        (maze-walker (add-room list (next-room n1 m1 x)) (car (next-room n1 m1 x)) (cdr (next-room n1 m1 x)) (open-maze n1 m1 x maze) n m)
                        (maze-walker list n1 m1 maze n m))))))
          (maze-walker initlist (car entrance) (cdr entrance) base n m)
        
))))
(define m1 (mazeGen 10 10))
(maze-pp m1)