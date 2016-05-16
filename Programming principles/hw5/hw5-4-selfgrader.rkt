#lang racket

(require racket/match)
(require "common-grade.rkt")
(require "hw5-4.rkt")

(printf "1. Internal Representations\n")
; In this homework, we strictly guide the internal structure of tiles.
; This auto-grader should results in "O" (rather than "X") for the
; following test cases.
;
;
(printf "1) Basic Representations\n")
; First of all, black tile should be 'B.
(sgoutput (lambda () (equal? 'B black)))
; Likewise, white tile should be 'W.
(sgoutput (lambda () (equal? 'W white)))
; Note that these two basic tile should be both array and tree:
(sgoutput (lambda () (equal? #t (is-tree? black))))
(sgoutput (lambda () (equal? #t (is-tree? white))))
(sgoutput (lambda () (equal? #t (is-array? black))))
(sgoutput (lambda () (equal? #t (is-array? white))))
;
;
; These are for ease of reading this grader.
(define B black)
(define W white)
;
;
; Tiles are of two kinds: arrays and trees.
; These are constructed by: glue-*-from-*.
;
(printf "2) Array Representations\n")
; Array tile should look like:
(define basic-array (glue-array-from-array B B B W))
(sgoutput (lambda () (equal? '(array (B B) (W B))
                           basic-array)))
(sgoutput (lambda () (equal? '(array (B B) (W B))
                           (glue-array-from-tree B B B W))))
;
(printf "3) Tree Representations\n")
; Tree tile should look like:
(define basic-tree (glue-tree-from-tree B B B W))
(sgoutput (lambda () (equal? '(tree B B B W)
                           basic-tree)))
(sgoutput (lambda () (equal? '(tree B B B W)
                           (glue-tree-from-array B B B W))))
;
(printf "4) Bigger Examples\n")
; Try bigger ones.
(define (turn-array pattern i)
  (if (<= i 0)
      pattern
      (turn-array (rotate-array pattern) (- i 1))))
(define (turn-tree pattern i)
  (if (<= i 0)
      pattern
      (turn-tree (rotate-tree pattern) (- i 1))))
(define compound1-array
  (glue-array-from-array
   basic-array
   (turn-array basic-array 1)
   (turn-array basic-array 2)
   (turn-array basic-array 3)))
(define compound2-array
  (rotate-array
   (glue-array-from-array
    basic-array
    basic-array
    (rotate-array basic-array)
    (rotate-array basic-array))))
(sgoutput (lambda () (equal? '(array (B B W B) (W B B B) (B B B W) (B W B B))
                           compound1-array)))
(sgoutput (lambda () (equal? '(array (B W W B) (B B B B) (B W W B) (B B B B))
                           compound2-array)))
(define compound1-tree
  (glue-tree-from-tree
   basic-tree
   (turn-tree basic-tree 1)
   (turn-tree basic-tree 2)
   (turn-tree basic-tree 3)))
(define compound2-tree
  (rotate-tree
   (glue-tree-from-tree
    basic-tree
    basic-tree
    (rotate-tree basic-tree)
    (rotate-tree basic-tree))))
(sgoutput (lambda () (equal? '(tree (B B B W) (W B B B) (B W B B) (B B W B))
                           compound1-tree)))
(sgoutput (lambda () (equal? '(tree (B W B B) (W B B B) (W B B B) (B W B B))
                           compound2-tree)))
(define cp-array-1
  '(array (B B W B W W B B) (W B W W B B B W) (B B W W W W W B) (B W W B W B W W) (W W W B B W B W) (W B W B W W B B) (W W B W B W W B) (W B B B W B W W)))
(define cp-array-2
  '(array (W B W W B W B W) (B W W B W W B W) (B W W W B W B W) (W B W B W W W W) (B B B B B W B W) (W B B W W B W B) (W W B W B W W W) (W B W W W W B W)))
(define cp-array-3
  '(array (B W W W W W B B) (W B B W W B B W) (W B W W B B B W) (B W W B W W W W) (B W B B B W B B) (B B B W W B W W) (B W W W B W B W) (W W W W B W W W)))
(define cp-array-4
  '(array (W W B W B W W W) (B W B W W W B W) (B W W W W B W B) (W B W B W B W W) (W B B B B B W W) (B W B W W W W W) (B W B W B W B W) (B B B W W W W W)))

;(sgoutput (lambda () (equal? '(array (B B W B W W B W) (W B B B B W W W) (W W W W W B B B) (B W W B B B W W) (W W W W B W B W) (B W B W W W B B) (B B W W B W W B) (B W B B W B W W))
;                           (glue-array-from-tree (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 1)))
;                                                 (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 2))))
;                                                 (append '(tree) (list-ref (array-to-tree cp-array-1) 3))
;                                                 (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 4)))))))

;(sgoutput (lambda () (equal? '(tree ((W B W B) (W B W W) (B B W B) (W W B W)) ((W W B W) (W B W W) (B W B W) (W B W B)) ((W W B W) (W W B W) (W W B W) (W B B W)) ((W B W B) (B B W B) (B W W B) (B W B B)))
;                           (glue-tree-from-tree (rotate (append '(tree) (list-ref (array-to-tree cp-array-1) 3)))
;                                                (rotate (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-3) 1)))))
;                                                (rotate (rotate (append '(tree) (list-ref (array-to-tree cp-array-2) 2))))
;                                                (append '(tree) (list-ref (array-to-tree cp-array-4) 4))))))

(sgoutput (lambda () (equal? '(tree
                             (((W W W B) (W W W B) (W W B B) (B B W B)) ((B B B W) (W B B B) (W W B W) (W W W B)) ((W W W B) (B W W B) (B B B W) (W W B W)) ((W B W B) (W B W W) (B B W B) (W W B W)))
                             (((W W B W) (W B B W) (B W B W) (W W B B)) ((W W W W) (W W B W) (B B B W) (W B B B)) ((B W W W) (W B W B) (B W B W) (W B W W)) ((W W B W) (W W B B) (B W W W) (W B B B)))
                             (((W W B B) (W W W B) (W W W B) (W W W B)) ((W B W B) (W W B W) (W W W B) (W B W B)) ((B W B B) (W W W B) (W B W W) (B B W B)) ((W B W W) (W B W W) (W B W B) (B W B W)))
                             (((W W W B) (B W W B) (W W B W) (B W B W)) ((B W W W) (W W W B) (W B W W) (W B B W)) ((B B W W) (W W W W) (B W W W) (B W W W)) ((W B W B) (B B W B) (B W W B) (B W B B))))
                           (glue-tree-from-array (rotate cp-array-1) (rotate (rotate cp-array-3)) (rotate (rotate (rotate cp-array-2))) cp-array-4))))

(sgoutput (lambda () (equal? '(array (W B B B B W W B W W W W W B B B) (W W B W W B B W W B W B W B W B) (W W B B W W B W W W W W W B W B) (W W W B B W W W W W B B B B B W)
                                   (B B W B W B W W W W B W B W B W) (W W B W W B B W B W B W W W W B) (W B W B W B B B W B W W W B W B) (W W W B W W W B W W W B W B W W)
                                   (W W B W B B B W W B W W W W B W) (B W W B W B W W W W W B W B W W) (B B W W B W B W B W B W W B B W) (W B W B B W W W W B W B B B B B)
                                   (W W B W B W W B W W W W B W B W) (B W W W W W B B W B W B W W W B) (W B B B W W B W W B W W B W W B) (B B W W B W B B W B W B W W B W))
                           (glue-array-from-tree (array-to-tree (rotate cp-array-3)) (array-to-tree (rotate (rotate cp-array-4))) (rotate (array-to-tree (rotate cp-array-2))) (rotate (rotate (array-to-tree cp-array-1)))))))

(printf "2. Interface Operability\n")

(printf "1) pprint\n")

; pprint (and pprint-*) should results in string as follows.
; Black (white) tile should be represented as "B" ("W").
; Each row are separated by new line character "\n".
; The last line also should contains "\n" after it.
(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint-array compound1-array))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint-array compound2-array))))
(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint-tree compound1-tree))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint-tree compound2-tree))))
(sgoutput (lambda () (equal? "BBWBWWBB\nWBWWBBBW\nBBWWWWWB\nBWWBWBWW\nWWWBBWBW\nWBWBWWBB\nWWBWBWWB\nWBBBWBWW\n"
                           (pprint-array cp-array-1))))
(sgoutput (lambda () (equal? "BWWWWWBB\nWBBWWBBW\nWBWWBBBW\nBWWBWWWW\nBWBBBWBB\nBBBWWBWW\nBWWWBWBW\nWWWWBWWW\n"
                           (pprint-array cp-array-3))))
(sgoutput (lambda () (equal? "WBWWBWBW\nBWWBWWBW\nBWWWBWBW\nWBWBWWWW\nBBBBBWBW\nWBBWWBWB\nWWBWBWWW\nWBWWWWBW\n"
                           (pprint-tree (array-to-tree cp-array-2)))))
(sgoutput (lambda () (equal? "WWBWBWWW\nBWBWWWBW\nBWWWWBWB\nWBWBWBWW\nWBBBBBWW\nBWBWWWWW\nBWBWBWBW\nBBBWWWWW\n"
                           (pprint-tree (array-to-tree cp-array-4)))))

(printf "2) neighbor\n")

(sgoutput (lambda () (equal? 2 (neighbor-array (list 0 0) compound1-array))))
(sgoutput (lambda () (equal? 6 (neighbor-array (list 2 0) compound1-array))))
(sgoutput (lambda () (equal? 2 (neighbor-array (list 3 3) compound1-array))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 0 3) compound2-array))))
(sgoutput (lambda () (equal? 4 (neighbor-array (list 1 3) compound2-array))))
(sgoutput (lambda () (equal? 2 (neighbor-array (list 2 2) compound2-array))))

(sgoutput (lambda () (equal? 2 (neighbor-tree (list 0 0) compound1-tree))))
(sgoutput (lambda () (equal? 6 (neighbor-tree (list 2 0) compound1-tree))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 3 3) compound1-tree))))
(sgoutput (lambda () (equal? 3 (neighbor-tree (list 0 3) compound2-tree))))
(sgoutput (lambda () (equal? 4 (neighbor-tree (list 1 3) compound2-tree))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 2 2) compound2-tree))))

(sgoutput (lambda () (equal? 2 (neighbor-array (list 0 1 2) cp-array-1))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 2 1 0) cp-array-1))))
(sgoutput (lambda () (equal? 5 (neighbor-array (list 3 2 1) cp-array-1))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 2 1 3) cp-array-2))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 0 2 1) cp-array-2))))
(sgoutput (lambda () (equal? 3 (neighbor-array (list 1 1 2) cp-array-2))))

(sgoutput (lambda () (equal? 3 (neighbor-tree (list 0 0 1) (array-to-tree cp-array-3)))))
(sgoutput (lambda () (equal? 3 (neighbor-tree (list 1 0 3) (array-to-tree cp-array-3)))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 2 1 0) (array-to-tree cp-array-3)))))
(sgoutput (lambda () (equal? 1 (neighbor-tree (list 0 0 0) (array-to-tree cp-array-3)))))
(sgoutput (lambda () (equal? 1 (neighbor-tree (list 2 2 2) (array-to-tree cp-array-4)))))
(sgoutput (lambda () (equal? 2 (neighbor-tree (list 3 3 3) (array-to-tree cp-array-4)))))



(printf "3) translation\n")
(sgoutput (lambda () (equal? compound1-tree (array-to-tree compound1-array))))
(sgoutput (lambda () (equal? compound2-tree (array-to-tree compound2-array))))
(sgoutput (lambda () (equal? compound1-array (tree-to-array compound1-tree))))
(sgoutput (lambda () (equal? compound2-array (tree-to-array compound2-tree))))


(printf "3. Casual Examples\n")


(define basic (glue B B B W))
(define (turn pattern i)
  (if (<= i 0)
      pattern
      (turn (rotate pattern) (- i 1))))
(define compound1
  (glue basic (turn basic 1) (turn basic 2) (turn basic 3)))
(define compound2
  (rotate (glue basic basic (rotate basic) (rotate basic))))

(sgoutput (lambda () (equal? "BBWB\nWBBB\nBBBW\nBWBB\n"
                               (pprint compound1))))
(sgoutput (lambda () (equal? "BWWB\nBBBB\nBWWB\nBBBB\n"
                               (pprint compound2))))

(sgoutput (lambda () (equal? 2 (neighbor (list 0 0) compound1))))
(sgoutput (lambda () (equal? 6 (neighbor (list 2 0) compound1))))
(sgoutput (lambda () (equal? 2 (neighbor (list 3 3) compound1))))
(sgoutput (lambda () (equal? 3 (neighbor (list 0 3) compound2))))
(sgoutput (lambda () (equal? 4 (neighbor (list 1 3) compound2))))
(sgoutput (lambda () (equal? 2 (neighbor (list 2 2) compound2))))
(sgoutput (lambda () (equal? 5 (neighbor (list 2 2 0 1) (glue-array-from-array cp-array-1 (rotate cp-array-3) (rotate (rotate cp-array-4)) cp-array-2)))))
(sgoutput (lambda () (equal? 2 (neighbor (list 2 1 0 3) (glue-array-from-tree (array-to-tree cp-array-3) (array-to-tree (rotate cp-array-2)) (array-to-tree cp-array-1) (rotate (array-to-tree (rotate (rotate cp-array-4)))))))))
(sgoutput (lambda () (equal? 2 (neighbor (list 0 0 1 3) (glue-tree-from-array cp-array-2 (rotate cp-array-3) (rotate (rotate (rotate cp-array-4))) cp-array-1)))))
(sgoutput (lambda () (equal? 4 (neighbor (list 2 1 3 3) (glue-tree-from-tree (array-to-tree cp-array-4) (rotate (array-to-tree cp-array-3)) (array-to-tree (rotate (rotate cp-array-2))) (rotate (rotate (array-to-tree (rotate cp-array-1))))))))) 