#lang racket

(provide black)
(provide white)

(provide glue-array-from-tree)
(provide glue-array-from-array)
(provide rotate-array)
(provide neighbor-array)
(provide pprint-array)
(provide is-array?)

(provide glue-tree-from-tree)
(provide glue-tree-from-array)
(provide rotate-tree)
(provide neighbor-tree)
(provide pprint-tree)
(provide is-tree?)

(provide array-to-tree)
(provide tree-to-array)

(provide glue)
(provide rotate)
(provide neighbor)
(provide pprint)


;;; primitive tile

(define black ; black: form
  'B)
(define white ; white: form
  'W)


;;; complex tile
;
; An array tile looks like:
; (cons 'array (list row_1 row_2 ... row_n)),
; for each row_i = (list cell_i1 ... cell_in).
;
; Examples:
; 1.
; (cons 'array (list (list 'B 'B) (list 'W 'W)))
; BB
; WW
;
; 2.
; (cons 'array (list (list 'B 'B 'B 'B) (list 'B 'B 'B 'B) (list 'W 'W 'B 'B) (list 'W 'W 'B 'B)))
; BBBB
; BBBB
; WWBB
; WWBB
;
;
; An tree tile looks like:
; (cons 'tree (list subtree_nw subtree_ne subtree_se subtree_sw)).
;
; Examples:
; 1.
; (cons 'tree (list 'B 'B 'W 'B))
; BB
; BW
;
; 2.
; (cons 'tree (list (list 'B 'B 'B 'W) (list 'B 'B 'W 'B) (list 'B 'W 'B 'B) (list 'W 'B 'B 'B)))
; BBBB
; WBBW
; WBBW
; BBBB
;
; See hw5-4-selfgrader.rkt for more details on grading array and tree representation.

(define (glue-array-from-tree nw ne se sw) ; glue-array-from-tree: form * form * form * form -> form
  (glue-array-from-array (tree-to-array nw) (tree-to-array ne) (tree-to-array se) (tree-to-array sw)))

(define (glue-array-from-array nw ne se sw) ; glue-array-from-array: form * form * form * form -> form
  (if (list? nw)
      (cons 'array
        (append (map (lambda (list1 list2) (append list1 list2)) (cdr nw) (cdr ne)) (map (lambda (list1 list2) (append list1 list2)) (cdr sw) (cdr se))))
      (cons 'array (list (list nw ne) (list sw se)))))

(define (glue-tree-from-tree nw ne se sw) ; glue-tree-from-tree: form * form * form * form -> form
  (if (list? nw)
      (cons 'tree (list (cdr nw) (cdr ne) (cdr se) (cdr sw)))
      (cons 'tree (list nw ne se sw))))

(define (glue-tree-from-array nw ne se sw) ; glue-tree-from-array: form * form * form * form -> form
  (glue-tree-from-tree (array-to-tree nw) (array-to-tree ne) (array-to-tree se) (array-to-tree sw)))
                        
(define (rotate-array f) ; rotate-array: form -> form
  (cond [(equal? 'B f) 'B]
         [(equal? 'W f) 'W]
         [(is-basic-array? (cdr f)) (cons 'array (list (list (list-ref (list-ref (cdr f) 1) 0) (list-ref (list-ref (cdr f) 0) 0))
                                                       (list (list-ref (list-ref (cdr f) 1) 1) (list-ref (list-ref (cdr f) 0) 1))))]
         [else (let ([quat (array-quarter-divide (cdr f))]) (glue-array-from-array (rotate-array (cons 'array (list-ref quat 3))) (rotate-array (cons 'array (list-ref quat 0)))
                                                                                   (rotate-array (cons 'array (list-ref quat 1))) (rotate-array (cons 'array (list-ref quat 2)))))]))

(define (neighbor-array location f) ; neighbor-array: location * form -> int
  (cond [(equal? 'B f) 1]
         [(equal? 'W f) 0]
         [else 
          (let ([loc-a (loc-trans-tree-to-arr location)] [max (- (find-two-exp (length location)) 1)])
                (examine-l (available-loc loc-a max) f))]))

(define (examine-l lst f)
  (if (null? lst) 0
     (+ (examine (car lst) f) (examine-l (cdr lst) f))))

(define (examine loc f)
  (if (eq? (list-ref (list-ref (cdr f) (car loc)) (cdr loc)) 'B) 1 0))

(define (available-loc loc-arr max)
  (cond [(and (zero? (car loc-arr)) (zero? (cdr loc-arr))) (list (cons 1 0) (cons 1 1) (cons 0 1))]
         [(and (zero? (car loc-arr)) (eq? (cdr loc-arr) max)) (list (cons 0 (- max 1)) (cons 1 (- max 1)) (cons 1 max))]
         [(and (eq? (car loc-arr) max) (zero? (cdr loc-arr))) (list (cons max 1) (cons (- max 1) 0) (cons (- max 1) 1))]
         [(and (eq? (car loc-arr) max) (eq? (cdr loc-arr) max)) (list (cons (- max 1) (- max 1)) (cons (- max 1) max) (cons max (- max 1)))]
         [(zero? (car loc-arr))
              (list (cons (car loc-arr) (- (cdr loc-arr) 1)) (cons (car loc-arr) (+ (cdr loc-arr) 1)) 
                                     (cons (+ (car loc-arr) 1) (- (cdr loc-arr) 1)) (cons (+ (car loc-arr) 1) (cdr loc-arr)) (cons (+ (car loc-arr) 1) (+ (cdr loc-arr) 1)))]
         [(zero? (cdr loc-arr)) (list (cons (car loc-arr) 1) (cons (- (car loc-arr) 1) 1) (cons (+ (car loc-arr) 1) 1) (cons (- (car loc-arr) 1) 0) (cons (+ (car loc-arr) 1) 0))]
         [(eq? (car loc-arr) max) (list (cons (- max 1) (cdr loc-arr)) (cons (- max 1) (- (cdr loc-arr) 1)) (cons (- max 1) (+ (cdr loc-arr) 1)) (cons max (- (cdr loc-arr) 1)) (cons max (+ (cdr loc-arr) 1)))]
         [(eq? (cdr loc-arr) max) (list (cons (- (car loc-arr) 1) max) (cons (+ (car loc-arr) 1) max) (cons (+ (car loc-arr) 1) (- max 1)) (cons (car loc-arr) (- max 1)) (cons (- (car loc-arr) 1) (- max 1)))]
         [else (list (cons (- (car loc-arr) 1) (cdr loc-arr)) (cons (- (car loc-arr) 1) (- (cdr loc-arr) 1)) (cons (- (car loc-arr) 1) (+ (cdr loc-arr) 1))
                       (cons (car loc-arr) (+ (cdr loc-arr) 1)) (cons (car loc-arr) (- (cdr loc-arr) 1)) 
                       (cons (+ (car loc-arr) 1) (cdr loc-arr)) (cons (+ (car loc-arr) 1) (- (cdr loc-arr) 1)) (cons (+ (car loc-arr) 1) (+ (cdr loc-arr) 1)))]))



(define (loc-trans-tree-to-arr loc-t)
  (let ([max (find-two-exp (length loc-t))])
    (define (trans loc-t loc-r mx)
      (cond  [(null? loc-t) loc-r]
             [(eq? (car loc-t) 0) (trans (cdr loc-t) (cons (+ (car loc-r) 0) (+ (cdr loc-r) 0)) (/ mx 2))]
             [(eq? (car loc-t) 1) (trans (cdr loc-t) (cons (+ (car loc-r) 0) (+ (cdr loc-r) (/ mx 2))) (/ mx 2))]
             [(eq? (car loc-t) 2) (trans (cdr loc-t) (cons (+ (car loc-r) (/ mx 2)) (+ (cdr loc-r) (/ mx 2))) (/ mx 2))]
             [(eq? (car loc-t) 3) (trans (cdr loc-t) (cons (+ (car loc-r) (/ mx 2)) (+ (cdr loc-r) 0)) (/ mx 2))]))
    (trans loc-t (cons 0 0) max)))

(define (find-two-exp k)
  (if (= k 0) 1 
     (* 2 (find-two-exp (- k 1)))))
    
  

; In the document, it is said to have type form -> void, but implement
; as form -> string.
; Read hw5-4-selfgrader.rkt for formatting.

(define (pprint-array f) ; pprint-array: form -> string
  (cond [(null? f) ""]
        [(eq? f 'B) "B\n"]
        [(eq? f 'W) "W\n"]
        [else (string-append (printlist (if (eq? (car f) 'array) (car (cdr f)) (car f)) "") "\n" (pprint-array (if (eq? (car f) 'array) (cdr (cdr f)) (cdr f))))]))

(define (printlist lst str)
   (cond [(eq? lst 'B) "B"]
         [(eq? lst 'W) "W"]
         [(null? lst) str]
         [else (if (eq? (car lst) 'B) (printlist (cdr lst) (string-append str "B")) (printlist (cdr lst) (string-append str "W")))]))

(define (is-array? f) ; is-array?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'array (car f)) #t]
        [else #f]))


;;; implementation with tree

(define (rotate-tree f) ; rotate-tree: form -> form
  (cond [(equal? 'B f) 'B]
         [(equal? 'W f) 'W]
         [(is-basic-tree? (cdr f)) (cons 'tree (list (list-ref (cdr f) 3) (list-ref (cdr f) 0) (list-ref (cdr f) 1) (list-ref (cdr f) 2)))]
         [else (glue-tree-from-tree (rotate-tree (cons 'tree (list-ref (cdr f) 3))) (rotate-tree (cons 'tree (list-ref (cdr f) 0)))
                                    (rotate-tree (cons 'tree (list-ref (cdr f) 1))) (rotate-tree (cons 'tree (list-ref (cdr f) 2))))]))

(define (neighbor-tree loc f) ; neighbor-tree: location * form -> int
  (neighbor-array loc (tree-to-array f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint-tree f) ; pprint-tree: form -> string
  (cond [(equal? f 'B) "B\n"]
        [(equal? f 'W) "W\n"]
        [else (pprint-array (tree-to-array f))]))

(define (is-tree? f) ; is-tree?: form -> bool
  (cond [(equal? 'B f) #t]
        [(equal? 'W f) #t]
        [(equal? 'tree (car f)) #t]
        [else #f]))

(define basic-array (cons 'array (list (list 'B 'B) (list 'W 'B))))

;;; conversions
(define (is-basic-array? f)
  (if (equal? (length f) 2) #t #f))
(define (basic-array-conversion f)
  (append (list-ref f 0) (reverse (list-ref f 1))))

(define (is-basic-tree? f)
  (if (list? (list-ref f 0)) #f #t))
(define (basic-tree-conversion f)
  (let ([half (list-half-divide f)]) (list (car half) (reverse (cdr half)))))

(define (array-quarter-divide f)
  (if (equal? (length f) 2) f
      (let ([half (list-half-divide f)])
        (list (map (lambda (lst) (car (list-half-divide lst))) (car half)) (map (lambda (lst) (cdr (list-half-divide lst))) (car half))
               (map (lambda (lst) (cdr (list-half-divide lst))) (cdr half)) (map (lambda (lst) (car (list-half-divide lst))) (cdr half))))))

(define (array-to-tree f) ; array-to-tree: form -> form
  (cond [(equal? 'B f) 'B]
        [(equal? 'W f) 'W]
        [(is-basic-array? (cdr f)) (cons 'tree (basic-array-conversion (cdr f)))]
        [else (let ([quat (array-quarter-divide (cdr f))])
               (glue-tree-from-tree (array-to-tree (cons 'array (list-ref quat 0))) (array-to-tree (cons 'array (list-ref quat 1)))
                                    (array-to-tree (cons 'array (list-ref quat 2))) (array-to-tree (cons 'array (list-ref quat 3)))))]))


(define (tree-to-array f) ; tree-to-array: form -> form
   (cond [(equal? 'B f) 'B]
         [(equal? 'W f) 'W]
         [(is-basic-tree? (cdr f)) (cons 'array (basic-tree-conversion (cdr f)))]
         [else (glue-array-from-array (tree-to-array (cons 'tree (list-ref (cdr f) 0))) (tree-to-array (cons 'tree (list-ref (cdr f) 1)))
                                      (tree-to-array (cons 'tree (list-ref (cdr f) 2))) (tree-to-array (cons 'tree (list-ref (cdr f) 3))))]))


(define (list-half-divide l)
  (let ([half (if (odd? (length l)) #f (quotient (length l) 2))])
    (cons (take l half) (list-tail l half))))


;;; interfaces

(define (glue nw ne se sw) ; glue: form * form * form * form -> form
  (cond
    [(not (is-tree? nw)) (glue (array-to-tree nw) ne se sw)]
    [(not (is-tree? ne)) (glue nw (array-to-tree ne) se sw)]
    [(not (is-tree? se)) (glue nw ne (array-to-tree se) sw)]
    [(not (is-tree? sw)) (glue nw ne se (array-to-tree sw))]
    [else (glue-tree-from-tree nw ne se sw)]))
  ; (let ([x (random 2)])
   ; (cond [(is-tree? nw) (if (= x 0) (glue-array-from-tree nw ne se sw) (glue-tree-from-tree nw ne se sw))]
    ;      [else (if (= x 0) (glue-tree-from-array nw ne se sw) (glue-array-from-array nw ne se sw))])))

(define (rotate f) ; rotate: form -> form
  (if (is-array? f)
      (rotate-array f)
      (rotate-tree f)))

(define (neighbor loc f) ; neighbor: location * form -> int
  (if (is-array? f)
      (neighbor-array loc f)
      (neighbor-tree loc f)))

; In the document, it is said to have type form -> void, but implement
; as form -> string.
(define (pprint f) ; pprint: form -> string
  (if (is-array? f)
      (pprint-array f)
      (pprint-tree f)))
