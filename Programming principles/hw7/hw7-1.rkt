#lang racket

(require "sum.rkt")
; Do not assume anything about 'sum'.
; Use the provided functions (inl, inr, case-sum) only.

(provide bstree-make bstree-add-elmt bstree-del-elmt bstree-find-elmt)

(define (node k v)
  (mcons (mcons (mcons k v) '()) '()))

(define (get-key node)
  (mcar (mcar (mcar node))))
(define (get-val node)
  (mcdr (mcar (mcar node))))
(define (set-key node key)
  (set-mcar! (mcar (mcar node)) key))
(define (set-val node val)
  (set-mcdr! (mcar (mcar node)) val)) 
(define (get-left-child n)
  (mcdr (mcar n)))
(define (get-right-child n)
  (mcdr n))
(define (set-left-child parent child)
  (set-mcdr! (mcar parent) child))
(define (set-right-child parent child)
  (set-mcdr! parent child))
(define (key-compare n1 key)
  (let ([x (get-key n1)] [y key])
    (cond
      [(> x y) -1]
      [(equal? x y) 0]
      [(< x y) 1])))

(define (find-most-left-child t)
  (if (null? (get-left-child t)) t (find-most-left-child (get-left-child t))))

(define (bstree-make)
  (node '() '()))

(define (bstree-add-elmt t k v)
  (cond
    [(null? (get-key t)) (begin (set-mcar! (mcar (mcar t)) k) (set-mcdr! (mcar (mcar t)) v) #f)]
    [(> (key-compare t k) 0) (if (null? (get-right-child t)) (begin (set-right-child t (node k v)) #f) (bstree-add-elmt (get-right-child t) k v))]
    [(< (key-compare t k) 0) (if (null? (get-left-child t)) (begin (set-left-child t (node k v)) #f) (bstree-add-elmt (get-left-child t) k v))]
    [(= (key-compare t k) 0) (begin (set-val t v) #t)]))
     
(define (bstree-del-elmt t k)
  (define (bstree-del-elmt-aux t k parent cnum)
    (cond
      [(null? t) #f]
      [(null? (get-key t)) #f]
      [(> (key-compare t k) 0) (bstree-del-elmt-aux (get-right-child t) k t 1)]
      [(< (key-compare t k) 0) (bstree-del-elmt-aux (get-left-child t) k t -1)]
      [(= (key-compare t k) 0) (cond
                                 [(and (null? (get-left-child t)) (null? (get-right-child t))) (if (> cnum 0) (begin (set-right-child parent '()) #t)
                                                                                                   (if (< cnum 0) (begin (set-left-child parent '()) #t)
                                                                                                       (begin (set-val t '()) (set-key t '()) #t)))]
                                 [(null? (get-left-child t)) (if (> cnum 0) (begin (set-right-child parent (get-right-child t)) #t)
                                                                 (if (< cnum 0) (begin (set-left-child parent (get-right-child t)) #t)
                                                                     (let ([p (get-right-child t)])
                                                                              (begin (set-val t (get-val p)) (set-key t (get-key p))
                                                                                     (set-left-child t (get-left-child p)) (set-right-child t (get-right-child p)) #t))))]
                                 [(null? (get-right-child t)) (if (> cnum 0) (begin (set-right-child parent (get-left-child t)) #t)
                                                                 (if (< cnum 0) (begin (set-left-child parent (get-left-child t)) #t)
                                                                     (let ([p (get-left-child t)])
                                                                              (begin (set-val t (get-val p)) (set-key t (get-key p))
                                                                                     (set-left-child t (get-left-child p)) (set-right-child t (get-right-child p)) #t))))]
                                 [else (letrec ([x (find-most-left-child (get-right-child t))] [y (get-key x)] [z (get-val x)])
                                         (begin (bstree-del-elmt-aux t y parent cnum) (set-key t y) (set-val t z) #t))])]))
  (bstree-del-elmt-aux t k '() 0))

(define (bstree-find-elmt t k)
  (cond [(null? t) (inr 'amumu)]
        [(null? (get-key t)) (inr 'amumu)]
        [(> (key-compare t k) 0) (bstree-find-elmt (get-right-child t) k)]
        [(< (key-compare t k) 0) (bstree-find-elmt (get-left-child t) k)]
        [(= (key-compare t k) 0) (inl (get-val t))]))