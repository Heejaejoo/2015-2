#lang racket

(require "hw5-1.rkt")
(require "hw5-2.rkt")

(provide is-empty? fst rest length nth-elmt map reduce)

(define (is-empty? #|tau list -> bool|# l #|tau list|#)
  (if (equal? l #|tau list|# empty #|tau list|#) #t #|bool|# #f #|bool|#)
  )

(define (fst #|tau list-> tau + unit|# l #|tau list|#) 
    (case-list #|(unit → τ + unit) ∗ (τ ∗ τ list → τ + unit) ∗ τ list → τ + unit|#
     (lambda (u) #|(unit → τ + unit)|# (inr #|unit → τ + unit|# '() #|unit|#) #|τ + unit|#)
     (lambda (h t) #|(τ ∗ τ list → τ + unit)|# (inl #|τ  → τ + unit|# h #|τ|#) #|τ + unit|#)
     l #|τ list|#) #|tau + unit|#)

(define (rest #|tau list-> tau list + unit|# l #|tau list|#)
    (case-list #|(unit → τ list + unit) ∗ (τ ∗ τ list → τ list + unit) ∗ τ list → τ list + unit|#
     (lambda (u) #|(unit → τ list + unit)|# (inr #|unit → τ list+ unit|# '() #|unit|#) #|τ list + unit|#)
     (lambda (h t) #|(τ ∗ τ list → τ list + unit)|# (inl #|τ list → τ list+ unit|# t #|tau list|#) #|τ list + unit|#)
     l #|τ list|#) #|tau list + unit|#)

(define (length #|τ list → int|# l #|τ list|#)
      (case-list #|(unit → int) ∗ (τ ∗ τ list → int) ∗ τ list → int|#
       (lambda (u) #|(unit → int)|# 0 #|int|#)
       (lambda (h t) #|(τ ∗ τ list → int)|# (+ 1 (length t) #|int|#) #|int|#)
       l #|τ list|#) #|int|#)

(define (nth-elmt #|τ list ∗ int → τ + unit|# l #|τ list|# i #|int|#)
  (define tst #|τ list ∗ int → τ + unit|#
    (lambda (lst cnt) #|τ list ∗ int → τ + unit|#
      (case-list #|(unit → τ + unit) ∗ (τ ∗ τ list → τ + unit) ∗ τ list → τ + unit|#
       (lambda (u) #|(unit → τ + unit)|# (inr #|unit → τ + unit|# '() #|unit|#) #|unit → τ + unit|#)
       (lambda (h t) #|(τ ∗ τ list → τ + unit)|# (if (equal? #|int*int -> bool|# cnt #|int|# i #|int|#) #|bool|# (inl #|τ → τ + unit|# h #|τ|#) #|τ + unit|# 
                                                    (tst #|τ list ∗ int → τ + unit|# t #|τ list|# (+ 1 cnt) #|int|#) #|τ + unit|#) #|τ + unit|#)
      lst #|tau list|#)))
    (tst l #|tau list|# 0 #|int|#) #|tau + unit|#)

(define (map #|(τ → σ) ∗ τ list → σ list|# f #|(τ → σ)|# l #|τ list|#)
      (case-list #|(unit → σ list) ∗ (τ ∗ τ list → σ list) ∗ τ list → σ list|#
       (lambda (u) #|(unit → σ list)|# empty #|σ list|#) #|σ list|#
       (lambda (h t) (link #|σ ∗ σ list → σ list|# (f #|τ → σ|# h #|τ|#) (map #|(τ → σ) ∗ τ list → σ list|# f #|(τ → σ)|# t #|τ list|#) #|σ list|#) #|σ list|#)
      l #|τ list|#) #|σ list|#)

(define (reduce #|τ list ∗ (τ ∗ σ → σ) ∗ σ → σ|# l #|τ list|# f #|(τ ∗ σ → σ)|# s #|σ|#)
      (case-list #|(unit → σ) ∗ (τ ∗ τ list → σ) ∗ τ list → σ|#
       (lambda (u) #|(unit → σ)|# s #|σ|#) #|σ|#
       (lambda (h t) #|(τ ∗ τ list → σ)|# (f #|(τ ∗ σ → σ)|# h #|τ|# (reduce #|τ list ∗ (τ ∗ σ → σ) ∗ σ → σ|# t #|τ list|# f #|(τ ∗ σ → σ)|# s #|σ|#) #|σ|#) #|σ|#) #|σ|#
      l #|τ list|#) #|σ|#)
