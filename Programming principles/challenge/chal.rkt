#lang racket

(provide myeval)
(require compatibility/mlist)

; Read the instruction of document carefully,
; and then write code according to it.

; This `myeval` function should mimic the behavior of `eval`,
; which is a built-in function of Racket.

; You should raise exceptions for irregular situations.
; The exception message doesn't have to be the same as `eval`,
; but the message should contain useful information.

(define (has-has-key? h key)
  (if (massoc key h) #t #f))

(define (has-set h key val)
  (if (has-has-key? h key)
      (mmap (lambda (x) (if (equal? (mcar x) key) (mlist key val) x)) h)
      (mappend (mlist (mlist key val)) h)))

(define (has-set-for-letrec h key val)
  (if (has-has-key? h key)
      (begin (set-mcdr! (massoc key h) (mcons val '())) h)
      (mappend (mlist (mlist key val)) h)))

(define (has-ref h key)
  (if (has-has-key? h key)
      (mcar (mcdr (massoc key h))) (raise "no such key")))
(define has (mlist))

(define (expr-check e)
  (cond
    [(equal? e ''()) (cons 0 '())]
    [(or (boolean? e) (integer? e)) (cons 0 e)]
    [(not (list? e)) (cons 1 e)]
    [(equal? (car e) 'if) (if (equal? (length e) 4) (cons 2 (cdr e))
                             (raise "if must have 3 expr"))]
    [(equal? (car e) 'cons) (if (equal? (length e) 3) (cons 3 (cdr e))
                             (raise "cons must have 2 expr"))]
    [(equal? (car e) 'car) (if (equal? (length e) 2) (cons 4 (cdr e))
                             (raise "car must have 1 expr"))]
    [(equal? (car e) 'cdr) (if (equal? (length e) 2) (cons 5 (cdr e))
                             (raise "cdr must have 1 expr"))]
    [(equal? (car e) 'lambda) (if (equal? (length e) 3) (cons 6 (cdr e))
                             (raise "lambda must have 2 expr"))]
    [(equal? (car e) 'let) (if (equal? (length e) 3) (cons 8 (cdr e))
                             (raise "let must have 2 expr"))]
    [(equal? (car e) 'letrec) (if (equal? (length e) 3) (cons 9 (cdr e))
                             (raise "letrec must have 2 expr"))]
    [(equal? (car e) '+) (if (equal? (length e) 3) (cons 10 (cdr e))
                             (raise "add must have 2 expr"))]
    [(equal? (car e) '-) (if (equal? (length e) 3) (cons 11 (cdr e))
                             (raise "sub must have 2 expr"))]
    [(equal? (car e) '*) (if (equal? (length e) 3) (cons 12 (cdr e))
                             (raise "mul must have 2 expr"))]
    [(equal? (car e) '=) (if (equal? (length e) 3) (cons 13 (cdr e))
                             (raise "equal must have 2 expr"))]
    [(equal? (car e) '<) (if (equal? (length e) 3) (cons 14 (cdr e))
                             (raise "< must have 2 expr"))]
    [(equal? (car e) '>) (if (equal? (length e) 3) (cons 15 (cdr e))
                             (raise "> must have 2 expr"))]
    [else (if (list? e) (cons 7 e) (raise "apl must have more than 1 expr"))]))

(define (duplicate-identifier-check lst)
  (if (null? lst) #t
      (if (member (caar lst) (filter-map (lambda (x) (car x)) (cdr lst)))
         #f
          (duplicate-identifier-check (cdr lst)))))

(define (duplicate-lambda lst)
  (if (null? lst) #f
     (if (member (car lst) (cdr lst)) #t (duplicate-lambda (cdr lst)))))

(define (hash-update-4let h lst outside-env)
  (if (null? lst) h
      (hash-update-4let (has-set h (caar lst) (myeval-aux (cadar lst) outside-env)) (cdr lst) outside-env)))

(define (lst-dummy-plus lst)
       (map (lambda (x) (list (car x) 'dummy)) lst)) 

(define (dummy-hash h lst)
  (if (null? lst) h
      (dummy-hash (has-set h (caar lst) (cadar lst)) (cdr lst))))

(define (updat h lst)
    (if (null? lst) h
        (updat (has-set-for-letrec h (caar lst) (myeval-aux (cadar lst) h)) (cdr lst))))

(define (hash-update-4letrec h lst)
  (let ((x (dummy-hash h (lst-dummy-plus lst))))
    (updat x lst)))
(define (sym-check lst)
  (andmap (lambda (x) (symbol? (car x))) lst))
(define (sym-check1 lst)
  (andmap (lambda (x) (symbol? x)) lst))

(define (myeval-aux e env)
       (let ((e1 (expr-check e)))
         (let ((id (car e1)) (expr (cdr e1)))
           (case id
             [(0) expr]
             [(1) (if (has-has-key? env expr) (let ((val (has-ref env expr))) (if (equal? val 'dummy) (raise "undefined identifier") val)) (raise "binding needed"))]
             [(2) (let ((x (myeval-aux (list-ref expr 0) env))) (if (false? x) (myeval-aux (list-ref expr 2) env)
                                                             (myeval-aux (list-ref expr 1) env)))]
             [(3) (cons (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(4) (let ((x (myeval-aux (car expr) env))) (if (pair? x) (car x)
                                                         (raise "car must be applied to a pair")))]
             [(5) (let ((x (myeval-aux (car expr) env))) (if (pair? x) (cdr x)
                                                         (raise "cdr must be applied to a pair")))]
             [(6) (if (sym-check1 (list-ref expr 0)) (if (duplicate-lambda (list-ref expr 0)) (raise "lambda: duplicate argument name")
                      (cons 'lambda (cons (cons env (list-ref expr 1)) (list-ref expr 0)))) (raise "lambda: not an identifier, identifier with default, or keyword"))] ;lambda has no value
             [(7) (let ((lam (myeval-aux (car expr) env)))
                    (if (pair? lam) 
                    (let ((laam (cdr lam)) (err (car lam)))
                      (if (equal? err 'lambda) 
                          (let ((restoredenv (caar laam)) (expression (cdar laam))
                                                   (var (cdr expr)) (asign (cdr laam)))
                            (myeval-aux expression (hash-update-4let restoredenv (if (equal? (length asign) (length var)) (map (lambda (x y) (list x y)) asign var)
                                                                                     (raise "arity mismatch")) env)))
                          (raise "application not a procedure"))) (raise "application not a procedure")))]
           
             [(8) (myeval-aux (cadr expr) (if (sym-check (car expr)) (if (duplicate-identifier-check (car expr)) (hash-update-4let env (car expr) env)
                                              (raise "duplicate identifier")) (raise "var is not symbol")))]
             
             [(9) (myeval-aux (cadr expr) (if (sym-check (car expr))
                                              (if (duplicate-identifier-check (car expr)) (hash-update-4letrec env (car expr)) (raise "duplicate identifier"))
                                              (raise "var is not symbol")))]
             
             [(10) (+ (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(11) (- (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(12) (* (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(13) (= (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(14) (< (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]
             [(15) (> (myeval-aux (list-ref expr 0) env) (myeval-aux (list-ref expr 1) env))]))))
    
(define (myeval e)    
  (let ((environment has))
    (myeval-aux e environment)))
