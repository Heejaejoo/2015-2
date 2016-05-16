open Syntax
open Proj

let e1_str = "(if #t (car (cons 1 2)) 4)"
let e1_val = myeval e1_str
let e1_val_str = value_to_string e1_val

let _ = print_endline e1_val_str

let _ = if (e1_val_str = "1") then
    print_endline "O"
  else
    print_endline "X"

let chk id exp_str result =
    let _ = print_string (id ^ " ") in
    
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (value_to_string (myeval exp_str))) in
    
    let _ = print_endline (try (if result = (value_to_string (myeval exp_str)) then "O" else "X") with NOT_IMPLEMENTED -> "Not Implemented") in
    ()
    
let errchk id exp_str =
    let _ = print_string (id ^ " ") in
    
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (try value_to_string (myeval exp_str) with RUNTIME_EXCEPTION s -> s)) in
    
    let result = try value_to_string (myeval exp_str) with RUNTIME_EXCEPTION _ -> "O" | NOT_IMPLEMENTED -> "Not Implemented" in
    let _ = print_endline (if result = "O" then "O" else "X") in
    ()

let exnchk id exp_str =
    let _ = print_string (id ^ " ") in
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (try value_to_string (myeval exp_str) with RUNTIME_EXCEPTION s -> s | UNCAUGHT_EXCEPTION -> "uncaught")) in
    let result = try value_to_string (myeval exp_str) with UNCAUGHT_EXCEPTION -> "O" | NOT_IMPLEMENTED -> "Not Implemented" in 
    let _ = print_endline (if result = "O" then "O" else "x") in ()

let memchk id exp_str result =
    let _ = print_string (id ^ " ") in
    
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (value_to_string (myeval_memo exp_str))) in
    
    let _ = print_endline (try (if result = (value_to_string (myeval_memo exp_str)) then "O" else "X") with NOT_IMPLEMENTED -> "Not Implemented") in
    ()
     
let memerrchk id exp_str =
    let _ = print_string (id ^ " ") in
    
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (try value_to_string (myeval_memo exp_str) with RUNTIME_EXCEPTION s -> s)) in
    
    let result = try value_to_string (myeval_memo exp_str) with RUNTIME_EXCEPTION _ -> "O" | NOT_IMPLEMENTED -> "Not Implemented" in
    let _ = print_endline (if result = "O" then "O" else "X") in
    ()

let memexnchk id exp_str =
    let _ = print_string (id ^ " ") in
    let _ = print_endline "" in
    let _ = print_endline ("Original : " ^ exp_str) in
    let lexbuf = Lexing.from_string exp_str in
    let lexer () = Lexer.token lexbuf in
    let exp = Parser.parse lexer in
    let _ = print_endline ("Parsed   : " ^ (exp_to_string exp)) in
    let _ = print_endline ("Result   : " ^ (try value_to_string (myeval_memo exp_str) with RUNTIME_EXCEPTION s -> s | UNCAUGHT_EXCEPTION -> "uncaught")) in
    let result = try value_to_string (myeval_memo exp_str) with UNCAUGHT_EXCEPTION -> "O" | NOT_IMPLEMENTED -> "Not Implemented" in 
    let _ = print_endline (if result = "O" then "O" else "x") in ()




let _ = print_endline "1) Basic Expression"
let _ = chk "1-1" "1234" "1234"
let _ = chk "1-2" "#t" "#t"
let _ = chk "1-3" "'()" "'()"
let _ = chk "1-4" "(if #t 8 17)" "8"
let _ = chk "1-5" "(if #f 8 17)" "17"
let _ = chk "1-6" "(cons 8 17)" "(cons 8 17)"
let _ = chk "1-7" "(car (cons 12 25))" "12"
let _ = chk "1-8" "(cdr (cons 12 25))" "25"
let _ = chk "1-9" "(+ (* 3 4) (- 9 2))" "19"
let _ = chk "1-10" "(= (if (< 8 17) 23 4) (if (> 11 11) 7 23))" "#t"

let _ = print_endline "2) Environment"
let _ = chk "2-1" "(let ((a 10) (b 20)) a)" "10"
let _ = chk "2-2" "(letrec ((a 10) (b a)) b)" "10"
let _ = errchk "2-3" "(let ((a 10) (b a)) b)"
let _ = errchk "2-4" "(letrec ((b a) (a 10)) b)"
let _ = errchk "2-5" "(let ((a 10) (a 5)) a)"
let _ = errchk "2-6" "(letrec ((a 10) (a 5)) a)"
let _ = chk "2-7" "(let ((x 1)) (+ (let ((x 2)) x) x))" "3"
let _ = chk "2-8" "(letrec ((x 1) (y (+ x 1))) (let ((a y)) (letrec ((x a) (y (+ x 2))) y)))" "4"
let _ = errchk "2-9" "(letrec ((x 1) (y (+ x 1))) (letrec ((x y) (y (+ x 2))) y))"
let _ = chk "2-10" "(letrec ((x 1) (y (+ x 1))) (let ((x y) (y (+ x 2))) y))" "3"

let _ = print_endline "3) Function & Application"
let _ = chk "3-1" "((lambda (x) (+ x 3)) 5)" "8"
let _ = chk "3-2" "((lambda (a b) (* a b)) 6 7)" "42"
let _ = chk "3-3" "((lambda () 1818))" "1818"
let _ = errchk "3-4" "((lambda (x) 1818))"
let _ = errchk "3-5" "((lambda (x x) x) 1 2)"
let _ = chk "3-6" "(letrec ((fib (lambda (n) (if (= n 0) 1 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 10))" "89"
let _ = chk "3-7" "(letrec ((f (lambda (x) (if (> x 0) (g (- x 1)) 0))) (g (lambda (x) (if (> x 0) (+ (f (- x 1)) 1) 0)))) (f 100))" "50"
let _ = chk "3-8" "(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)))))) (f 100 0))" "5050"
(*let _ = chk "3-9" "(letrec ((f (lambda (l result) (if (= l '()) result (f (cdr l) (cons (car l) result)))))) (f (cons 1 (cons 2 (cons 3 '()))) '()))" "(cons 3 (cons 2 (cons 1 '())))"*)
let _ = chk "3-10" "(letrec ((is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1))))) (is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1)))))) (is-odd 11))" "#t"
let _ = chk "3-11" "((lambda (c) (((lambda (f) (lambda (x) (f x))) (lambda (x) (* c (+ x 1)))) 2)) 2)" "6"
let _ = chk "3-12" "(let ((x 2) (f (lambda (x) (+ x 3)))) (let ((x 5) (g (lambda (x) (f x)))) (g x)))" "8"


let _ = print_endline "4) Tail Recursion (might take some time, which is normal)"
let _ = chk "4-1" "(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)))))) (f 999999 0))" "499999500000"
let _ = chk "4-2" "(letrec ((is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1))))) (is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1)))))) (is-odd 999999))" "#t"

let _ = print_endline "5) Memory"
let _ = chk "5-1" "(mcar (mcons 1 2))" "1"
let _ = chk "5-2" "(letrec ((p (mcons 1 2)) (tmp (set-mcar! p 3))) (mcar p))" "3"
let _ = chk "5-3" "(let ((mp (mcons 1 2))) (let ((tmp (set-mcdr! mp 3))) (mcdr mp)))" "3"
let _ = chk "5-4" "(let ((p (mcons (mcons 1 2) (mcons 3 4)))) (let ((tmp1 (set-mcar! (mcar p) 5)) (tmp2 (set-mcdr! p (cons 6 7)))) (* (mcar (mcar p)) (car (mcdr p)))))" "30"
let _ = chk "5-5" "(let ((p (mcons 1 1))) (letrec ((f (lambda (n) (if (= n 0) (mcdr p) (letrec ((sum (+ (mcar p) (mcdr p))) (tmp1 (set-mcar! p (mcdr p))) (tmp2 (set-mcdr! p sum))) (f (- n 1))))))) (f 15)))" "1597"
let _ = errchk "5-6" "(car (mcons 1 2))"
let _ = errchk "5-7" "(mcar (cons 1 2))"
let _ = chk "5-8" "(let ((p (mcons 3 4))) (let ((p (mcar p))) p))" "3"
let _ = errchk "5-9" "(let ((p (mcons 3 4))) (letrec ((p (mcar p))) p))"
let _ = chk "5-10" "(let ((p (mcons 3 4))) (set-mcar! p 5))" "#<void>"

let _ = print_endline "6) Exception Handling"
let _ = exnchk "6-1" "(raise 1)"
let _ = chk "6-2" "(with-handlers (((lambda (x) (= x 5)) (lambda (x) (* x 2)))) (cons (+ 1 3) (- 2 (raise 5))))" "10"
let _ = chk "6-3" "(with-handlers (((lambda (x) (= x 1)) (lambda (x) (* x 3)))) (with-handlers (((lambda (x) (= x 2)) (lambda (x) (* x 3)))) (raise 1)))" "3"

let _ = chk "6-4" "(with-handlers (((lambda (x) (= x 3)) (lambda (x) (* x 3)))) (with-handlers (((lambda (x) (raise (+ x 2))) (lambda (x) (* x 3)))) (raise 1)))" "9"
let _ = chk "6-5" "(let ((p (mcons 4 5))) (let ((tmp (with-handlers (((lambda (n) (> n 0)) (lambda (n) (let ((tmp (set-mcar! p (+ (mcar p) n)))) n)))) (letrec ((f (lambda (n) (if (= n 1) (raise 1) (raise (f (- n 1))))))) (f 10))))) (mcar p)))" "5"
let _ = chk "6-6" "(with-handlers (((lambda (x) (x 1 2)) (lambda (x) (x 10 5)))) (raise (lambda (a b) (< a b))))" "#f"
let _ = errchk "6-7" "(with-handlers (((lambda () #t) (lambda (x) 1))) (raise 3))"
let _ = exnchk "6-8" "(with-handlers (((lambda (x) (raise 1)) (lambda (x) 1))) (raise 3))"
let _ = errchk "6-9" "(with-handlers (((lambda (a) #t) (lambda (a b) 1))) (raise 3))"
let _ = chk "6-10" "(with-handlers (((lambda (x) #t) (lambda (x) 10))) 3)" "3"
let _ = chk "6-11" "(with-handlers (((lambda (x) (car x)) (lambda (x) 10)) ((lambda () 1) (lambda (a b) '()))) (raise (cons 1 2)))" "10"

let _ = print_endline "7) Memoize" 
let _ = memchk "7-1" "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))) ))) )) (fib 50))" "12586269025"
let _ = memchk "7-2" "(let ((x 1)) (let ((f (lambda () x))) (let ((y (f))) (+ y (f)))))" "2"
let _ = memchk "7-3" "(letrec ((ways (lambda (n m) (if (= n 0) 1 (if (= m 0) 1 (+ (ways (- n 1) m) (ways n (- m 1)))))))) (ways 30 30))" "118264581564861424"
let _ = memchk "7-4" "(letrec ((fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2))) ))) )) (fib 80))" "23416728348467685"

let _ = print_endline "myeval_memo test from the beginning"
let _ = print_endline "8) Mem_Basic Expression"
let _ = memchk "8-1" "1234" "1234"
let _ = memchk "8-2" "#t" "#t"
let _ = memchk "8-3" "'()" "'()"
let _ = memchk "8-4" "(if #t 8 17)" "8"
let _ = memchk "8-5" "(if #f 8 17)" "17"
let _ = memchk "8-6" "(cons 8 17)" "(cons 8 17)"
let _ = memchk "8-7" "(car (cons 12 25))" "12"
let _ = memchk "8-8" "(cdr (cons 12 25))" "25"
let _ = memchk "8-9" "(+ (* 3 4) (- 9 2))" "19"
let _ = memchk "8-10" "(= (if (< 8 17) 23 4) (if (> 11 11) 7 23))" "#t"

let _ = print_endline "9) Mem_Environment"
let _ = memchk "9-1" "(let ((a 10) (b 20)) a)" "10"
let _ = memchk "9-2" "(letrec ((a 10) (b a)) b)" "10"
let _ = memerrchk "9-3" "(let ((a 10) (b a)) b)"
let _ = memerrchk "9-4" "(letrec ((b a) (a 10)) b)"
let _ = memerrchk "9-5" "(let ((a 10) (a 5)) a)"
let _ = memerrchk "9-6" "(letrec ((a 10) (a 5)) a)"
let _ = memchk "9-7" "(let ((x 1)) (+ (let ((x 2)) x) x))" "3"
let _ = memchk "9-8" "(letrec ((x 1) (y (+ x 1))) (let ((a y)) (letrec ((x a) (y (+ x 2))) y)))" "4"
let _ = memerrchk "9-9" "(letrec ((x 1) (y (+ x 1))) (letrec ((x y) (y (+ x 2))) y))"
let _ = memchk "9-10" "(letrec ((x 1) (y (+ x 1))) (let ((x y) (y (+ x 2))) y))" "3"

let _ = print_endline "10) Mem_Function & Application"
let _ = memchk "10-1" "((lambda (x) (+ x 3)) 5)" "8"
let _ = memchk "10-2" "((lambda (a b) (* a b)) 6 7)" "42"
let _ = memchk "10-3" "((lambda () 1818))" "1818"
let _ = memerrchk "10-4" "((lambda (x) 1818))"
let _ = memerrchk "10-5" "((lambda (x x) x) 1 2)"
let _ = memchk "10-6" "(letrec ((fib (lambda (n) (if (= n 0) 1 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))) (fib 10))" "89"
let _ = memchk "10-7" "(letrec ((f (lambda (x) (if (> x 0) (g (- x 1)) 0))) (g (lambda (x) (if (> x 0) (+ (f (- x 1)) 1) 0)))) (f 100))" "50"
let _ = memchk "10-8" "(letrec ((f (lambda (x n) (if (= x 0) n (f (- x 1) (+ n x)))))) (f 100 0))" "5050"
(*let _ = chk "3-9" "(letrec ((f (lambda (l result) (if (= l '()) result (f (cdr l) (cons (car l) result)))))) (f (cons 1 (cons 2 (cons 3 '()))) '()))" "(cons 3 (cons 2 (cons 1 '())))"*)
let _ = memchk "10-10" "(letrec ((is-even (lambda (n) (if (= n 0) #t (is-odd (- n 1))))) (is-odd (lambda (n) (if (= n 0) #f (is-even (- n 1)))))) (is-odd 11))" "#t"
let _ = memchk "10-11" "((lambda (c) (((lambda (f) (lambda (x) (f x))) (lambda (x) (* c (+ x 1)))) 2)) 2)" "6"
let _ = memchk "10-12" "(let ((x 2) (f (lambda (x) (+ x 3)))) (let ((x 5) (g (lambda (x) (f x)))) (g x)))" "8"

let _ = print_endline "11) Mem_Memory"
let _ = memchk "11-1" "(mcar (mcons 1 2))" "1"
let _ = memchk "11-2" "(letrec ((p (mcons 1 2)) (tmp (set-mcar! p 3))) (mcar p))" "3"
let _ = memchk "11-3" "(let ((mp (mcons 1 2))) (let ((tmp (set-mcdr! mp 3))) (mcdr mp)))" "3"
let _ = memchk "11-4" "(let ((p (mcons (mcons 1 2) (mcons 3 4)))) (let ((tmp1 (set-mcar! (mcar p) 5)) (tmp2 (set-mcdr! p (cons 6 7)))) (* (mcar (mcar p)) (car (mcdr p)))))" "30"
let _ = memchk "11-5" "(let ((p (mcons 1 1))) (letrec ((f (lambda (n) (if (= n 0) (mcdr p) (letrec ((sum (+ (mcar p) (mcdr p))) (tmp1 (set-mcar! p (mcdr p))) (tmp2 (set-mcdr! p sum))) (f (- n 1))))))) (f 15)))" "1597"
let _ = memerrchk "11-6" "(car (mcons 1 2))"
let _ = memerrchk "11-7" "(mcar (cons 1 2))"
let _ = memchk "11-8" "(let ((p (mcons 3 4))) (let ((p (mcar p))) p))" "3"
let _ = memerrchk "11-9" "(let ((p (mcons 3 4))) (letrec ((p (mcar p))) p))"
let _ = memchk "11-10" "(let ((p (mcons 3 4))) (set-mcar! p 5))" "#<void>"

let _ = print_endline "12) Exception Handling"
let _ = memexnchk "12-1" "(raise 1)"
let _ = memchk "12-2" "(with-handlers (((lambda (x) (= x 5)) (lambda (x) (* x 2)))) (cons (+ 1 3) (- 2 (raise 5))))" "10"
let _ = memchk "12-3" "(with-handlers (((lambda (x) (= x 1)) (lambda (x) (* x 3)))) (with-handlers (((lambda (x) (= x 2)) (lambda (x) (* x 3)))) (raise 1)))" "3"

let _ = memchk "12-4" "(with-handlers (((lambda (x) (= x 3)) (lambda (x) (* x 3)))) (with-handlers (((lambda (x) (raise (+ x 2))) (lambda (x) (* x 3)))) (raise 1)))" "9"
let _ = memchk "12-5" "(let ((p (mcons 4 5))) (let ((tmp (with-handlers (((lambda (n) (> n 0)) (lambda (n) (let ((tmp (set-mcar! p (+ (mcar p) n)))) n)))) (letrec ((f (lambda (n) (if (= n 1) (raise 1) (raise (f (- n 1))))))) (f 10))))) (mcar p)))" "5"
let _ = memchk "12-6" "(with-handlers (((lambda (x) (x 1 2)) (lambda (x) (x 10 5)))) (raise (lambda (a b) (< a b))))" "#f"
let _ = memerrchk "12-7" "(with-handlers (((lambda () #t) (lambda (x) 1))) (raise 3))"
let _ = memexnchk "12-8" "(with-handlers (((lambda (x) (raise 1)) (lambda (x) 1))) (raise 3))"
let _ = memerrchk "12-9" "(with-handlers (((lambda (a) #t) (lambda (a b) 1))) (raise 3))"
let _ = memchk "12-10" "(with-handlers (((lambda (x) #t) (lambda (x) 10))) 3)" "3"
let _ = memchk "12-11" "(with-handlers (((lambda (x) (car x)) (lambda (x) 10)) ((lambda () 1) (lambda (a b) '()))) (raise (cons 1 2)))" "10"


