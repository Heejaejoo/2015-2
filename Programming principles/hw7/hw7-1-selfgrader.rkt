#lang racket

(require "common-grade.rkt")
(require "hw7-1.rkt")
(require "sum.rkt")

(println "t1")
(define t1 (bstree-make))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t1 3 "v1")) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t1 4)) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t1 5 "v3")) ))

(sgoutput (lambda () (equal? #t (bstree-del-elmt t1 3)) ))

(sgoutput (lambda () (equal? "v3" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t1 5)
                                            ))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t1 3)
                                                 ))))

(println "t2")
(define t2 (bstree-make))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 35 "v1")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 18 "v2")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 68 "v3")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 90 "v4")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 7 "v5")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 3 "v6")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 12 "v7")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 26 "v8")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 22 "v9")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 30 "v10")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 21 "v100")) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t2 18)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t2 21)) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t2 99 "v11")) ))
(sgoutput (lambda () (equal? "v1" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 35)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 18)))))
(sgoutput (lambda () (equal? "v3" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 68)))))
(sgoutput (lambda () (equal? "v4" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 90)))))
(sgoutput (lambda () (equal? "v5" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 7)))))
(sgoutput (lambda () (equal? "v6" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 3)))))
(sgoutput (lambda () (equal? "v7" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 12)))))
(sgoutput (lambda () (equal? "v8" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 26)))))
(sgoutput (lambda () (equal? "v9" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 22)))))
(sgoutput (lambda () (equal? "v10" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 30)))))
(sgoutput (lambda () (equal? "v11" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t2 99)))))

(sgoutput (lambda () (equal? #t (bstree-del-elmt t2 68)) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t2 18)) ))
(sgoutput (lambda () (equal? "v1" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 35)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 18)))))
(sgoutput (lambda () (equal? "v5" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 7)))))
(sgoutput (lambda () (equal? "v6" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 3)))))
(sgoutput (lambda () (equal? "v7" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 12)))))
(sgoutput (lambda () (equal? "v8" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 26)))))
(sgoutput (lambda () (equal? "v9" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 22)))))
(sgoutput (lambda () (equal? "v10" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t2 30)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 68)))))
(sgoutput (lambda () (equal? "v11" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t2 99)))))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t2 30)) ))
(sgoutput (lambda () (equal? "v1" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 35)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 18)))))
(sgoutput (lambda () (equal? "v5" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 7)))))
(sgoutput (lambda () (equal? "v6" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 3)))))
(sgoutput (lambda () (equal? "v7" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 12)))))
(sgoutput (lambda () (equal? "v8" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 26)))))
(sgoutput (lambda () (equal? "v9" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 22)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 30)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 68)))))
(sgoutput (lambda () (equal? "v11" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t2 99)))))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t2 22)) ))
(sgoutput (lambda () (equal? "v1" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 35)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 18)))))
(sgoutput (lambda () (equal? "v5" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 7)))))
(sgoutput (lambda () (equal? "v6" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 3)))))
(sgoutput (lambda () (equal? "v7" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 12)))))
(sgoutput (lambda () (equal? "v8" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 26)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t2 22)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 30)))))
(sgoutput (lambda () (equal? "nothing" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t2 68)))))
(sgoutput (lambda () (equal? "v11" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t2 99)))))


(println "t3")
(define t3 (bstree-make))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 22 "v9")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 7 "v5")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 35 "v1")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 30 "v10")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 68 "v3")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 90 "v4")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 99 "v11")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 18 "v2")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 12 "v7")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 26 "v8")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 3 "v6")) ))

(sgoutput (lambda () (equal? "v1" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 35)))))
(sgoutput (lambda () (equal? "v2" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t3 18)))))
(sgoutput (lambda () (equal? "v3" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t3 68)))))
(sgoutput (lambda () (equal? "v4" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t3 90)))))
(sgoutput (lambda () (equal? "v5" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 7)))))
(sgoutput (lambda () (equal? "v6" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 3)))))
(sgoutput (lambda () (equal? "v7" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 12)))))
(sgoutput (lambda () (equal? "v8" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 26)))))
(sgoutput (lambda () (equal? "v9" (case-sum (lambda (x) x)
                                            (lambda (u) "nothing")
                                            (bstree-find-elmt t3 22)))))
(sgoutput (lambda () (equal? "v10" (case-sum (lambda (x) x)
                                                 (lambda (u) "nothing")
                                                 (bstree-find-elmt t3 30)))))
(sgoutput (lambda () (equal? "v11" (case-sum (lambda (x) x)
                                             (lambda (u) "nothing")
                                             (bstree-find-elmt t3 99)))))

(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 30)) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t3 30)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 35)) )) 
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 18)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 7)) ))  
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 3)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 12)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 26)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 22)) )) 
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 68)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 99)) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t3 35)) ))

(println "t4")
(define t4 (bstree-make))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 1 "a")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 2 "b")) ))
(sgoutput (lambda () (equal? #f (bstree-add-elmt t3 3 "c")) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 1)) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t3 1)) ))
(sgoutput (lambda () (equal? #f (bstree-del-elmt t3 1)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 2)) ))
(sgoutput (lambda () (equal? #t (bstree-del-elmt t3 3)) ))
