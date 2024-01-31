;#include test/lib-test.scm

; misc

(assert= '(not 2) #f)
(assert= '(not ()) #t)

(assert= '(all '(1 2 3)) #t)
(assert= '(all '(1 () 3)) '())
(assert= '(any '(1 2 3)) 1)
(assert= '(any '(#f ())) #f)

; functional

(assert= '(zip '(1 2 3) '(a b c)) ''((1 . a) (2 . b) (3 . c)))

; lists

(assert= '(caar (cons (cons 1 2) 4)) 1)
(assert= '(cadadr (cons 3 (cons (cons 1 (cons 99 98)) 8))) 99)

(assert= '(list 1 2 3) '(cons 1 (cons 2 (cons 3 '()))))

(assert= '(length (list 1 9 3 4))  4)
(assert= '(length '())  0)

(assert= '(list? '(1 2 . 3)) #f)
(assert= '(list? '(1 2 3)) #t)

(assert= '(list 1 2 3 4 5 6) '(append (list 1 2 3 4) (list 5 6)))
(assert= '(list 1 2 3 4 5 6) '(append (list 1 2 3 4 5 6) '()))

(assert= '(reverse (list 1 2 3)) '(list 3 2 1))

(assert= '(list-tail (list 1 2 3 4 5) 2) '(list 3 4 5))

(assert= '(list-ref (list 9 8 7 6 5) 4) 5)
(assert= '(list-ref (list 9 8 7 6 5 4 3) 4) 5)

(assert= '(last-pair (list 9 8 7 6 5)) '(cons 5 ()))

(assert= '(memq 'a '(a b c)) ''(a b c))
(assert= '(memq 3 (list 1 2 3 4)) '(list 3 4))
(assert= '(memq 3 '(a b c)) #f)

(assert= '(assq 'a '((a 1) (b 2) (c 3))) ''(a 1))
(assert= '(assq 'b '((a 1) (b 2) (c 3))) ''(b 2))
(assert= '(assq 'd '((a 1) (b 2) (c 3))) #f)

(define (inc x) (+ 1 x))
(assert= '(map inc (list 1 2 3)) '(list 2 3 4))

; numeric

(assert= '(+ 1 2 3) 6)
(assert= '(+) 0)

(assert= '(- 1) -1)
(assert= '(- 1 3) -2)
(assert= '(- 1 3 5) -7)

; eval

(assert= '(eval '(and () 1 2) '()) '())
(assert= '(eval '(and () 1 2 #f) '()) '())
(assert= '(eval '(and "hi" 1 2 #f) '()) #f)
(assert= '(eval '(and "hi" 1 2) '()) #t)

(assert= '(eval '(or #f 2 () 3 2 9 5 "hi" #f) '()) 2)
(assert= '(eval '(or #\c 2 () 3 2 9 5 "hi" #f) '()) #\c)
(assert= '(eval '(or () () #f () #f ()) '()) #f)
(assert= '(eval '(or () () #f () #t #f ()) '()) #t)

(assert= '(eval '(if #t 1 0) '()) 1)
(assert= '(eval '(if #t 0 1) '()) 0)
(assert= '(eval '(if #f 0 1) '()) 1)
(assert= '(eval '(if #f 1 0) '()) 0)

(assert= '(eval '(cond (#t 9) (#f 4)) '()) 9)
(assert= '(eval '(cond (#f 9) (#t 4)) '()) 4)
(assert= '(eval '(cond (#f 9) (else 4)) '()) 4)
(assert= '(eval '(cond (else 9) (else 4)) '()) 9)

(assert= '(eval '(lambda (x) (+ x 1)) '()) ''(closure ((x) (+ x 1))))

(define xyz 2)
(define e '(((xyz . 100))))
(eval '(set! xyz 50) e)
(assert= 'xyz 2)
(assert= 'e ''(((xyz . 50))))

(assert= '(eval '(begin 1 (set! xyz 45) 2) e) 2)
(assert= 'e ''(((xyz . 45))))

(assert= '(eval '(let ((x 1) (y 2)) (+ x y)) '()) 3)
(assert= '(eval '(let ((x (+ 0 1)) (y (+ 1 1))) (+ x y)) '()) 3)

(assert= '(eval '(let* ((x 1) (y (+ x 1))) 123 (+ x y)) '()) 3)

(assert= '(eval '(letrec ((even? (lambda (n) (if (zero? n)
                                                 #t
                                                 (odd? (- n 1)))))
                          (odd? (lambda (n) (if (zero? n)
                                                #f
                                                (even? (- n 1))))))
                         (even? 88)) '()) #t)
