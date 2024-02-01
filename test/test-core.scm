;#include test/lib-test.scm

; primitives / special forms

(assert= (if 2 3 4) 3)
(assert= (if '() 3 4) 4)
(assert= (if #f 3 4) 4)
(assert= (if 'a 3 4) 3)
(assert= (if '#() 3 4) 3)

(assert= ''(a b c) (quote (quote (a b c))))

(assert= ((lambda (x y z) (* (+ x y) z)) 1 2 3) (* (+ 1 2) 3))
(assert= ((lambda x (apply + x)) 1 2 3) 6)
(assert= ((lambda (divisor . terms) (/ (apply + terms) divisor)) 2 1 2 3) 3)

(define var 99)
(define (var+ x) (set! var (+ x var)))

(assert= var 99)
(var+ 1)
(assert= var 100)
(set! var 2)
(assert= var 2)

(assert= (begin (set! var 10) 8 9) 9)
(assert= var 10)

(assert= (cond (1 2) (3 4)) 2)
(assert= (cond ('() 2) (3 4)) 4)
(assert= (cond ('() 2) (else 3)) 3)

(assert= (and 1 2 3) 3)
(assert= (and 1 '() 3) '())
(assert= (or 1 2 3) 1)
(assert= (or #f '()) '())

(assert= (let ((a 1) (b 2)) (+ a b)) 3)

(assert= (let* ((a 1) (b (+ a 1))) (+ a b)) 3)

(assert= (letrec ((even? (lambda (n) (if (zero? n)
                                         #t
                                         (odd? (- n 1)))))
                  (odd? (lambda (n) (if (zero? n)
                                        #f
                                        (even? (- n 1))))))
                 (even? 87)) #f)

; core functions

(assert= '(b+ 1 2) 3)
(assert= '(b/ 1 2) 0.5)
(assert= (char->integer #\c) 99)
(assert= (integer->char (char->integer #\c)) #\c)

(assert= '(string->list "abc") ''(#\a #\b #\c))
(assert= '(list->string (string->list "abc")) "abc")

(assert= '(list->vector '(1 2 3)) '#(1 2 3))

(assert= '(b+ 1 2) 3)
(assert= '(b/ 1 2) 0.5)

(define x-dyn 2)
(set!-dynamic 'x-dyn 3)
(assert= 'x-dyn 3)
