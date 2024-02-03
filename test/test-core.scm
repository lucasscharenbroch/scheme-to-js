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

(assert= '(eq? 1 1) #t)
(assert= '(eq? 'a 'a) #t)
(assert= '(eq? 1 'a) #f)
(assert= '(eq? "a" 'a) #f)
(assert= '(eq? '(1 2 3) '(1 2 3)) #f) ; not recursive
(assert= '(eq? '#(1 2 3) '#(1 2 3)) #f) ; not recursive

(define abc 123)
(assert= '(js-eval-symbol 'abc) 123)
(set!-dynamic 'abc 999)
(assert= abc 999)

(assert= '(apply-procedure + '(1 2 3)) 6)

(assert= '(boolean? #t) #t)
(assert= '(boolean? '()) #f)

(assert= '(pair? '(1 2 3)) #t)
(assert= '(pair? (cons 1 2)) #t)
(assert= '(pair? '()) #f)

(assert= '(null? '()) #t)
(assert= '(null? #f) #f)

(assert= '(symbol? 'a) #t)
(assert= '(symbol? '#t) #f)
(assert= '(symbol? '()) #f)

(assert= '(number? '1) #t)
(assert= '(number? '()) #f)

(assert= '(string? "a") #t)
(assert= '(string? #\a) #f)
(assert= '(string? '()) #f)

(assert= '(char? #\a) #t)
(assert= '(char? "a") #f)
(assert= '(char? '()) #f)

(assert= '(vector? '#(a b c)) #t)
(assert= '(vector? ''#(a b c)) #f)
(assert= '(vector? '(a b c)) #f)
(assert= '(vector? '()) #f)

(assert= '(procedure? +) #t)
(assert= '(procedure? '+) #f)
(assert= '(procedure? '()) #f)

(assert= '(symbol->string 'abc) "abc")
(assert= '(string->symbol "x+*/") ''x+*/)

(assert= '(char->integer #\c) 99)
(assert= '(integer->char (char->integer #\c)) #\c)

(assert= '(string->list "abc") ''(#\a #\b #\c))
(assert= '(list->string (string->list "abc")) "abc")

(assert= '(list->vector '(1 2 3 a b c)) '#(1 2 3 a b c))
(assert= '(list->vector '(1 2 3)) '#(1 2 3))
(assert= '(vector->list '#(1 2 3 a b c)) ''(1 2 3 a b c))

(assert= (string->number "-123.456") -123.456)
(assert= (number->string -123.456) "-123.456")

(assert= '(cons 1 2) ''(1 . 2))
(assert= '(car '(1 . 2)) 1)
(assert= '(cdr '(1 . 2)) 2)

(define qrs '(1 . 2))
(set-car! qrs 'new-car)
(assert= 'qrs ''(new-car . 2))
(set-cdr! qrs 'new-cdr)
(assert= 'qrs ''(new-car . new-cdr))

(assert= '(b+ 1 2) 3)
(assert= '(b- 5 10) -5)
(assert= '(b* 2 3.5) 7)
(assert= '(b/ 1 2) 0.5)

(assert= '(= 3 2) #f)
(assert= '(= 3 3) #t)
(assert= '(< 3 2) #f)
(assert= '(< 2 3) #t)
(assert= '(< 3 3) #f)
(assert= '(> 3 2) #t)
(assert= '(> 2 3) #f)
(assert= '(> 3 3) #f)

(assert= '(floor 1.99) 1)
(assert= '(ceiling 1.99) 2)

(assert= '(exp 0) 1)
(assert= '(log 1) 0)
(assert= '(sin 0) 0)
(assert= '(cos 0) 1)
(assert= '(tan 0) 0)
(assert= '(asin 0) 0)
(assert '(let ((x (acos 0))) (> 0.001 (abs (- x 1.5708)))))
(assert= '(atan 0) 0)
(assert= '(sqrt 4) 2)
(assert '(let ((x (atan2 2 3))) (> 0.001 (abs (- x 0.588)))))
(assert= '(expt 3 4) 81)

(define test-vec  '#(1 2 3 4 5))
(vector-set! test-vec 2 100)
(assert= 'test-vec '#(1 2 100 4 5))

; tail recursion

(define (to-50k x)
    (if (< 50000 x)
        x
        (to-50k (+ 1 x))))

(to-50k 0)
