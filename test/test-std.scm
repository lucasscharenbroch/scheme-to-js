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

(assert= '(foldl b+ 0 '(1 2 3)) 6)

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

(assert= '(<= 1 2) #t)
(assert= '(<= 2 2) #t)
(assert= '(<= 3 2) #f)

(assert= '(>= 1 2) #f)
(assert= '(>= 2 2) #t)
(assert= '(>= 3 2) #t)

(assert= '(~= 0.00000002 0) #t)
(assert= '(~= 3.00000002 3) #t)
(assert= '(~= 3.2 3) #f)

(assert= '(zero? 0) #t)
(assert= '(zero? 0.1) #f)
(assert= '(zero? -0.1) #f)

(assert= '(positive? 1) #t)
(assert= '(positive? 0) #f)
(assert= '(positive? -1) #f)

(assert= '(negative? 1) #f)
(assert= '(negative? 0) #f)
(assert= '(negative? -1) #t)

(assert= '(odd? 3) #t)
(assert= '(odd? 3.3) #f)
(assert= '(odd? 2) #f)

(assert= '(even? 3) #f)
(assert= '(even? 3.3) #f)
(assert= '(even? 2) #t)

(assert= '(max -1 2 -3) 2)
(assert= '(min -1 2 -3) -3)

(assert= '(map abs '(1 -2 -3 3 4 -0.4)) ''(1 2 3 3 4 0.4))

(assert= '(map truncate '(1.2 4.99 -4.99 0 0.1)) ''(1 4 -4 0 0))

(assert= '(// 13 6) 2)
(assert= '(// -13 6) -2)
(assert= '(% 13 6) 1)
(assert= '(% -1 6) 5)
(assert= '(% -9 6) 3)

; chars

(assert= '(char-alphabetic? #\a) #t)
(assert= '(char-alphabetic? #\x) #t)
(assert= '(char-alphabetic? #\z) #t)
(assert= '(char-alphabetic? #\+) #f)

(assert= '(char-numeric? #\1) #t)
(assert= '(char-numeric? #\2) #t)
(assert= '(char-numeric? #\x) #f)
(assert= '(char-numeric? #\+) #f)

(assert= '(char-whitespace? #\space) #t)
(assert= '(char-whitespace? #\newline) #t)
(assert= '(char-whitespace? #\2) #f)
(assert= '(char-whitespace? #\+) #f)

(assert= '(list->string (map char-upcase (string->list "aBcDeF123")))
          "ABCDEF123")

(assert= '(list->string (map char-downcase (string->list "aBcDeF123")))
          "abcdef123")

; strings

; vectors

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

; misc / integration

(define (croot x)
  (define (improve y)
    (/ (+ (/ x (* y y))
          (* 2 y))
       3))
  (define (good-enough? y)
    (< (abs (- (* y y y)
               x))
       epsilon))
  (define (try y)
    (if (good-enough? y)
        y
        (try (improve y))))
  (try 1.0))


(assert= '(~= (croot (* 3 3 3)) 3) #t)
(assert= '(~= (croot (* 8 8 8)) 8) #t)
