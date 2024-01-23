(define (not b)
  (if b #f #t))

; eqv? ; alias for eq?
(define eqv? eq?)

; equal? ; recursive eq? (pairs and vectors)
(define (equal? x y)
  (cond ((or (pair? x) (pair? y))
         (and (pair? y)
              (pair? x)
              (eq? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((or (vector? x) (vector? y))
         (and (vector? x)
              (vector? y)
              (equal? (vector->list x) (vector->list y))))
        (else (eq? x y))))

; list?

;;;;; pairs / lists

; caar
; cadr
; aa
; ad
; da
; dd
; aaa
; aad
; ada
; add
; daa
; dad
; dda
; ddd
; aaaa
; aaad
; aada
; aadd
; adaa
; adad
; adda
; addd
; daaa
; daad
; dada
; dadd
; ddaa
; ddad
; ddda
; dddd

(define (list . elems) elems) ; take advantage of variadic list construction :)

(define (length l)
  (cond
    ((null? l) 0)
    ((not (pair? l)) (error "length: expected list"))
    (else (+ 1 (length (cdr list))))))

; append

; reverse

; list-tail (same as haskell's "tail")

; list-ref (!!)

; last-pair
(define (last-pair x)
  (if (pair? x)
      (if (pair? (cdr x))
          (last-pair (cdr x))
          x)
      x))

; memq

; memv

; member

; assq

; assv

; assoc



;;;;; numeric

; <=
; >=

; zero?

; positive?

; negative?

; odd?

; even?

; max

; min

; abs

; quotient

; remainder

; modulo

; truncate

; round

; number->string

; string->number



;;;;; chars

; char=?
; char<?
; char>?
; char<=?
; char>=?

; char-alphabetic?
; char-numeric?
; char-whitespace?

; char-upcase
; char-downcase

;;;;; strings

; make-string (k spaces, or k of the given char)

; string-length

; string-ref

; string=?
; string<?
; string>?
; string<=?
; string>=?

; substring (string, start, end)

; string-append (variadic)

; string-copy (immutable)

; string-fill! (mutable)

;;;;; vector

; make-vector (k * 0, or given)

; vector (like list)
(define (vector. elems)
  (vector->list elems))

; vector-length

; vector-ref

; vector-fill! (mutable)

;;;;; control

; map (variadic)
