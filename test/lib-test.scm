;#include std.scm

(define (assert pred)
  (if (eval pred '())
    '()
    (begin
      (error "assertion failed" pred))))

(define (assert= x y)
  (assert (list 'equal? x y)))
