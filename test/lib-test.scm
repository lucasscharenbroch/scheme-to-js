;#include std.scm

(define (assert pred)
  (if pred
    '()
    (begin
      (error "assertion failed"))))

(define (assert= x y)
  (assert (equal? x y)))
