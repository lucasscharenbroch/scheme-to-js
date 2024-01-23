;#include std

(define (assert pred)
  (if pred
    '()
    (begin
      (print pred)
      (err "assertion failed (^)"))))

(define (assert= x y)
  (assert (equal? x y)))
