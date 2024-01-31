;#include test/lib-test.scm

(assert= '(b+ 1 2) 3)
(assert= '(b/ 1 2) 0.5)

(define x-dyn 2)
(set!-dynamic 'x-dyn 3)
(assert= x-dyn 3)
