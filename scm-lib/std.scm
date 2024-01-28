(define (not b)
  (if b #f #t))

(define eqv? eq?) ; alias for eq?

(define (equal? x y) ; recursive eq? (pairs and vectors)
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

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

;;;;; pairs / lists

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define (list . elems) elems) ; take advantage of variadic list construction :)

(define (length l)
  (cond
    ((null? l) 0)
    ((not (pair? l)) (error "length: expected list"))
    (else (+ 1 (length (cdr l))))))

(define (append x y)
  (define (append-rec a b)
    (if (null? a)
        b
        (cons (car a) (append-rec (cdr a) b))))
  (if (not (and (list? x) (list? y)))
      (begin
        (error "append: expected lists")
        '())
      (append-rec x y)))

(define (reverse l)
  (define (reverse-iter l acc)
    (if (null? l)
        acc
        (reverse-iter (cdr l)
                      (cons (car l) acc))))
  (if (not (list? l))
      (error "reverse: expected list")
      (reverse-iter l '())))

; (same as haskell's "drop")
(define (list-tail l n)
  (if (<= n 0)
      l
      (cond
        ((null? l) (error "list-tail: list is not long enough"))
        ((not (pair? l)) ("list-tail: non-pair reached"))
        (else (list-tail (cdr l) (- n 1))))))

; (!!)
(define (list-ref l k)
  (let ((tail (list-tail l k)))
       (cond ((null? tail) (error "list-ref: error when computing tail"))
             ((not (pair? tail)) tail)
             (else (car tail)))))

(define (last-pair x)
  (if (pair? x)
      (if (pair? (cdr x))
          (last-pair (cdr x))
          x)
      x))

(define (memq obj l)
  (define (memq-rec l)
    (cond ((null? l) #f)
          ((eq? obj (car l)) l)
          (else (memq-rec (cdr l)))))
  (if (not (list? l))
      (error "memq: expected list")
      (memq-rec l)))

(define (assq obj alist)
  (define (assq-rec l)
    (cond ((null? l) #f)
          ((not (pair? (car l))) (error "assq: elements of given list should be pairs"))
          ((eq? obj (car (car l))) (car l))
          (else (assq-rec (cdr l)))))
  (if (not (list? alist))
      (error "assq: expected list")
      (assq-rec alist)))

(define (map f l)
  (define (map-rec f l)
    (if (null? l)
        '()
        (cons (f (car l))
              (map-rec f (cdr l)))))
  (cond ((not (procedure? f)) (error "map: expected procedure as first argument"))
        ((not (list? l)) (error "map: expected list as second argument"))
        (else (map-rec f l))))


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
(define (vector . elems)
  (vector->list elems))

; vector-length

; vector-ref

; vector-fill! (mutable)

;;;;; metacircular evaluator

;;; eval

; eval-lambda

; eval-define

; eval-cond

; eval-if

; eval-and

; eval-or

; evel-set!

; eval-let

; eval-let*

; eval-letrec

; eval-begin

; eval-list

; lookup

(define (eval-list l env)
  (if (null? l)
      '()
      (cons (eval (car l) env)
            (eval-list (cdr l) env))))

(define special-forms (list
  (cons 'lambda eval-lambda)
  (cons 'define eval-define)
  (cons 'cond eval-cond)
  (cons 'if eval-if)
  (cons 'and eval-and)
  (cons 'or eval-or)
  (cons 'set! eval-set!)
  (cons 'let eval-let)
  (cons 'let* eval-let*)
  (const 'letrec eval-letrec)
  (cons 'begin eval-begin)
))

(define (eval expr env)
  (cond ((or (number? expr)
             (boolean? expr)
             (string? expr)
             (vector? expr)
             (char? expr)
             (procedure? expr))
         x)
        ((symbol? expr) (lookup expr env))
        ((null? expr) (error "eval: can't evaluate '()"))
        ((list? expr) (let ((sf (assq (car expr) special-forms)))
                        (if sf
                            ((cdr sf) (cdr expr) env)
                            (apply (eval (car expr) env)
                                   (eval-list (cdr expr) env)))))
        (else (error "eval: can't evaluate")
              (error expr))))

;;; apply

(define (zip-args params args)
  (cond ((null? params) (if (null? args) '() (error "apply: too many arguments")))
        ((symbol? params) (list (cons params args)))
        ((null? args) (error "apply: too few argument"))
        (else (cons ((car params) (car args))
                    (zip-args (cdr params) (cdr args))))))

(define (bind params args env)
  (cons (zip-args params args) env))

(define (apply f args)
  (define (closure? x) (and (pair? x) (eq? (car x) 'closure)))
  (cond ((not (list? args)) (error "apply: expected list as second argument"))
        ((procedure? f) (apply-procedure f))
        ((closure? f) (eval (cdadr closure) (bind args (cddr closure)))) ; eval function body in new env
        (error "apply: expected procedure or closure as first argument")))

