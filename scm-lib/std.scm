;;;;; misc

(define (not b)
  (if b #f #t))

(define (all xs) ; non-variadic version of "and"
  (define (all-rec xs)
    (if (null? xs)
        #t
        (and (car xs) (all-rec (cdr xs)))))
  (if (not (list? xs))
      (error "all: expected list")
      (all-rec xs)))

(define (any xs) ; non-variadic version of "or"
  (define (any-rec xs)
    (if (null? xs)
        #f
        (or (car xs) (any-rec (cdr xs)))))
  (if (not (list? xs))
      (error "any: expected list")
      (any-rec xs)))

(define eqv? eq?) ; alias for eq?

(define (equal? x y) ; recursive eq? (pairs and vectors)
  (cond ((or (pair? x) (pair? y))
         (and (pair? y)
              (pair? x)
              (equal? (car x) (car y))
              (equal? (cdr x) (cdr y))))
        ((or (vector? x) (vector? y))
         (and (vector? x)
              (vector? y)
              (equal? (vector->list x) (vector->list y))))
        (else (eq? x y))))

;;;;; functional

(define (compose f g)
  (lambda (x) (f (g x))))

(define (zip x y)
  (define (zip-rec x y)
    (if (null? x)
        '()
        (cons (cons (car x) (car y)) (zip (cdr x) (cdr y)))))
  (cond ((or (not (list? x))
            (not (list? y)))
         (error "zip: expected lists"))
        ((not (= (length x) (length y))) (error "zip: mismatched lengths"))
        (else (zip-rec x y))))

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

(define (list? x) (or (null? x) (and (pair? x) (list? (cdr x)))))

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

(define (+ . l)
  (define (rec+ l)
    (if (null? l)
        0
        (b+ (car l) (rec+ (cdr l)))))
  (rec+ l))

(define (- . l)
  (define (rec- l)
    (cond ((null? l) (error "-: expected at least one argument"))
          ((null? (cdr l)) (b- 0 (car l))) ; negate
          ((null? (cddr l)) (b- (car l) (cadr l))) ; binary minus
          (else (rec- (cons (b- (car l) (cadr l)) ; n-ary minus
                            (cddr l))))))
  (rec- l))

(define (* . l)
  (define (rec* l)
    (if (null? l)
        1
        (b* (car l) (rec* (cdr l)))))
  (rec* l))

(define (/ . l)
  (define (rec/ l)
    (cond ((null? l) (error "/: expected at least one argument"))
          ((null? (cdr l)) (b/ 1 (car l))) ; inverse
          ((null? (cddr l)) (b/ (car l) (cadr l))) ; binary divide
          (else (rec- (cons (b/ (car l) (cadr l)) ; n-ary divide
                            (cddr l))))))
  (rec/ l))

(define (<= x y) (or (= x y) (< x y)))

(define (>= x y) (or (= x y) (> x y)))

(define (zero? x) (= x 0))

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

(define (eval-lambda args env)
  (if (not (= 2 (length args)))
      (error "lambda: expected two arguments")
      (cons 'closure (cons args env))))

(define (eval-cond args env)
  (cond ((null? args) (error "non-exhaustive conditional"))
        ((not (pair? (car args))) (error "cond: expected pairs"))
        ((or (eq? (caar args) 'else) (eval (caar args) env)) (eval (cadar args) env))
        (else (eval-cond (cdr args) env))))

(define (eval-if args env)
  (cond ((not (= 3 (length args))) (error "if: expected 3 arguments"))
        (else (let ((pred   (lambda () (eval (car args) env)))
                    (conseq (lambda () (eval (cadr args) env)))
                    (alt    (lambda () (eval (caddr args) env))))
                   (if (pred) (conseq) (alt))))))

(define (eval-and xs env)
  (if (null? xs)
      #t
      (and (eval (car xs) env) (eval-and (cdr xs) env))))

(define (eval-or xs env)
  (if (null? xs)
      #f
      (or (eval (car xs) env) (eval-or (cdr xs) env))))

(define (eval-set! args env)
  (define (eval-set-rec id val env)
    (cond ((null? env) (set!-dynamic id val))
          (else (let ((binding (assq id (car env))))
                     (if binding
                         (set-cdr! binding val)
                         (eval-set-rec id val (cdr env)))))))
  (cond ((not (= 2 (length args))) (error "set!: expected two arguments"))
        ((not (symbol? (car args))) (error "set!: expected symbol as first argument"))
        (else (eval-set-rec (car args) (eval (cadr args) env) env))))

(define (valid-binding? x) (and (list? x)
                               (= 2 (length x))
                               (symbol? (car x))))

(define (eval-let args env)
  (cond ((< (length args) 1) (error "let: expected more than one argument"))
        ((not (all (map valid-binding? (car args)))) (error "let: invalid bindings"))
        (else (let* ((bindings (car args))
                     (names (map car bindings))
                     (vals (map cadr bindings))
                     (eval-in-env (lambda (x) (eval x env)))
                     (bound-layer (zip names (map eval-in-env vals)))
                     (env2 (cons bound-layer env)))
                    (eval-begin (cdr args) env2)))))

(define (eval-let* args env)
  (define (eval-let*-rec bindings body) ; reduce bindings recursive normal let expressions
    (if (null? bindings)
      (cons 'begin body)
      (list 'let (list (car bindings)) (eval-let*-rec (cdr bindings) body))))
  (cond ((< (length args) 1) (error "let*: expected more than one argument"))
        (else (eval (eval-let*-rec (car args) (cdr args)) env))))

(define (eval-letrec args env)
  (define (defaultify-snd x)
    (if (not (pair? x))
        (error "letrec: expected bindings to be pairs")
        (list (car x) "default-arg")))
  (define (cascade-sets bindings body)
    (if (null? bindings)
        body
        (cons (list 'set! (caar bindings) (cadar bindings))
              (cascade-sets (cdr bindings) body))))
  (cond ((< (length args) 1) (error "letrec: expected more than one argument"))
        (else (let ((args2 (map defaultify-snd (car args))) ;; TODO remove "*" in let*
                    (body2 (cascade-sets (car args) (cdr args))))
                   (eval-let* (cons args2 body2)
                              env)))))

(define (eval-begin args env)
  (cond ((null? args) '())
        ((null? (cdr args)) (eval (car args) env))
        (else (eval (car args) env) (eval-begin (cdr args) env))))

(define (eval-list l env)
  (if (null? l)
      '()
      (cons (eval (car l) env)
            (eval-list (cdr l) env))))

(define (eval-quote l env)
  (if (not (= 1 (length l)))
    (error "quote: expected exactly one argument")
    (car l)))

; lookup
(define (lookup sym env)
  (if (null? env)
      (js-eval-symbol sym)
      (let ((pair (assq sym (car env))))
           (if pair (cdr pair)
                    (lookup sym (cdr env))))))

(define special-forms (list
  (cons 'lambda eval-lambda)
  (cons 'cond eval-cond)
  (cons 'if eval-if)
  (cons 'and eval-and)
  (cons 'or eval-or)
  (cons 'set! eval-set!)
  (cons 'let eval-let)
  (cons 'let* eval-let*)
  (cons 'letrec eval-letrec)
  (cons 'begin eval-begin)
  (cons 'quote eval-quote)
))

(define (eval expr env)
  (cond ((or (null? expr)
             (number? expr)
             (boolean? expr)
             (string? expr)
             (vector? expr)
             (char? expr)
             (procedure? expr))
         expr)
        ((symbol? expr) (lookup expr env))
        ((list? expr) (let ((sf (assq (car expr) special-forms)))
                        (if sf
                            ((cdr sf) (cdr expr) env)
                            (apply (eval (car expr) env)
                                   (eval-list (cdr expr) env)))))
        (else (error "eval: can't evaluate" expr)
              (error expr))))

;;; apply

(define (zip-args params args)
  (cond ((null? params) (if (null? args) '() (error "apply: too many arguments")))
        ((symbol? params) (list (cons params args)))
        ((null? args) (error "apply: too few argument"))
        (else (cons (cons (car params) (car args))
                    (zip-args (cdr params) (cdr args))))))

(define (bind params args env)
  (cons (zip-args params args) env))

(define (apply f args)
  (define (closure? x) (and (pair? x) (eq? (car x) 'closure)))
  (cond ((not (list? args)) (error "apply: expected list as second argument"))
        ((procedure? f) (apply-procedure f args))
        ((closure? f) (eval (cadadr f) (bind (caadr f) args (cddr f)))) ; eval function body in new env
        (else (error "apply: expected procedure or closure as first argument"))))
