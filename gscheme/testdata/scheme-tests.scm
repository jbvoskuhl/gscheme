;; GScheme test suite — SRFI 64

(test-begin "gscheme")

(test-group "when"
  (test-equal "when true" 3 (when #t 1 2 3))
  (test-assert "when false is void" (eq? (when #f 1 2 3) (if #f #f)))
  ;; TCO test
  (define (when-loop n) (when (> n 0) (when-loop (- n 1))))
  (when-loop 100000)
  (test-assert "when TCO" #t))

(test-group "unless"
  (test-eqv "unless true" #f (unless #t 1 2 3))
  (test-equal "unless false" 3 (unless #f 1 2 3))
  ;; TCO test
  (define (unless-loop n) (unless (= n 0) (unless-loop (- n 1))))
  (unless-loop 100000)
  (test-assert "unless TCO" #t))

(test-group "if"
  (test-equal "if consequent tail" 42 (if #t (begin 42) 0))
  (test-equal "if alternate tail" 42 (if #f 0 (begin 42)))
  ;; TCO consequent
  (define (if-loop-c n) (if (= n 0) 'done (if-loop-c (- n 1))))
  (test-eq "if TCO consequent" 'done (if-loop-c 100000))
  ;; TCO alternate
  (define (if-loop-a n) (if (> n 0) (if-loop-a (- n 1)) 'done))
  (test-eq "if TCO alternate" 'done (if-loop-a 100000)))

(test-group "begin"
  (test-equal "begin returns last" 3 (begin 1 2 3))
  (test-assert "begin empty is void" (eq? (begin) (if #f #f)))
  (test-equal "begin single" 42 (begin 42))
  ;; TCO
  (define (begin-loop n) (begin (if (= n 0) 'done (begin-loop (- n 1)))))
  (test-eq "begin TCO" 'done (begin-loop 100000)))

(test-group "cond"
  (test-equal "cond first" 42 (cond (#t 42)))
  (test-equal "cond second" 2 (cond (#f 1) (#t 2)))
  (test-equal "cond else tail" 99 (cond (#f 1) (else (begin 99))))
  (test-eqv "cond no match" #f (cond (#f 1) (#f 2)))
  (test-equal "cond no body" 42 (cond (42)))
  (test-equal "cond arrow" 11 (cond (1 => (lambda (x) (+ x 10)))))
  ;; TCO
  (define (cond-loop n) (cond ((= n 0) 'done) (else (cond-loop (- n 1)))))
  (test-eq "cond TCO" 'done (cond-loop 100000)))

(test-group "case"
  (test-eq "case match first" 'one (case 1 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case match second" 'two (case 2 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case else" 'other (case 3 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case multiple values" 'low (case 2 ((1 2 3) 'low) ((4 5 6) 'high)))
  ;; TCO
  (define (case-loop n)
    (case (= n 0) ((#t) 'done) (else (case-loop (- n 1)))))
  (test-eq "case TCO" 'done (case-loop 100000))
  (test-eq "case +1+1" 'two (case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three)))
  (test-eq "case else 99" 'other (case 99 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case multi" 'low (case 2 ((1 2 3) 'low) ((4 5 6) 'high)))
  (test-eqv "case no match" #f (case 99 ((1) 'one) ((2) 'two))))

(test-group "do"
  (test-equal "do basic" 5 (do ((i 0 (+ i 1))) ((= i 5) i)))
  (test-equal "do multiple bindings" 10
    (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum)))
  (test-equal "do with body" 30
    (let ((x 0))
      (do ((i 0 (+ i 1))) ((= i 3) x) (set! x (+ x 10)))))
  ;; TCO
  (test-eq "do TCO" 'done (do ((i 0 (+ i 1))) ((= i 100000) 'done)))
  (test-equal "do count to 3" 3 (do ((n 0 (+ n 1))) ((= n 3) n)))
  (test-equal "do sum 1 to 5" 15
    (do ((n 1 (+ n 1)) (sum 0 (+ sum n))) ((> n 5) sum)))
  (test-equal "do build list" '(2 1 0)
    (do ((n 0 (+ n 1)) (lst '() (cons n lst))) ((= n 3) lst))))

(test-group "let/let*/letrec"
  (test-equal "let basic" 3 (let ((x 1) (y 2)) (+ x y)))
  (test-equal "let body tail" 10 (let ((x 10)) (begin 1 2 x)))
  ;; let TCO
  (define (let-loop n) (let ((m (- n 1))) (if (= m 0) 'done (let-loop m))))
  (test-eq "let TCO" 'done (let-loop 100000))
  ;; named let
  (test-equal "named let" 10
    (let loop ((i 0) (sum 0))
      (if (= i 5) sum (loop (+ i 1) (+ sum i)))))
  ;; named let TCO
  (test-eq "named let TCO" 'done
    (let loop ((n 100000)) (if (= n 0) 'done (loop (- n 1)))))
  ;; let*
  (test-equal "let*" 2 (let* ((x 1) (y (+ x 1))) y))
  ;; let* TCO
  (define (letstar-loop n) (let* ((m (- n 1))) (if (= m 0) 'done (letstar-loop m))))
  (test-eq "let* TCO" 'done (letstar-loop 100000))
  ;; letrec
  (test-eqv "letrec mutual recursion" #t
    (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
             (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
      (even? 10)))
  ;; letrec TCO
  (test-eq "letrec TCO" 'done
    (letrec ((loop (lambda (n) (if (= n 0) 'done (loop (- n 1))))))
      (loop 100000)))
  ;; letrec*
  (test-equal "letrec*" 2 (letrec* ((x 1) (y (+ x 1))) y)))

(test-group "and"
  (test-eqv "and no args" #t (and))
  (test-equal "and single" 42 (and 42))
  (test-equal "and last truthy" 3 (and 1 2 3))
  (test-eqv "and short circuits" #f (and 1 #f 3))
  (test-equal "and tail position" 42 (and #t #t (begin 42)))
  ;; TCO
  (define (and-loop n) (and (> n 0) (and-loop (- n 1))))
  (test-eqv "and TCO" #f (and-loop 100000)))

(test-group "or"
  (test-eqv "or no args" #f (or))
  (test-equal "or single" 42 (or 42))
  (test-equal "or first truthy" 1 (or #f 1 2))
  (test-eqv "or all false" #f (or #f #f #f))
  (test-equal "or tail position" 42 (or #f #f (begin 42)))
  ;; TCO
  (define (or-loop n) (or (= n 0) (or-loop (- n 1))))
  (test-eqv "or TCO" #t (or-loop 100000)))

(test-group "error-objects"
  (test-eqv "error-object? true"  #t (guard (e (#t (error-object? e))) (error "test")))
  (test-eqv "error-object? false" #f (error-object? 42))
  (test-eqv "error-object? raised non-error" #f
    (guard (e (#t (error-object? e))) (raise 42)))
  (test-equal "error-object-message" "\"hello\""
    (guard (e (#t (error-object-message e))) (error "hello" 1 2)))
  (test-equal "error-object-irritants" '(1 2 3)
    (guard (e (#t (error-object-irritants e))) (error "msg" 1 2 3)))
  (test-assert "error-object-irritants empty"
    (null? (guard (e (#t (error-object-irritants e))) (error "msg")))))

(test-group "guard"
  (test-equal "guard catch raise" 42 (guard (e (#t e)) (raise 42)))
  (test-equal "guard no error" 99 (guard (e (#t 'caught)) 99))
  (test-eq "guard else" 'caught (guard (e (else 'caught)) (error "test")))
  (test-eq "guard multiple clauses" 'string
    (guard (e ((number? e) 'number)
              ((string? e) 'string)
              (else 'other))
      (raise "hello")))
  ;; guard tail position
  (define (guard-loop n)
    (guard (e (#t e))
      (if (= n 0) (raise 'done) (guard-loop (- n 1)))))
  (test-eq "guard tail position" 'done (guard-loop 100000)))

(test-group "quasiquote"
  (test-equal "basic quasiquote" '(1 2 3) `(1 2 3))
  (define x 42)
  (test-equal "quasiquote unquote" '(a 42 c) `(a ,x c))
  (define xs '(1 2 3))
  (test-equal "quasiquote splicing" '(a 1 2 3 b) `(a ,@xs b))
  (test-equal "quasiquote expression" '(a 3 c) `(a ,(+ 1 2) c))
  (test-equal "quasiquote constants" '(a b c) `(a b c))
  (test-eq "quasiquote symbol" 'x `x)
  (test-equal "quasiquote number" 42 `42))

(test-group "when/unless extras"
  ;; when with side effects
  (define x 0)
  (when (> 5 3) (set! x 10))
  (test-equal "when side effect" 10 x)
  ;; unless with side effects
  (set! x 0)
  (unless (< 5 3) (set! x 10))
  (test-equal "unless side effect" 10 x))

(test-group "delay/force"
  (test-equal "basic delay/force" 3 (force (delay (+ 1 2))))
  ;; memoization
  (define count 0)
  (define p (delay (begin (set! count (+ count 1)) count)))
  (force p)
  (force p)
  (test-equal "promise memoization" 1 count)
  ;; promise is a procedure
  (test-eqv "promise is procedure" #t (procedure? (delay 42))))

(test-group "map/for-each/apply"
  (test-equal "map multi-list" '(11 22 33) (map + '(1 2 3) '(10 20 30)))
  (test-equal "map shortest" '(11 22) (map + '(1 2) '(10 20 30)))
  (test-equal "for-each multi-list" 33
    (let ((sum 0))
      (for-each (lambda (x y) (set! sum (+ sum x y))) '(1 2) '(10 20))
      sum))
  (test-equal "apply variadic" 10 (apply + 1 2 '(3 4)))
  (test-equal "apply list" 6 (apply + '(1 2 3))))

(test-group "eval"
  (test-equal "eval interaction-environment" 3
    (eval '(+ 1 2) (interaction-environment))))

(test-group "syntax-error"
  (test-eqv "syntax-error is error" #t
    (guard (e (#t (error-object? e))) (syntax-error "bad syntax"))))

(test-group "macro-expand"
  (test-equal "macro-expand non-macro" '(+ 1 2) (macro-expand '(+ 1 2))))

(test-group "time-call"
  (test-equal "time-call result" 3 (car (time-call (lambda () (+ 1 2)))))
  (test-equal "time-call with count" 42 (car (time-call (lambda () 42) 10))))

(test-group "procedure-display"
  (define (written obj)
    (define p (open-output-string))
    (write obj p)
    (get-output-string p))

  (define (my-add x y) (+ x y))
  (test-equal "named closure" "#<procedure my-add>" (written my-add))

  (define my-sub (lambda (x y) (- x y)))
  (test-equal "named closure via define" "#<procedure my-sub>" (written my-sub))

  (test-equal "anonymous closure" "#<procedure>" (written (lambda (x) x)))

  (define (apply-and-write f) (written f))
  (test-equal "anonymous closure as arg" "#<procedure>" (apply-and-write (lambda (x) x)))

  (test-equal "primitive car" "#<primitive car>" (written car))
  (test-equal "primitive +" "#<primitive +>" (written +))
  (test-equal "primitive string-append" "#<primitive string-append>" (written string-append))

  (test-equal "primitive map" "#<primitive map>" (written map))
  (test-equal "primitive call/cc" "#<primitive call/cc>" (written call/cc))
  (test-equal "primitive eval" "#<primitive eval>" (written eval))
  (test-equal "primitive apply" "#<primitive apply>" (written apply))

  (define my-swap
    (macro (a b)
      (list 'let (list (list 'tmp a))
            (list 'set! a b)
            (list 'set! b 'tmp))))
  (test-equal "named macro" "#<macro my-swap>" (written my-swap))

  (test-equal "built-in macro when" "#<macro when>" (written when))
  (test-equal "built-in macro unless" "#<macro unless>" (written unless))

  (test-equal "continuation display" "#<continuation>"
    (call/cc (lambda (k) (written k))))

  (test-equal "input-port display" "#<input-port>"
    (written (current-input-port)))
  (test-equal "output-port display" "#<output-port>"
    (written (current-output-port)))
  (test-equal "string output-port display" "#<output-port>"
    (written (open-output-string)))

  (test-equal "environment display" "#<environment>"
    (written (interaction-environment)))

  (test-assert "procedure? on closure" (procedure? my-add))
  (test-assert "procedure? on primitive" (procedure? car))
  (test-assert "procedure? on lambda" (procedure? (lambda () 1))))

(test-group "exact?"
  (test-eqv "integer is exact" #t (exact? 42))
  (test-eqv "bigint is exact" #t (exact? (expt 2 100)))
  (test-eqv "rational is exact" #t (exact? 3/4))
  (test-eqv "float is inexact" #f (exact? 42.0))
  (test-eqv "complex is inexact" #f (exact? 3+4i))
  (test-eqv "symbol is not exact" #f (exact? 'x)))

(test-group "inexact?"
  (test-eqv "float is inexact" #t (inexact? 42.0))
  (test-eqv "complex is inexact" #t (inexact? 3+4i))
  (test-eqv "integer is not inexact" #f (inexact? 42))
  (test-eqv "bigint is not inexact" #f (inexact? (expt 2 100)))
  (test-eqv "rational is not inexact" #f (inexact? 3/4))
  (test-eqv "symbol is not inexact" #f (inexact? 'x)))

(test-group "exact-integer?"
  (test-eqv "integer" #t (exact-integer? 42))
  (test-eqv "bigint" #t (exact-integer? (expt 2 100)))
  (test-eqv "integer rational" #t (exact-integer? 6/3))
  (test-eqv "non-integer rational" #f (exact-integer? 3/4))
  (test-eqv "float" #f (exact-integer? 42.0))
  (test-eqv "complex" #f (exact-integer? 3+4i)))

(test-group "number?"
  (test-eqv "float is number" #t (number? 42))
  (test-eqv "symbol is not number" #f (number? 'x)))

(test-group "list?"
  (test-eqv "proper list" #t (list? '(1 2 3)))
  (test-eqv "dotted pair" #f (list? (cons 1 2)))
  (test-eqv "empty list" #t (list? '()))
  (test-eqv "number" #f (list? 5))
  ;; circular list
  (define cycle (list 0 1 2 3))
  (set-cdr! (cdddr cycle) cycle)
  (test-eqv "circular list" #f (list? cycle)))

(test-group "complex?"
  (test-eqv "real is complex" #t (complex? 42))
  (test-eqv "float is complex" #t (complex? 3.14))
  (test-eqv "complex is complex" #t (complex? 3+4i))
  (test-eqv "symbol is not complex" #f (complex? 'x))
  (test-eqv "string is not complex" #f (complex? "hello")))

(test-group "real?"
  (test-eqv "real is real" #t (real? 42))
  (test-eqv "complex with zero imag is real" #t (real? 3+0i))
  (test-eqv "complex with nonzero imag is not real" #f (real? 3+4i)))

(test-group "with-exception-handler"
  (test-equal "handler called on raise" 42
    (with-exception-handler
      (lambda (e) e)
      (lambda () (raise 42))))
  (test-eqv "handler called on error" #t
    (with-exception-handler
      (lambda (e) (error-object? e))
      (lambda () (error "boom"))))
  (test-equal "normal return" 99
    (with-exception-handler
      (lambda (e) 'should-not-reach)
      (lambda () 99)))
  (test-equal "nested handlers" 'outer
    (with-exception-handler
      (lambda (e) 'outer)
      (lambda ()
        (with-exception-handler
          (lambda (e) (raise 'reraised))
          (lambda () (raise 'inner)))))))

(test-group "raise-continuable"
  (test-equal "raise-continuable calls handler" 42
    (with-exception-handler
      (lambda (e) (+ e 1))
      (lambda () (raise-continuable 41)))))

(test-group "rationalize"
  (test-equal "rationalize 3/10 1/10" 1/3 (rationalize 3/10 1/10))
  (test-equal "rationalize 7/16 1/16" 1/2 (rationalize 7/16 1/16))
  (test-equal "rationalize integer" 3 (rationalize 4 1))
  (test-equal "rationalize zero" 0 (rationalize 0 1/2))
  ;; Exactness preserved
  (test-eqv "rationalize exact is exact" #t (exact? (rationalize 3/10 1/10)))
  (test-eqv "rationalize inexact is inexact" #t (inexact? (rationalize 0.3 0.1))))

(test-group "set-car!/set-cdr!"
  ;; set-car! / set-first! mutation
  (define p (cons 1 2))
  (set-car! p 99)
  (test-eqv "set-car! changes car" 99 (car p))
  (test-eqv "set-car! preserves cdr" 2 (cdr p))

  ;; set-first! is an alias for set-car!
  (define q (cons 'a 'b))
  (set-first! q 'z)
  (test-eq "set-first! changes car" 'z (car q))

  ;; set-cdr! / set-rest! mutation
  (define r (cons 1 2))
  (set-cdr! r 99)
  (test-eqv "set-cdr! changes cdr" 99 (cdr r))
  (test-eqv "set-cdr! preserves car" 1 (car r))

  ;; set-rest! is an alias for set-cdr!
  (define s (cons 'a 'b))
  (set-rest! s '(c d))
  (test-eq "set-rest! changes cdr" 'c (cadr s))

  ;; set-cdr! with list tail
  (define lst (list 1 2 3))
  (set-cdr! lst '(20 30))
  (test-equal "set-cdr! on list" '(1 20 30) lst))

(test-group "truncate-quotient/truncate-remainder"
  ;; truncate-quotient truncates toward zero, matching quotient
  (test-eqv "truncate-quotient 7 2" 3 (truncate-quotient 7 2))
  (test-eqv "truncate-quotient -7 2" -3 (truncate-quotient -7 2))
  (test-eqv "truncate-quotient 7 -2" -3 (truncate-quotient 7 -2))
  (test-eqv "truncate-quotient -7 -2" 3 (truncate-quotient -7 -2))

  ;; truncate-remainder: sign follows dividend, matching remainder
  (test-eqv "truncate-remainder 7 2" 1 (truncate-remainder 7 2))
  (test-eqv "truncate-remainder -7 2" -1 (truncate-remainder -7 2))
  (test-eqv "truncate-remainder 7 -2" 1 (truncate-remainder 7 -2))
  (test-eqv "truncate-remainder -7 -2" -1 (truncate-remainder -7 -2))

  ;; Compatibility with quotient/remainder
  (test-eqv "truncate-quotient = quotient" #t
    (= (truncate-quotient 13 4) (quotient 13 4)))
  (test-eqv "truncate-remainder = remainder" #t
    (= (truncate-remainder 13 4) (remainder 13 4)))

  ;; Euclidean identity: n = q*d + r
  (test-eqv "truncate identity" #t
    (= 13 (+ (* (truncate-quotient 13 4) 4) (truncate-remainder 13 4)))))

(test-group "asin"
  ;; asin(0) = 0 exactly
  (test-equal "asin 0" 0.0 (asin 0))
  ;; sin is the left-inverse of asin on [-1, 1]
  (test-assert "asin/sin roundtrip"
    (< (abs (- (asin (sin 0.5)) 0.5)) 1e-10))
  ;; 2*asin(1) = acos(-1) = pi
  (test-assert "asin 1 = pi/2"
    (< (abs (- (* 2.0 (asin 1)) (acos -1))) 1e-10))
  ;; Negative argument: asin(-1) = -pi/2, so 2*asin(-1) + acos(-1) = -pi + pi = 0
  (test-assert "asin -1 = -pi/2"
    (< (abs (+ (* 2.0 (asin -1)) (acos -1))) 1e-10)))

(test-group "acos"
  ;; acos(1) = 0 exactly
  (test-equal "acos 1" 0.0 (acos 1))
  ;; cos is the left-inverse of acos on [0, pi]
  (test-assert "acos/cos roundtrip"
    (< (abs (- (acos (cos 1.0)) 1.0)) 1e-10))
  ;; asin(x) + acos(x) = pi/2 for all x in [-1, 1]
  (test-assert "asin + acos = pi/2"
    (< (abs (- (+ (asin 0.5) (acos 0.5)) (* 0.5 (acos -1)))) 1e-10))
  ;; acos(-1) = pi (the canonical source of pi)
  (test-assert "acos -1 = pi"
    (< (abs (- (acos -1) 3.141592653589793)) 1e-10)))

(test-group "call-with-port"
  ;; Return value is the result of the procedure
  (define p1 (open-output-string))
  (define r1 (call-with-port p1 (lambda (port) (write-string "hello" port) 'done)))
  (test-eq "call-with-port returns proc result" 'done r1)
  ;; Output port is closed after the call
  (test-eqv "output port closed after call-with-port" #f (output-port-open? p1))

  ;; Works with input ports
  (define p2 (open-input-string "abc"))
  (define r2 (call-with-port p2 (lambda (port) (read-char port))))
  (test-eqv "call-with-port reads from input port" #\a r2)
  ;; Input port is closed after the call
  (test-eqv "input port closed after call-with-port" #f (input-port-open? p2)))

(test-group "max/min exactness contagion"
  ;; When all args are exact, result is exact
  (test-eqv "max exact exact is exact" #t (exact? (max 1 2)))
  (test-eqv "min exact exact is exact" #t (exact? (min 1 2)))
  ;; When any arg is inexact, result is inexact (R7RS 6.2.6)
  (test-eqv "max inexact exact is inexact" #t (inexact? (max 1 2.0)))
  (test-eqv "max exact inexact is inexact" #t (inexact? (max 1.0 2)))
  (test-eqv "min inexact exact is inexact" #t (inexact? (min 1 2.0)))
  (test-eqv "min exact inexact is inexact" #t (inexact? (min 1.0 2)))
  ;; Value is still correct even when coerced
  (test-equal "max value correct after inexact coercion" 3.0 (max 1 3.0 2))
  (test-equal "min value correct after inexact coercion" 1.0 (min 3 1.0 2)))

(test-group "floor/ceiling/round/truncate on rationals"
  ;; These exercise the *big.Rat code paths
  (test-eqv "floor 7/2" 3 (floor 7/2))
  (test-eqv "floor -7/2" -4 (floor -7/2))
  (test-eqv "floor 4/2 exact integer" 2 (floor 4/2))
  (test-eqv "ceiling 7/2" 4 (ceiling 7/2))
  (test-eqv "ceiling -7/2" -3 (ceiling -7/2))
  (test-eqv "ceiling 4/2 exact integer" 2 (ceiling 4/2))
  ;; round uses banker's rounding (round-half-to-even)
  (test-eqv "round 7/2 = 4 (even)" 4 (round 7/2))
  (test-eqv "round 5/2 = 2 (even)" 2 (round 5/2))
  (test-eqv "round 3/2 = 2 (even)" 2 (round 3/2))
  (test-eqv "truncate 7/2" 3 (truncate 7/2))
  (test-eqv "truncate -7/2" -3 (truncate -7/2))
  ;; Results should be exact integers
  (test-eqv "floor result is exact" #t (exact? (floor 7/2)))
  (test-eqv "ceiling result is exact" #t (exact? (ceiling 7/2)))
  (test-eqv "truncate result is exact" #t (exact? (truncate 7/2))))

(test-group "numerator/denominator on exact rationals"
  ;; Integer inputs
  (test-eqv "numerator of exact integer" 5 (numerator 5))
  (test-eqv "denominator of exact integer" 1 (denominator 5))
  ;; Rational inputs
  (test-eqv "numerator of 3/4" 3 (numerator 3/4))
  (test-eqv "denominator of 3/4" 4 (denominator 3/4))
  (test-eqv "numerator of -3/4" -3 (numerator -3/4))
  (test-eqv "denominator of -3/4" 4 (denominator -3/4))
  ;; Reducible fractions are simplified
  (test-eqv "numerator of 6/4 = 3" 3 (numerator 6/4))
  (test-eqv "denominator of 6/4 = 2" 2 (denominator 6/4))
  ;; Results are exact
  (test-eqv "numerator result is exact" #t (exact? (numerator 3/4)))
  (test-eqv "denominator result is exact" #t (exact? (denominator 3/4))))

(test-group "member/assoc with custom comparator"
  ;; member with 3-arg form
  (test-equal "member custom comparator found" '(2 3)
    (member 2.0 '(1 2 3) =))
  (test-eqv "member custom comparator not found" #f
    (member 4 '(1 2 3) =))
  ;; assoc with 3-arg form
  (test-equal "assoc custom comparator found" '(2 . "two")
    (assoc 2.0 '((1 . "one") (2 . "two") (3 . "three")) =))
  (test-eqv "assoc custom comparator not found" #f
    (assoc 4 '((1 . "one") (2 . "two")) =)))

(test-end "gscheme")
