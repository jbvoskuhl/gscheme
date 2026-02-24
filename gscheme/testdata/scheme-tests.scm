;; GScheme test suite — SRFI 64
;; Converted from tailcall_test.go and higher_test.go

(test-begin "gscheme")

;;;;;;;;;;;;;;;; when

(test-group "when"
  (test-equal "when true" 3 (when #t 1 2 3))
  (test-assert "when false is void" (eq? (when #f 1 2 3) (if #f #f)))
  ;; TCO test
  (define (when-loop n) (when (> n 0) (when-loop (- n 1))))
  (when-loop 100000)
  (test-assert "when TCO" #t))

;;;;;;;;;;;;;;;; unless

(test-group "unless"
  (test-eqv "unless true" #f (unless #t 1 2 3))
  (test-equal "unless false" 3 (unless #f 1 2 3))
  ;; TCO test
  (define (unless-loop n) (unless (= n 0) (unless-loop (- n 1))))
  (unless-loop 100000)
  (test-assert "unless TCO" #t))

;;;;;;;;;;;;;;;; if

(test-group "if"
  (test-equal "if consequent tail" 42 (if #t (begin 42) 0))
  (test-equal "if alternate tail" 42 (if #f 0 (begin 42)))
  ;; TCO consequent
  (define (if-loop-c n) (if (= n 0) 'done (if-loop-c (- n 1))))
  (test-eq "if TCO consequent" 'done (if-loop-c 100000))
  ;; TCO alternate
  (define (if-loop-a n) (if (> n 0) (if-loop-a (- n 1)) 'done))
  (test-eq "if TCO alternate" 'done (if-loop-a 100000)))

;;;;;;;;;;;;;;;; begin

(test-group "begin"
  (test-equal "begin returns last" 3 (begin 1 2 3))
  (test-assert "begin empty is void" (eq? (begin) (if #f #f)))
  (test-equal "begin single" 42 (begin 42))
  ;; TCO
  (define (begin-loop n) (begin (if (= n 0) 'done (begin-loop (- n 1)))))
  (test-eq "begin TCO" 'done (begin-loop 100000)))

;;;;;;;;;;;;;;;; cond

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

;;;;;;;;;;;;;;;; case

(test-group "case"
  (test-eq "case match first" 'one (case 1 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case match second" 'two (case 2 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case else" 'other (case 3 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case multiple values" 'low (case 2 ((1 2 3) 'low) ((4 5 6) 'high)))
  ;; TCO
  (define (case-loop n)
    (case (= n 0) ((#t) 'done) (else (case-loop (- n 1)))))
  (test-eq "case TCO" 'done (case-loop 100000))
  ;; Tests from higher_test.go TestCase
  (test-eq "case +1+1" 'two (case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three)))
  (test-eq "case else 99" 'other (case 99 ((1) 'one) ((2) 'two) (else 'other)))
  (test-eq "case multi" 'low (case 2 ((1 2 3) 'low) ((4 5 6) 'high)))
  (test-eqv "case no match" #f (case 99 ((1) 'one) ((2) 'two))))

;;;;;;;;;;;;;;;; do

(test-group "do"
  (test-equal "do basic" 5 (do ((i 0 (+ i 1))) ((= i 5) i)))
  (test-equal "do multiple bindings" 10
    (do ((i 0 (+ i 1)) (sum 0 (+ sum i))) ((= i 5) sum)))
  (test-equal "do with body" 30
    (let ((x 0))
      (do ((i 0 (+ i 1))) ((= i 3) x) (set! x (+ x 10)))))
  ;; TCO
  (test-eq "do TCO" 'done (do ((i 0 (+ i 1))) ((= i 100000) 'done)))
  ;; Tests from higher_test.go TestDo
  (test-equal "do count to 3" 3 (do ((n 0 (+ n 1))) ((= n 3) n)))
  (test-equal "do sum 1 to 5" 15
    (do ((n 1 (+ n 1)) (sum 0 (+ sum n))) ((> n 5) sum)))
  (test-equal "do build list" '(2 1 0)
    (do ((n 0 (+ n 1)) (lst '() (cons n lst))) ((= n 3) lst))))

;;;;;;;;;;;;;;;; let / let* / letrec

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

;;;;;;;;;;;;;;;; and

(test-group "and"
  (test-eqv "and no args" #t (and))
  (test-equal "and single" 42 (and 42))
  (test-equal "and last truthy" 3 (and 1 2 3))
  (test-eqv "and short circuits" #f (and 1 #f 3))
  (test-equal "and tail position" 42 (and #t #t (begin 42)))
  ;; TCO
  (define (and-loop n) (and (> n 0) (and-loop (- n 1))))
  (test-eqv "and TCO" #f (and-loop 100000)))

;;;;;;;;;;;;;;;; or

(test-group "or"
  (test-eqv "or no args" #f (or))
  (test-equal "or single" 42 (or 42))
  (test-equal "or first truthy" 1 (or #f 1 2))
  (test-eqv "or all false" #f (or #f #f #f))
  (test-equal "or tail position" 42 (or #f #f (begin 42)))
  ;; TCO
  (define (or-loop n) (or (= n 0) (or-loop (- n 1))))
  (test-eqv "or TCO" #t (or-loop 100000)))

;;;;;;;;;;;;;;;; error objects

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

;;;;;;;;;;;;;;;; guard

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

;;;;;;;;;;;;;;;; quasiquote

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

;;;;;;;;;;;;;;;; when/unless from higher_test.go

(test-group "when/unless extras"
  ;; when with side effects
  (define x 0)
  (when (> 5 3) (set! x 10))
  (test-equal "when side effect" 10 x)
  ;; unless with side effects
  (set! x 0)
  (unless (< 5 3) (set! x 10))
  (test-equal "unless side effect" 10 x))

;;;;;;;;;;;;;;;; delay/force

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

;;;;;;;;;;;;;;;; map/for-each/apply (evalScheme-based)

(test-group "map/for-each/apply"
  (test-equal "map multi-list" '(11 22 33) (map + '(1 2 3) '(10 20 30)))
  (test-equal "map shortest" '(11 22) (map + '(1 2) '(10 20 30)))
  (test-equal "for-each multi-list" 33
    (let ((sum 0))
      (for-each (lambda (x y) (set! sum (+ sum x y))) '(1 2) '(10 20))
      sum))
  (test-equal "apply variadic" 10 (apply + 1 2 '(3 4)))
  (test-equal "apply list" 6 (apply + '(1 2 3))))

;;;;;;;;;;;;;;;; eval / interaction-environment

(test-group "eval"
  (test-equal "eval interaction-environment" 3
    (eval '(+ 1 2) (interaction-environment))))

;;;;;;;;;;;;;;;; syntax-error

(test-group "syntax-error"
  (test-eqv "syntax-error is error" #t
    (guard (e (#t (error-object? e))) (syntax-error "bad syntax"))))

;;;;;;;;;;;;;;;; macro-expand

(test-group "macro-expand"
  ;; non-macro returns unchanged
  (test-equal "macro-expand non-macro" '(+ 1 2) (macro-expand '(+ 1 2))))

;;;;;;;;;;;;;;;; time-call

(test-group "time-call"
  (test-equal "time-call result" 3 (car (time-call (lambda () (+ 1 2)))))
  (test-equal "time-call with count" 42 (car (time-call (lambda () 42) 10))))

(test-end "gscheme")
