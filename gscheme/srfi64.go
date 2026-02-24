package gscheme

// srfi64Scheme implements a subset of SRFI 64 (Scheme API for test suites)
// as embedded Scheme code.
const srfi64Scheme = `
;; SRFI 64 — A Scheme API for test suites (subset)

;; State variables
(define %test-pass-count 0)
(define %test-fail-count 0)
(define %test-suite-name "")

;; Internal helpers
(define (%test-pass name)
  (set! %test-pass-count (+ %test-pass-count 1)))

(define (%test-fail name expected actual)
  (set! %test-fail-count (+ %test-fail-count 1))
  (display "FAIL: ")
  (display name)
  (display " — expected ")
  (write expected)
  (display ", got ")
  (write actual)
  (newline))

;; Suite structure
(define (test-begin name)
  (set! %test-suite-name name)
  (set! %test-pass-count 0)
  (set! %test-fail-count 0))

(define (test-end . args)
  (display %test-pass-count)
  (display " passed, ")
  (display %test-fail-count)
  (display " failed")
  (newline))

;; test-group: wrap test-begin/test-end around a body
(define test-group
  (macro (name . body)
    (cons 'begin body)))

;; test-equal: compare with equal?
(define test-equal
  (macro args
    (if (= (length args) 3)
        ;; (test-equal name expected test-expr)
        (let ((name (first args))
              (expected (second args))
              (test-expr (third args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'equal? '__expected__ '__actual__)
                      (list '%test-pass name)
                      (list '%test-fail name '__expected__ '__actual__))))
        ;; (test-equal expected test-expr) — auto-generate name
        (let ((expected (first args))
              (test-expr (second args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'equal? '__expected__ '__actual__)
                      (list '%test-pass (write-to-string test-expr))
                      (list '%test-fail (write-to-string test-expr)
                            '__expected__ '__actual__)))))))

;; test-eqv: compare with eqv?
(define test-eqv
  (macro args
    (if (= (length args) 3)
        (let ((name (first args))
              (expected (second args))
              (test-expr (third args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'eqv? '__expected__ '__actual__)
                      (list '%test-pass name)
                      (list '%test-fail name '__expected__ '__actual__))))
        (let ((expected (first args))
              (test-expr (second args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'eqv? '__expected__ '__actual__)
                      (list '%test-pass (write-to-string test-expr))
                      (list '%test-fail (write-to-string test-expr)
                            '__expected__ '__actual__)))))))

;; test-eq: compare with eq?
(define test-eq
  (macro args
    (if (= (length args) 3)
        (let ((name (first args))
              (expected (second args))
              (test-expr (third args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'eq? '__expected__ '__actual__)
                      (list '%test-pass name)
                      (list '%test-fail name '__expected__ '__actual__))))
        (let ((expected (first args))
              (test-expr (second args)))
          (list 'let (list (list '__expected__ expected)
                           (list '__actual__ test-expr))
                (list 'if (list 'eq? '__expected__ '__actual__)
                      (list '%test-pass (write-to-string test-expr))
                      (list '%test-fail (write-to-string test-expr)
                            '__expected__ '__actual__)))))))

;; test-assert: expr must be truthy
(define test-assert
  (macro args
    (if (= (length args) 2)
        (let ((name (first args))
              (expr (second args)))
          (list 'if expr
                (list '%test-pass name)
                (list '%test-fail name #t expr)))
        (let ((expr (first args)))
          (list 'if expr
                (list '%test-pass (write-to-string expr))
                (list '%test-fail (write-to-string expr) #t #f))))))

;; test-error: expr must signal an error
(define test-error
  (macro args
    (if (= (length args) 2)
        (let ((name (first args))
              (test-expr (second args)))
          (list 'let (list (list '__result__
                                 (list 'guard (list '__e__ (list #t #t))
                                       test-expr #f)))
                (list 'if '__result__
                      (list '%test-pass name)
                      (list '%test-fail name "error" "no error"))))
        (let ((test-expr (first args)))
          (list 'let (list (list '__result__
                                 (list 'guard (list '__e__ (list #t #t))
                                       test-expr #f)))
                (list 'if '__result__
                      (list '%test-pass (write-to-string test-expr))
                      (list '%test-fail (write-to-string test-expr)
                            "error" "no error")))))))

;; test: chibi-style shorthand — (test name expected test-expr)
(define test
  (macro (name expected test-expr)
    (list 'test-equal name expected test-expr)))

;; write-to-string: helper to stringify an expression for auto-naming
(define (write-to-string x)
  (cond ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((number? x) (number->string x))
        ((pair? x) (string-append "(" (write-to-string (car x)) " ...)"))
        (else "?")))
`

// LoadSRFI64 loads the SRFI 64 test framework into the interpreter.
func (s *scheme) LoadSRFI64() {
	s.LoadCode(srfi64Scheme)
}
