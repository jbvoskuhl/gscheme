package gscheme

// primitivesScheme contains Scheme primitives implemented in Scheme.
// Ported from jscheme/primitives.scm by Peter Norvig and Darius Bacon.
const primitivesScheme = `
;; Scheme primitives implemented in Scheme.
;; Ported from jscheme/primitives.scm

;;;;;;;;;;;;;;;; Aliases for list accessors
(define second cadr)
(define third caddr)

;;;;;;;;;;;;;;;; Standard Scheme Macros

;; or macro: returns the first truthy value or #f
(define or
  (macro args
    (if (null? args)
        #f
        (cons 'cond (map list args)))))

;; and macro: returns #f if any value is false, else returns last value
(define and
  (macro args
    (cond ((null? args) #t)
          ((null? (rest args)) (first args))
          (else (list 'if (first args) (cons 'and (rest args)) #f)))))

;; let macro without quasiquote dependency
;; (let ((x 1) (y 2)) body...) => ((lambda (x y) body...) 1 2)
(define let
  (macro (bindings . body)
    (if (symbol? bindings)
        ;; named let: (let name ((x 1)) body...)
        (let ((name bindings)
              (real-bindings (first body))
              (real-body (rest body)))
          (list 'let (list (list name #f))
                (list 'set! name
                      (cons 'lambda
                            (cons (map first real-bindings) real-body)))
                (cons name (map second real-bindings))))
        ;; regular let
        (cons (cons 'lambda (cons (map first bindings) body))
              (map second bindings)))))

;; let* macro: sequential binding
(define let*
  (macro (bindings . body)
    (if (null? bindings)
        (cons 'begin body)
        (list 'let (list (first bindings))
              (cons 'let* (cons (rest bindings) body))))))

;; letrec macro: recursive bindings
(define letrec
  (macro (bindings . body)
    (cons 'let
          (cons (map (lambda (b) (list (first b) #f)) bindings)
                (append (map (lambda (b) (list 'set! (first b) (second b))) bindings)
                        body)))))

;;;;;;;;;;;;;;;; Quasiquote

;; quasiquote macro: backquote with unquote and unquote-splicing
;; Based on jscheme/primitives.scm by Peter Norvig and Darius Bacon.
(define quasiquote
  (macro (x)
    (define (constant? exp)
      (if (pair? exp) (eq? (car exp) 'quote) (not (symbol? exp))))
    (define (combine-skeletons left right exp)
      (cond
       ((and (constant? left) (constant? right))
        (list 'quote exp))
       ((null? right) (list 'list left))
       ((and (pair? right) (eq? (car right) 'list))
        (cons 'list (cons left (cdr right))))
       (else (list 'cons left right))))
    (define (expand-quasiquote exp nesting)
      (cond
       ((vector? exp)
        (list 'apply 'vector (expand-quasiquote (vector->list exp) nesting)))
       ((not (pair? exp))
        (if (constant? exp) exp (list 'quote exp)))
       ((and (eq? (car exp) 'unquote) (= (length exp) 2))
        (if (= nesting 0)
            (second exp)
            (combine-skeletons (list 'quote 'unquote)
                               (expand-quasiquote (cdr exp) (- nesting 1))
                               exp)))
       ((and (eq? (car exp) 'quasiquote) (= (length exp) 2))
        (combine-skeletons (list 'quote 'quasiquote)
                           (expand-quasiquote (cdr exp) (+ nesting 1))
                           exp))
       ((and (pair? (car exp))
             (eq? (caar exp) 'unquote-splicing)
             (= (length (car exp)) 2))
        (if (= nesting 0)
            (list 'append (second (first exp))
                  (expand-quasiquote (cdr exp) nesting))
            (combine-skeletons (expand-quasiquote (car exp) (- nesting 1))
                               (expand-quasiquote (cdr exp) nesting)
                               exp)))
       (else (combine-skeletons (expand-quasiquote (car exp) nesting)
                                (expand-quasiquote (cdr exp) nesting)
                                exp))))
    (expand-quasiquote x 0)))

;;;;;;;;;;;;;;;; case, do, delay, force

;; case macro: multi-way dispatch
(define case
  (macro (exp . cases)
    (define (do-case case)
      (cond ((not (pair? case)) (error "bad syntax in case" case))
            ((eq? (first case) 'else) case)
            (else (cons (list 'member '__exp__ (list 'quote (first case)))
                        (rest case)))))
    (list 'let (list (list '__exp__ exp))
          (cons 'cond (map do-case cases)))))

;; do macro: iteration construct
(define do
  (macro (bindings test-and-result . body)
    (let ((variables (map first bindings))
          (inits (map second bindings))
          (steps (map (lambda (clause)
                        (if (null? (cddr clause))
                            (first clause)
                            (third clause)))
                      bindings))
          (test (first test-and-result))
          (result (rest test-and-result)))
      (list 'letrec
            (list (list '__loop__
                        (cons 'lambda
                              (cons variables
                                    (list (list 'if test
                                                (cons 'begin result)
                                                (cons 'begin
                                                      (append body
                                                              (list (cons '__loop__ steps))))))))))
            (cons '__loop__ inits)))))

;; delay macro: lazy evaluation, creates a promise
(define delay
  (macro (exp)
    (list 'let (list (list '__result-ready?__ #f)
                     (list '__result__ #f))
          (list 'lambda '()
                (list 'if '__result-ready?__
                      '__result__
                      (list 'let (list (list '__x__ exp))
                            (list 'if '__result-ready?__
                                  '__result__
                                  (list 'begin
                                        (list 'set! '__result-ready?__ #t)
                                        (list 'set! '__result__ '__x__)
                                        '__result__))))))))

;; force: evaluate a promise
(define (force promise) (promise))
`

// loadPrimitivesScheme loads the embedded primitives Scheme code.
func (s *scheme) loadPrimitivesScheme() {
	s.LoadCode(primitivesScheme)
}
