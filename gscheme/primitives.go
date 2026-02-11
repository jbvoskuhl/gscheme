package gscheme

// primitivesScheme contains Scheme primitives implemented in Scheme.
// Ported from jscheme/primitives.scm by Peter Norvig and Darius Bacon.
const primitivesScheme = `
;; Scheme primitives implemented in Scheme.
;; Ported from jscheme/primitives.scm

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
    (let ((vars (map first bindings))
          (vals (map second bindings)))
      (cons 'let
            (cons (map (lambda (var) (list var #f)) vars)
                  (append (map (lambda (var val) (list 'set! var val)) vars vals)
                          body))))))
`

// loadPrimitivesScheme loads the embedded primitives Scheme code.
func (s *scheme) loadPrimitivesScheme() {
	s.LoadCode(primitivesScheme)
}
