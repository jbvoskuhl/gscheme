package gscheme

import "testing"

func TestMap(t *testing.T) {
	scheme := New()

	// (map (lambda (x) (* x 2)) '(1 2 3)) => (2 4 6)
	result := scheme.EvalGlobal(List(Symbol("map"),
		List(Symbol("lambda"), List(Symbol("x")),
			List(Symbol("*"), Symbol("x"), float64(2))),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))

	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(2) || First(Rest(pair)) != float64(4) || First(Rest(Rest(pair))) != float64(6) {
		t.Errorf("Expected (2 4 6) but got: %v", result)
	}
}

func TestMapEmpty(t *testing.T) {
	scheme := New()

	// (map (lambda (x) x) '()) => ()
	result := scheme.EvalGlobal(List(Symbol("map"),
		List(Symbol("lambda"), List(Symbol("x")), Symbol("x")),
		List(Symbol("quote"), nil)))

	if result != nil {
		t.Errorf("Expected nil but got: %v", result)
	}
}

func TestApply(t *testing.T) {
	scheme := New()

	// (apply + '(1 2 3)) => 6
	result := scheme.EvalGlobal(List(Symbol("apply"),
		Symbol("+"),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))

	if result != float64(6) {
		t.Errorf("Expected 6 but got: %v", result)
	}
}

func TestMember(t *testing.T) {
	scheme := New()

	// (member 2 '(1 2 3)) => (2 3)
	result := scheme.EvalGlobal(List(Symbol("member"),
		float64(2),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))

	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(2) {
		t.Errorf("Expected (2 3) but got: %v", result)
	}

	// (member 4 '(1 2 3)) => #f
	result = scheme.EvalGlobal(List(Symbol("member"),
		float64(4),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))

	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestForEach(t *testing.T) {
	scheme := New()

	// (let ((sum 0)) (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3)) sum) => 6
	result := scheme.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("sum"), float64(0))),
		List(Symbol("for-each"),
			List(Symbol("lambda"), List(Symbol("x")),
				List(Symbol("set!"), Symbol("sum"),
					List(Symbol("+"), Symbol("sum"), Symbol("x")))),
			List(Symbol("quote"), List(float64(1), float64(2), float64(3)))),
		Symbol("sum")))
	if result != float64(6) {
		t.Errorf("Expected 6 but got: %v", result)
	}

	// (for-each (lambda (x) x) '()) => ()
	result = scheme.EvalGlobal(List(Symbol("for-each"),
		List(Symbol("lambda"), List(Symbol("x")), Symbol("x")),
		List(Symbol("quote"), nil)))
	if result != nil {
		t.Errorf("Expected nil but got: %v", result)
	}
}

func TestMemq(t *testing.T) {
	scheme := New()

	// (memq 'b '(a b c)) => (b c)
	result := scheme.EvalGlobal(List(Symbol("memq"),
		List(Symbol("quote"), Symbol("b")),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c")))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != Symbol("b") {
		t.Errorf("Expected (b c) but got: %v", result)
	}

	// (memq 'd '(a b c)) => #f
	result = scheme.EvalGlobal(List(Symbol("memq"),
		List(Symbol("quote"), Symbol("d")),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c")))))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestMemv(t *testing.T) {
	scheme := New()

	// (memv 2 '(1 2 3)) => (2 3)
	result := scheme.EvalGlobal(List(Symbol("memv"),
		float64(2),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(2) {
		t.Errorf("Expected (2 3) but got: %v", result)
	}

	// (memv 4 '(1 2 3)) => #f
	result = scheme.EvalGlobal(List(Symbol("memv"),
		float64(4),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestAssoc(t *testing.T) {
	scheme := New()

	// (assoc 'b '((a 1) (b 2) (c 3))) => (b 2)
	result := scheme.EvalGlobal(List(Symbol("assoc"),
		List(Symbol("quote"), Symbol("b")),
		List(Symbol("quote"), List(
			List(Symbol("a"), float64(1)),
			List(Symbol("b"), float64(2)),
			List(Symbol("c"), float64(3))))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != Symbol("b") || Second(pair) != float64(2) {
		t.Errorf("Expected (b 2) but got: %v", result)
	}

	// (assoc 'd '((a 1) (b 2))) => #f
	result = scheme.EvalGlobal(List(Symbol("assoc"),
		List(Symbol("quote"), Symbol("d")),
		List(Symbol("quote"), List(
			List(Symbol("a"), float64(1)),
			List(Symbol("b"), float64(2))))))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestAssq(t *testing.T) {
	scheme := New()

	// (assq 'b '((a 1) (b 2) (c 3))) => (b 2)
	result := scheme.EvalGlobal(List(Symbol("assq"),
		List(Symbol("quote"), Symbol("b")),
		List(Symbol("quote"), List(
			List(Symbol("a"), float64(1)),
			List(Symbol("b"), float64(2)),
			List(Symbol("c"), float64(3))))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != Symbol("b") {
		t.Errorf("Expected (b 2) but got: %v", result)
	}
}

func TestAssv(t *testing.T) {
	scheme := New()

	// (assv 2 '((1 a) (2 b) (3 c))) => (2 b)
	result := scheme.EvalGlobal(List(Symbol("assv"),
		float64(2),
		List(Symbol("quote"), List(
			List(float64(1), Symbol("a")),
			List(float64(2), Symbol("b")),
			List(float64(3), Symbol("c"))))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(2) || Second(pair) != Symbol("b") {
		t.Errorf("Expected (2 b) but got: %v", result)
	}

	// (assv 4 '((1 a) (2 b))) => #f
	result = scheme.EvalGlobal(List(Symbol("assv"),
		float64(4),
		List(Symbol("quote"), List(
			List(float64(1), Symbol("a")),
			List(float64(2), Symbol("b"))))))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestEval(t *testing.T) {
	scheme := New()

	// (eval '(+ 1 2)) => 3
	result := scheme.EvalGlobal(List(Symbol("eval"),
		List(Symbol("quote"), List(Symbol("+"), float64(1), float64(2)))))

	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}
}

func TestOr(t *testing.T) {
	scheme := New()

	// (or #f #f 3) => 3
	result := scheme.EvalGlobal(List(Symbol("or"), false, false, float64(3)))
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}

	// (or #f #f #f) => #f
	result = scheme.EvalGlobal(List(Symbol("or"), false, false, false))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}

	// (or 1 2 3) => 1
	result = scheme.EvalGlobal(List(Symbol("or"), float64(1), float64(2), float64(3)))
	if result != float64(1) {
		t.Errorf("Expected 1 but got: %v", result)
	}
}

func TestAnd(t *testing.T) {
	scheme := New()

	// (and 1 2 3) => 3
	result := scheme.EvalGlobal(List(Symbol("and"), float64(1), float64(2), float64(3)))
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}

	// (and 1 #f 3) => #f
	result = scheme.EvalGlobal(List(Symbol("and"), float64(1), false, float64(3)))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}

	// (and) => #t
	result = scheme.EvalGlobal(List(Symbol("and")))
	if result != true {
		t.Errorf("Expected #t but got: %v", result)
	}
}

func TestLet(t *testing.T) {
	scheme := New()

	// (let ((x 1) (y 2)) (+ x y)) => 3
	result := scheme.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("x"), float64(1)), List(Symbol("y"), float64(2))),
		List(Symbol("+"), Symbol("x"), Symbol("y"))))

	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}
}

func TestLetStar(t *testing.T) {
	scheme := New()

	// (let* ((x 1) (y (+ x 1))) y) => 2
	result := scheme.EvalGlobal(List(Symbol("let*"),
		List(List(Symbol("x"), float64(1)),
			List(Symbol("y"), List(Symbol("+"), Symbol("x"), float64(1)))),
		Symbol("y")))

	if result != float64(2) {
		t.Errorf("Expected 2 but got: %v", result)
	}
}

func TestLetrec(t *testing.T) {
	scheme := New()

	// (letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5)) => 120
	result := scheme.EvalGlobal(List(Symbol("letrec"),
		List(List(Symbol("fact"),
			List(Symbol("lambda"), List(Symbol("n")),
				List(Symbol("if"),
					List(Symbol("="), Symbol("n"), float64(0)),
					float64(1),
					List(Symbol("*"), Symbol("n"),
						List(Symbol("fact"),
							List(Symbol("-"), Symbol("n"), float64(1)))))))),
		List(Symbol("fact"), float64(5))))

	if result != float64(120) {
		t.Errorf("Expected 120 but got: %v", result)
	}
}

func eval(code string) interface{} {
	s := New()
	input := NewInputPortFromString(code)
	var result interface{}
	for {
		x := input.Read()
		if IsEOF(x) {
			return result
		}
		result = s.EvalGlobal(x)
	}
}

func TestQuasiquote(t *testing.T) {
	// Basic quasiquote with no unquotes
	result := eval("`(1 2 3)")
	if Stringify(result) != "(1 2 3)" {
		t.Errorf("Expected (1 2 3) but got: %v", Stringify(result))
	}

	// Quasiquote with unquote
	result = eval("(define x 42) `(a ,x c)")
	if Stringify(result) != "(a 42 c)" {
		t.Errorf("Expected (a 42 c) but got: %v", Stringify(result))
	}

	// Quasiquote with unquote-splicing
	result = eval("(define xs '(1 2 3)) `(a ,@xs b)")
	if Stringify(result) != "(a 1 2 3 b)" {
		t.Errorf("Expected (a 1 2 3 b) but got: %v", Stringify(result))
	}

	// Nested quasiquote with expression
	result = eval("`(a ,(+ 1 2) c)")
	if Stringify(result) != "(a 3 c)" {
		t.Errorf("Expected (a 3 c) but got: %v", Stringify(result))
	}

	// Quasiquote with only constants
	result = eval("`(a b c)")
	if Stringify(result) != "(a b c)" {
		t.Errorf("Expected (a b c) but got: %v", Stringify(result))
	}

	// Quasiquote with symbol
	result = eval("`x")
	if result != Symbol("x") {
		t.Errorf("Expected x but got: %v", result)
	}

	// Quasiquote with number
	result = eval("`42")
	if result != float64(42) {
		t.Errorf("Expected 42 but got: %v", result)
	}
}

func TestCase(t *testing.T) {
	// Basic case with matching clause
	result := eval("(case (+ 1 1) ((1) 'one) ((2) 'two) ((3) 'three))")
	if result != Symbol("two") {
		t.Errorf("Expected two but got: %v", result)
	}

	// Case with else clause
	result = eval("(case 99 ((1) 'one) ((2) 'two) (else 'other))")
	if result != Symbol("other") {
		t.Errorf("Expected other but got: %v", result)
	}

	// Case with multiple values in a clause
	result = eval("(case 2 ((1 2 3) 'low) ((4 5 6) 'high))")
	if result != Symbol("low") {
		t.Errorf("Expected low but got: %v", result)
	}

	// Case with no match and no else
	result = eval("(case 99 ((1) 'one) ((2) 'two))")
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestDo(t *testing.T) {
	// Simple do loop: count to 3
	result := eval(`
		(do ((n 0 (+ n 1)))
		    ((= n 3) n))
	`)
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}

	// Basic do loop: sum 1 to 5
	result = eval(`
		(do ((n 1 (+ n 1))
		     (sum 0 (+ sum n)))
		    ((> n 5) sum))
	`)
	if result != float64(15) {
		t.Errorf("Expected 15 but got: %v", result)
	}

	// Do loop building a list (reversed)
	result = eval(`
		(do ((n 0 (+ n 1))
		     (lst '() (cons n lst)))
		    ((= n 3) lst))
	`)
	if Stringify(result) != "(2 1 0)" {
		t.Errorf("Expected (2 1 0) but got: %v", Stringify(result))
	}
}

func TestDelayForce(t *testing.T) {
	// Basic delay/force
	result := eval("(force (delay (+ 1 2)))")
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}

	// Promise memoization: value is computed only once
	result = eval(`
		(define count 0)
		(define p (delay (begin (set! count (+ count 1)) count)))
		(force p)
		(force p)
		count
	`)
	if result != float64(1) {
		t.Errorf("Expected 1 (promise should memoize) but got: %v", result)
	}

	// Promise is a procedure
	result = eval("(procedure? (delay 42))")
	if result != true {
		t.Errorf("Expected #t but got: %v", result)
	}
}
