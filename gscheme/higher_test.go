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

func TestMemberWithComparator(t *testing.T) {
	s := New()

	// (member 2.0 '(1 2 3) =) => (2 3)
	result := evalScheme(s, `(member 2.0 '(1 2 3) =)`)
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected pair but got: %v (%T)", result, result)
	}
	if Stringify(pair) != "(2 3)" {
		t.Errorf("Expected (2 3) but got: %v", Stringify(pair))
	}

	// (member "b" '("a" "b" "c") string=?) => ("b" "c")
	result = evalScheme(s, `(member "b" '("a" "b" "c") string=?)`)
	pair, ok = result.(Pair)
	if !ok {
		t.Fatalf("Expected pair but got: %v (%T)", result, result)
	}
	if Stringify(pair) != "(\"b\" \"c\")" {
		t.Errorf("Expected (\"b\" \"c\") but got: %v", Stringify(pair))
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

func TestAssocWithComparator(t *testing.T) {
	s := New()

	// (assoc 2.0 '((1 . a) (2 . b) (3 . c)) =) => (2 . b)
	result := evalScheme(s, `(assoc 2.0 '((1 . a) (2 . b) (3 . c)) =)`)
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected pair but got: %v (%T)", result, result)
	}
	if Stringify(pair) != "(2 . b)" {
		t.Errorf("Expected (2 . b) but got: %v", Stringify(pair))
	}

	// (assoc 5 '((1 . a) (2 . b)) =) => #f
	result = evalScheme(s, `(assoc 5 '((1 . a) (2 . b)) =)`)
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

func TestMacroExpand(t *testing.T) {
	s := New()
	// 'when' is defined as a macro; expanding it should produce an if expression
	result := evalScheme(s, `(macro-expand '(when #t 42))`)
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected a pair from macro-expand, got: %v", result)
	}
	if First(pair) != Symbol("if") {
		t.Errorf("Expected car of expansion to be 'if', got: %v", First(pair))
	}
}

func TestTimeCallIsPair(t *testing.T) {
	s := New()
	result := evalScheme(s, `(time-call (lambda () 42))`)
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected time-call to return a pair, got: %v", result)
	}
	if First(pair) != int64(42) {
		t.Errorf("Expected car to be 42, got: %v", First(pair))
	}
	stats := Second(pair)
	if _, ok := stats.(Pair); !ok {
		t.Errorf("Expected cadr to be a stats list, got: %v", stats)
	}
}

func TestTimeCallNonProcedure(t *testing.T) {
	s := New()
	result := evalScheme(s, `(time-call 42)`)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected error for non-procedure argument, got: %v", result)
	}
}
