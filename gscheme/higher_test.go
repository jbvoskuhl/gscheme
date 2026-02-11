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
