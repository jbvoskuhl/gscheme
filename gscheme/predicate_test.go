package gscheme

import "testing"

func TestEq(t *testing.T) {
	scheme := New()
	// Same symbol should be eq?
	scheme.EvalGlobal(List(Symbol("define"), Symbol("x"), List(Symbol("quote"), Symbol("foo"))))
	result := scheme.EvalGlobal(List(Symbol("eq?"), Symbol("x"), Symbol("x")))
	if result != true {
		t.Errorf("Expected (eq? x x) to be true but was: %v", result)
	}

	// Different objects should not be eq?
	result = scheme.EvalGlobal(List(Symbol("eq?"), float64(1), float64(1)))
	// In Go, float64(1) == float64(1) is true for identity
	if result != true {
		t.Errorf("Expected (eq? 1 1) to be true but was: %v", result)
	}

	// Different values should not be eq?
	result = scheme.EvalGlobal(List(Symbol("eq?"), float64(1), float64(2)))
	if result != false {
		t.Errorf("Expected (eq? 1 2) to be false but was: %v", result)
	}
}

func TestEqv(t *testing.T) {
	scheme := New()
	// Numbers with same value should be eqv?
	result := scheme.EvalGlobal(List(Symbol("eqv?"), float64(42), float64(42)))
	if result != true {
		t.Errorf("Expected (eqv? 42 42) to be true but was: %v", result)
	}

	// Different numbers should not be eqv?
	result = scheme.EvalGlobal(List(Symbol("eqv?"), float64(1), float64(2)))
	if result != false {
		t.Errorf("Expected (eqv? 1 2) to be false but was: %v", result)
	}

	// Booleans
	result = scheme.EvalGlobal(List(Symbol("eqv?"), true, true))
	if result != true {
		t.Errorf("Expected (eqv? #t #t) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("eqv?"), true, false))
	if result != false {
		t.Errorf("Expected (eqv? #t #f) to be false but was: %v", result)
	}
}

func TestEqual(t *testing.T) {
	scheme := New()
	// Equal lists should be equal?
	result := scheme.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3))),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	if result != true {
		t.Errorf("Expected (equal? '(1 2 3) '(1 2 3)) to be true but was: %v", result)
	}

	// Different lists should not be equal?
	result = scheme.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("quote"), List(float64(1), float64(2))),
		List(Symbol("quote"), List(float64(1), float64(3)))))
	if result != false {
		t.Errorf("Expected (equal? '(1 2) '(1 3)) to be false but was: %v", result)
	}

	// Nested lists
	result = scheme.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("quote"), List(List(float64(1)), float64(2))),
		List(Symbol("quote"), List(List(float64(1)), float64(2)))))
	if result != true {
		t.Errorf("Expected (equal? '((1) 2) '((1) 2)) to be true but was: %v", result)
	}
}

func TestNumberP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("number?"), float64(42)))
	if result != true {
		t.Errorf("Expected (number? 42) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("number?"), Symbol("x")))
	if result != false {
		t.Errorf("Expected (number? 'x) to be false but was: %v", result)
	}
}

func TestIntegerP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("integer?"), float64(42)))
	if result != true {
		t.Errorf("Expected (integer? 42) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("integer?"), float64(3.14)))
	if result != false {
		t.Errorf("Expected (integer? 3.14) to be false but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("integer?"), Symbol("x")))
	if result != false {
		t.Errorf("Expected (integer? 'x) to be false but was: %v", result)
	}
}

func TestStringP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("string?"), "hello"))
	if result != true {
		t.Errorf("Expected (string? \"hello\") to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("string?"), float64(42)))
	if result != false {
		t.Errorf("Expected (string? 42) to be false but was: %v", result)
	}
}

func TestProcedureP(t *testing.T) {
	scheme := New()
	// Built-in procedure
	result := scheme.EvalGlobal(List(Symbol("procedure?"), Symbol("+")))
	if result != true {
		t.Errorf("Expected (procedure? +) to be true but was: %v", result)
	}

	// Lambda
	scheme.EvalGlobal(List(Symbol("define"), Symbol("f"),
		List(Symbol("lambda"), List(Symbol("x")), Symbol("x"))))
	result = scheme.EvalGlobal(List(Symbol("procedure?"), Symbol("f")))
	if result != true {
		t.Errorf("Expected (procedure? f) to be true for lambda but was: %v", result)
	}

	// Non-procedure
	result = scheme.EvalGlobal(List(Symbol("procedure?"), float64(42)))
	if result != false {
		t.Errorf("Expected (procedure? 42) to be false but was: %v", result)
	}
}

func TestZeroP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("zero?"), float64(0)))
	if result != true {
		t.Errorf("Expected (zero? 0) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("zero?"), float64(1)))
	if result != false {
		t.Errorf("Expected (zero? 1) to be false but was: %v", result)
	}
}

func TestPositiveP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("positive?"), float64(5)))
	if result != true {
		t.Errorf("Expected (positive? 5) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("positive?"), float64(-5)))
	if result != false {
		t.Errorf("Expected (positive? -5) to be false but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("positive?"), float64(0)))
	if result != false {
		t.Errorf("Expected (positive? 0) to be false but was: %v", result)
	}
}

func TestNegativeP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("negative?"), float64(-5)))
	if result != true {
		t.Errorf("Expected (negative? -5) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("negative?"), float64(5)))
	if result != false {
		t.Errorf("Expected (negative? 5) to be false but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("negative?"), float64(0)))
	if result != false {
		t.Errorf("Expected (negative? 0) to be false but was: %v", result)
	}
}

func TestOddP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("odd?"), float64(3)))
	if result != true {
		t.Errorf("Expected (odd? 3) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("odd?"), float64(4)))
	if result != false {
		t.Errorf("Expected (odd? 4) to be false but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("odd?"), float64(-3)))
	if result != true {
		t.Errorf("Expected (odd? -3) to be true but was: %v", result)
	}
}

func TestEvenP(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("even?"), float64(4)))
	if result != true {
		t.Errorf("Expected (even? 4) to be true but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("even?"), float64(3)))
	if result != false {
		t.Errorf("Expected (even? 3) to be false but was: %v", result)
	}

	result = scheme.EvalGlobal(List(Symbol("even?"), float64(0)))
	if result != true {
		t.Errorf("Expected (even? 0) to be true but was: %v", result)
	}
}
