package gscheme

import (
	"math"
	"testing"
)

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

// TestNumberP has been moved to testdata/scheme-tests.scm.

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

	result = scheme.EvalGlobal(List(Symbol("integer?"), List(Symbol("quote"), Symbol("x"))))
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

func TestVectorP(t *testing.T) {
	scheme := New()
	// Test with a vector
	vec := []interface{}{float64(1), float64(2), float64(3)}
	scheme.Environment().Define(Symbol("v"), vec)
	result := scheme.EvalGlobal(List(Symbol("vector?"), Symbol("v")))
	if result != true {
		t.Errorf("Expected (vector? v) to be true but was: %v", result)
	}

	// Test with non-vector
	result = scheme.EvalGlobal(List(Symbol("vector?"), float64(42)))
	if result != false {
		t.Errorf("Expected (vector? 42) to be false but was: %v", result)
	}

	// Test with list (not a vector)
	result = scheme.EvalGlobal(List(Symbol("vector?"),
		List(Symbol("quote"), List(float64(1), float64(2)))))
	if result != false {
		t.Errorf("Expected (vector? '(1 2)) to be false but was: %v", result)
	}
}

func TestBytevectorP(t *testing.T) {
	scheme := New()
	// Test with a bytevector
	bv := []uint8{1, 2, 3}
	scheme.Environment().Define(Symbol("bv"), bv)
	result := scheme.EvalGlobal(List(Symbol("bytevector?"), Symbol("bv")))
	if result != true {
		t.Errorf("Expected (bytevector? bv) to be true but was: %v", result)
	}

	// Test with regular vector (not a bytevector)
	vec := []interface{}{float64(1), float64(2)}
	scheme.Environment().Define(Symbol("v"), vec)
	result = scheme.EvalGlobal(List(Symbol("bytevector?"), Symbol("v")))
	if result != false {
		t.Errorf("Expected (bytevector? v) to be false for regular vector but was: %v", result)
	}
}

func TestEofObjectP(t *testing.T) {
	scheme := New()
	// Test with EOF object
	scheme.Environment().Define(Symbol("eof"), EOF)
	result := scheme.EvalGlobal(List(Symbol("eof-object?"), Symbol("eof")))
	if result != true {
		t.Errorf("Expected (eof-object? eof) to be true but was: %v", result)
	}

	// Test with non-EOF
	result = scheme.EvalGlobal(List(Symbol("eof-object?"), float64(42)))
	if result != false {
		t.Errorf("Expected (eof-object? 42) to be false but was: %v", result)
	}
}

func TestInputPortP(t *testing.T) {
	scheme := New()
	// Test with input port
	port := NewInputPortFromString("test")
	scheme.Environment().Define(Symbol("port"), port)
	result := scheme.EvalGlobal(List(Symbol("input-port?"), Symbol("port")))
	if result != true {
		t.Errorf("Expected (input-port? port) to be true but was: %v", result)
	}

	// Test with non-port
	result = scheme.EvalGlobal(List(Symbol("input-port?"), float64(42)))
	if result != false {
		t.Errorf("Expected (input-port? 42) to be false but was: %v", result)
	}
}

func TestPortP(t *testing.T) {
	scheme := New()
	// Test with input port
	port := NewInputPortFromString("test")
	scheme.Environment().Define(Symbol("port"), port)
	result := scheme.EvalGlobal(List(Symbol("port?"), Symbol("port")))
	if result != true {
		t.Errorf("Expected (port? port) to be true but was: %v", result)
	}

	// Test with non-port
	result = scheme.EvalGlobal(List(Symbol("port?"), float64(42)))
	if result != false {
		t.Errorf("Expected (port? 42) to be false but was: %v", result)
	}
}

func TestRationalP(t *testing.T) {
	scheme := New()
	// Real numbers are rational
	result := scheme.EvalGlobal(List(Symbol("rational?"), float64(3.14)))
	if result != true {
		t.Errorf("Expected (rational? 3.14) to be true but was: %v", result)
	}

	// Complex with zero imaginary part is rational
	scheme.Environment().Define(Symbol("c"), complex(5, 0))
	result = scheme.EvalGlobal(List(Symbol("rational?"), Symbol("c")))
	if result != true {
		t.Errorf("Expected (rational? 5+0i) to be true but was: %v", result)
	}

	// Complex with non-zero imaginary part is not rational
	scheme.Environment().Define(Symbol("c2"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("rational?"), Symbol("c2")))
	if result != false {
		t.Errorf("Expected (rational? 3+4i) to be false but was: %v", result)
	}
}

// TestExactP has been moved to testdata/scheme-tests.scm.

// TestInexactP has been moved to testdata/scheme-tests.scm.

// TestExactIntegerP has been moved to testdata/scheme-tests.scm.

func TestFiniteP(t *testing.T) {
	scheme := New()
	// Regular numbers are finite
	result := scheme.EvalGlobal(List(Symbol("finite?"), float64(42)))
	if result != true {
		t.Errorf("Expected (finite? 42) to be true but was: %v", result)
	}

	// Infinity is not finite
	scheme.Environment().Define(Symbol("inf"), math.Inf(1))
	result = scheme.EvalGlobal(List(Symbol("finite?"), Symbol("inf")))
	if result != false {
		t.Errorf("Expected (finite? +inf) to be false but was: %v", result)
	}

	// NaN is not finite
	scheme.Environment().Define(Symbol("nan"), math.NaN())
	result = scheme.EvalGlobal(List(Symbol("finite?"), Symbol("nan")))
	if result != false {
		t.Errorf("Expected (finite? nan) to be false but was: %v", result)
	}

	// Complex numbers can be finite
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("finite?"), Symbol("c")))
	if result != true {
		t.Errorf("Expected (finite? 3+4i) to be true but was: %v", result)
	}
}

func TestInfiniteP(t *testing.T) {
	scheme := New()
	// Regular numbers are not infinite
	result := scheme.EvalGlobal(List(Symbol("infinite?"), float64(42)))
	if result != false {
		t.Errorf("Expected (infinite? 42) to be false but was: %v", result)
	}

	// Positive infinity
	scheme.Environment().Define(Symbol("inf"), math.Inf(1))
	result = scheme.EvalGlobal(List(Symbol("infinite?"), Symbol("inf")))
	if result != true {
		t.Errorf("Expected (infinite? +inf) to be true but was: %v", result)
	}

	// Negative infinity
	scheme.Environment().Define(Symbol("ninf"), math.Inf(-1))
	result = scheme.EvalGlobal(List(Symbol("infinite?"), Symbol("ninf")))
	if result != true {
		t.Errorf("Expected (infinite? -inf) to be true but was: %v", result)
	}

	// Complex with infinite component
	scheme.Environment().Define(Symbol("cinf"), complex(math.Inf(1), 0))
	result = scheme.EvalGlobal(List(Symbol("infinite?"), Symbol("cinf")))
	if result != true {
		t.Errorf("Expected (infinite? inf+0i) to be true but was: %v", result)
	}
}

func TestNanP(t *testing.T) {
	scheme := New()
	// Regular numbers are not NaN
	result := scheme.EvalGlobal(List(Symbol("nan?"), float64(42)))
	if result != false {
		t.Errorf("Expected (nan? 42) to be false but was: %v", result)
	}

	// NaN is NaN
	scheme.Environment().Define(Symbol("nan"), math.NaN())
	result = scheme.EvalGlobal(List(Symbol("nan?"), Symbol("nan")))
	if result != true {
		t.Errorf("Expected (nan? nan) to be true but was: %v", result)
	}

	// Infinity is not NaN
	scheme.Environment().Define(Symbol("inf"), math.Inf(1))
	result = scheme.EvalGlobal(List(Symbol("nan?"), Symbol("inf")))
	if result != false {
		t.Errorf("Expected (nan? +inf) to be false but was: %v", result)
	}

	// Complex with NaN component
	scheme.Environment().Define(Symbol("cnan"), complex(math.NaN(), 0))
	result = scheme.EvalGlobal(List(Symbol("nan?"), Symbol("cnan")))
	if result != true {
		t.Errorf("Expected (nan? nan+0i) to be true but was: %v", result)
	}
}
