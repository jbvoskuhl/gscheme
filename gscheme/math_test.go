package gscheme

import "testing"

func TestArithmetic(t *testing.T) {
	scheme := New()
	expression := List(
		Symbol("+"),
		float64(2),
		List(Symbol("-"), float64(5), float64(3)),
		List(Symbol("/"), float64(4), float64(2)),
		List(Symbol("*"), float64(2), float64(1)))
	result := scheme.EvalGlobal(expression)
	if float64(8.0) != result {
		t.Errorf("Expected (+ 2 (- 5 3) (/ 4 2) (* 2 1)) to evaluate to 8 but was: %v.", result)
	}
}

func TestNumEqual(t *testing.T) {
	scheme := New()
	tests := []struct {
		args     []float64
		expected bool
	}{
		{[]float64{1, 1}, true},
		{[]float64{1, 2}, false},
		{[]float64{1, 1, 1}, true},
		{[]float64{1, 1, 2}, false},
		{[]float64{5, 5, 5, 5}, true},
	}
	for _, tt := range tests {
		args := make([]interface{}, len(tt.args)+1)
		args[0] = Symbol("=")
		for i, v := range tt.args {
			args[i+1] = v
		}
		result := scheme.EvalGlobal(List(args...))
		if result != tt.expected {
			t.Errorf("Expected (= %v) to be %v but was: %v", tt.args, tt.expected, result)
		}
	}
}

func TestNumLessThan(t *testing.T) {
	scheme := New()
	tests := []struct {
		args     []float64
		expected bool
	}{
		{[]float64{1, 2}, true},
		{[]float64{2, 1}, false},
		{[]float64{1, 1}, false},
		{[]float64{1, 2, 3}, true},
		{[]float64{1, 3, 2}, false},
	}
	for _, tt := range tests {
		args := make([]interface{}, len(tt.args)+1)
		args[0] = Symbol("<")
		for i, v := range tt.args {
			args[i+1] = v
		}
		result := scheme.EvalGlobal(List(args...))
		if result != tt.expected {
			t.Errorf("Expected (< %v) to be %v but was: %v", tt.args, tt.expected, result)
		}
	}
}

func TestNumGreaterThan(t *testing.T) {
	scheme := New()
	tests := []struct {
		args     []float64
		expected bool
	}{
		{[]float64{2, 1}, true},
		{[]float64{1, 2}, false},
		{[]float64{1, 1}, false},
		{[]float64{3, 2, 1}, true},
		{[]float64{3, 1, 2}, false},
	}
	for _, tt := range tests {
		args := make([]interface{}, len(tt.args)+1)
		args[0] = Symbol(">")
		for i, v := range tt.args {
			args[i+1] = v
		}
		result := scheme.EvalGlobal(List(args...))
		if result != tt.expected {
			t.Errorf("Expected (> %v) to be %v but was: %v", tt.args, tt.expected, result)
		}
	}
}

func TestNumLessThanOrEqual(t *testing.T) {
	scheme := New()
	tests := []struct {
		args     []float64
		expected bool
	}{
		{[]float64{1, 2}, true},
		{[]float64{1, 1}, true},
		{[]float64{2, 1}, false},
		{[]float64{1, 2, 2, 3}, true},
		{[]float64{1, 2, 3, 2}, false},
	}
	for _, tt := range tests {
		args := make([]interface{}, len(tt.args)+1)
		args[0] = Symbol("<=")
		for i, v := range tt.args {
			args[i+1] = v
		}
		result := scheme.EvalGlobal(List(args...))
		if result != tt.expected {
			t.Errorf("Expected (<= %v) to be %v but was: %v", tt.args, tt.expected, result)
		}
	}
}

func TestNumGreaterThanOrEqual(t *testing.T) {
	scheme := New()
	tests := []struct {
		args     []float64
		expected bool
	}{
		{[]float64{2, 1}, true},
		{[]float64{1, 1}, true},
		{[]float64{1, 2}, false},
		{[]float64{3, 2, 2, 1}, true},
		{[]float64{3, 2, 1, 2}, false},
	}
	for _, tt := range tests {
		args := make([]interface{}, len(tt.args)+1)
		args[0] = Symbol(">=")
		for i, v := range tt.args {
			args[i+1] = v
		}
		result := scheme.EvalGlobal(List(args...))
		if result != tt.expected {
			t.Errorf("Expected (>= %v) to be %v but was: %v", tt.args, tt.expected, result)
		}
	}
}

func TestComparisonWithIf(t *testing.T) {
	scheme := New()
	// (if (= 5 5) 1 2) should return 1
	result := scheme.EvalGlobal(List(Symbol("if"),
		List(Symbol("="), float64(5), float64(5)),
		float64(1), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (if (= 5 5) 1 2) to be 1 but was: %v", result)
	}
}

func TestFactorial(t *testing.T) {
	scheme := New()
	// (define (fact n) (if (= n 0) 1 (* n (fact (- n 1)))))
	scheme.EvalGlobal(List(Symbol("define"),
		List(Symbol("fact"), Symbol("n")),
		List(Symbol("if"),
			List(Symbol("="), Symbol("n"), float64(0)),
			float64(1),
			List(Symbol("*"), Symbol("n"),
				List(Symbol("fact"), List(Symbol("-"), Symbol("n"), float64(1)))))))
	// (fact 5) should return 120
	result := scheme.EvalGlobal(List(Symbol("fact"), float64(5)))
	if result != float64(120) {
		t.Errorf("Expected (fact 5) to be 120 but was: %v", result)
	}
}

func TestAbs(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected float64
	}{
		{-5, 5},
		{5, 5},
		{0, 0},
		{-3.14, 3.14},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("abs"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (abs %v) to be %v but was: %v", tt.input, tt.expected, result)
		}
	}
}

func TestMax(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("max"), float64(1), float64(3), float64(2)))
	if result != float64(3) {
		t.Errorf("Expected (max 1 3 2) to be 3 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("max"), float64(5)))
	if result != float64(5) {
		t.Errorf("Expected (max 5) to be 5 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("max"), float64(-1), float64(-5)))
	if result != float64(-1) {
		t.Errorf("Expected (max -1 -5) to be -1 but was: %v", result)
	}
}

func TestMin(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("min"), float64(1), float64(3), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (min 1 3 2) to be 1 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("min"), float64(5)))
	if result != float64(5) {
		t.Errorf("Expected (min 5) to be 5 but was: %v", result)
	}
}

func TestQuotient(t *testing.T) {
	scheme := New()
	tests := []struct {
		a, b     float64
		expected int64
	}{
		{7, 2, 3},
		{-7, 2, -3},
		{7, -2, -3},
		{-7, -2, 3},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("quotient"), tt.a, tt.b))
		if result != tt.expected {
			t.Errorf("Expected (quotient %v %v) to be %v but was: %v (%T)", tt.a, tt.b, tt.expected, result, result)
		}
	}
}

func TestRemainder(t *testing.T) {
	scheme := New()
	tests := []struct {
		a, b, expected float64
	}{
		{7, 2, 1},
		{-7, 2, -1},
		{7, -2, 1},
		{-7, -2, -1},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("remainder"), tt.a, tt.b))
		if result != tt.expected {
			t.Errorf("Expected (remainder %v %v) to be %v but was: %v", tt.a, tt.b, tt.expected, result)
		}
	}
}

func TestModulo(t *testing.T) {
	scheme := New()
	tests := []struct {
		a, b, expected float64
	}{
		{7, 2, 1},
		{-7, 2, 1},
		{7, -2, -1},
		{-7, -2, -1},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("modulo"), tt.a, tt.b))
		if result != tt.expected {
			t.Errorf("Expected (modulo %v %v) to be %v but was: %v", tt.a, tt.b, tt.expected, result)
		}
	}
}

func TestGcd(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("gcd"), float64(12), float64(8)))
	if result != float64(4) {
		t.Errorf("Expected (gcd 12 8) to be 4 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("gcd"), float64(12), float64(8), float64(6)))
	if result != float64(2) {
		t.Errorf("Expected (gcd 12 8 6) to be 2 but was: %v", result)
	}
	// (gcd) with no args returns 0
	result = scheme.EvalGlobal(List(Symbol("gcd")))
	if result != int64(0) {
		t.Errorf("Expected (gcd) to be 0 but was: %v", result)
	}
	// gcd with negative numbers
	result = scheme.EvalGlobal(List(Symbol("gcd"), float64(-12), float64(8)))
	if result != float64(4) {
		t.Errorf("Expected (gcd -12 8) to be 4 but was: %v", result)
	}
}

func TestLcm(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("lcm"), float64(4), float64(6)))
	if result != float64(12) {
		t.Errorf("Expected (lcm 4 6) to be 12 but was: %v", result)
	}
	// (lcm) with no args returns 1
	result = scheme.EvalGlobal(List(Symbol("lcm")))
	if result != int64(1) {
		t.Errorf("Expected (lcm) to be 1 but was: %v", result)
	}
	// lcm with zero
	result = scheme.EvalGlobal(List(Symbol("lcm"), float64(5), float64(0)))
	if result != float64(0) {
		t.Errorf("Expected (lcm 5 0) to be 0 but was: %v", result)
	}
}

func TestExpt(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("expt"), float64(2), float64(10)))
	if result != float64(1024) {
		t.Errorf("Expected (expt 2 10) to be 1024 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("expt"), float64(3), float64(0)))
	if result != float64(1) {
		t.Errorf("Expected (expt 3 0) to be 1 but was: %v", result)
	}
}

func TestSquare(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("square"), float64(5)))
	if result != float64(25) {
		t.Errorf("Expected (square 5) to be 25 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("square"), float64(-3)))
	if result != float64(9) {
		t.Errorf("Expected (square -3) to be 9 but was: %v", result)
	}
}

func TestFloor(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected int64
	}{
		{3.7, 3},
		{-3.7, -4},
		{3.0, 3},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("floor"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (floor %v) to be %v but was: %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestCeiling(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected int64
	}{
		{3.1, 4},
		{-3.1, -3},
		{3.0, 3},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("ceiling"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (ceiling %v) to be %v but was: %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestRound(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected int64
	}{
		{3.5, 4},   // rounds to even
		{4.5, 4},   // rounds to even
		{3.7, 4},
		{3.2, 3},
		{-3.5, -4}, // rounds to even
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("round"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (round %v) to be %v but was: %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestTruncate(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected int64
	}{
		{3.7, 3},
		{-3.7, -3},
		{3.0, 3},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("truncate"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (truncate %v) to be %v but was: %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestFloorQuotient(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("floor-quotient"), float64(7), float64(2)))
	if result != int64(3) {
		t.Errorf("Expected (floor-quotient 7 2) to be 3 but was: %v (%T)", result, result)
	}
	result = scheme.EvalGlobal(List(Symbol("floor-quotient"), float64(-7), float64(2)))
	if result != int64(-4) {
		t.Errorf("Expected (floor-quotient -7 2) to be -4 but was: %v (%T)", result, result)
	}
}

func TestFloorRemainder(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(List(Symbol("floor-remainder"), float64(7), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (floor-remainder 7 2) to be 1 but was: %v", result)
	}
	result = scheme.EvalGlobal(List(Symbol("floor-remainder"), float64(-7), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (floor-remainder -7 2) to be 1 but was: %v", result)
	}
}

func TestExactInexact(t *testing.T) {
	scheme := New()
	// exact converts float64 to int64 (truncates)
	result := scheme.EvalGlobal(List(Symbol("exact"), float64(3.5)))
	if result != int64(3) {
		t.Errorf("Expected (exact 3.5) to be 3 but was: %v (%T)", result, result)
	}
	// inexact on float64 stays float64
	result = scheme.EvalGlobal(List(Symbol("inexact"), float64(3)))
	if result != float64(3) {
		t.Errorf("Expected (inexact 3) to be 3.0 but was: %v (%T)", result, result)
	}
}

func TestNumerator(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected float64
	}{
		{5, 5},
		{0, 0},
		{-5, -5},
		{0.5, 1},
		{-0.5, -1},
		{0.75, 3},
		{1.5, 3},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("numerator"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (numerator %v) to be %v but was: %v", tt.input, tt.expected, result)
		}
	}
}

func TestDenominator(t *testing.T) {
	scheme := New()
	tests := []struct {
		input    float64
		expected float64
	}{
		{5, 1},
		{0, 1},
		{-5, 1},
		{0.5, 2},
		{-0.5, 2},
		{0.75, 4},
		{1.5, 2},
	}
	for _, tt := range tests {
		result := scheme.EvalGlobal(List(Symbol("denominator"), tt.input))
		if result != tt.expected {
			t.Errorf("Expected (denominator %v) to be %v but was: %v", tt.input, tt.expected, result)
		}
	}
}
