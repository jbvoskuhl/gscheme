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
