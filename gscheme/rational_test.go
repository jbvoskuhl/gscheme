package gscheme

import (
	"math/big"
	"testing"
)

// Helper to parse and evaluate Scheme code.
func evalRat(code string) interface{} {
	s := New()
	return evalScheme(s, code)
}

// --- Parsing Tests ---

func TestRatParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected interface{}
	}{
		{"3/4", NewRat(3, 4)},
		{"-1/3", NewRat(-1, 3)},
		{"0/1", int64(0)},   // simplifies to int64
		{"6/3", int64(2)},   // simplifies to int64
		{"4/2", int64(2)},   // simplifies to int64
		{"1/1", int64(1)},   // simplifies to int64
		{"-6/3", int64(-2)}, // simplifies to int64
	}
	for _, tt := range tests {
		input := NewInputPortFromString(tt.input)
		result := input.Read()
		switch expected := tt.expected.(type) {
		case int64:
			if result != expected {
				t.Errorf("Parsing %q: expected int64(%d), got %v (%T)", tt.input, expected, result, result)
			}
		case *big.Rat:
			r, ok := result.(*big.Rat)
			if !ok {
				t.Errorf("Parsing %q: expected *big.Rat, got %v (%T)", tt.input, result, result)
			} else if r.Cmp(expected) != 0 {
				t.Errorf("Parsing %q: expected %s, got %s", tt.input, expected.RatString(), r.RatString())
			}
		}
	}
}

func TestRatInvalidParsing(t *testing.T) {
	// Denominator 0 should not parse as rational
	input := NewInputPortFromString("1/0")
	result := input.Read()
	if _, ok := result.(*big.Rat); ok {
		t.Errorf("Parsing 1/0 should not produce a rational, got %v", result)
	}
	if _, ok := result.(int64); ok {
		t.Errorf("Parsing 1/0 should not produce an integer, got %v", result)
	}

	// Negative denominator should not parse
	input = NewInputPortFromString("1/-3")
	result = input.Read()
	if _, ok := result.(*big.Rat); ok {
		t.Errorf("Parsing 1/-3 should not produce a rational, got %v", result)
	}
}

// --- Stringify Tests ---

func TestRatStringify(t *testing.T) {
	tests := []struct {
		value    interface{}
		expected string
	}{
		{NewRat(3, 4), "3/4"},
		{NewRat(-1, 3), "-1/3"},
		{NewRat(7, 2), "7/2"},
	}
	for _, tt := range tests {
		result := Stringify(tt.value)
		if result != tt.expected {
			t.Errorf("Stringify(%v): expected %q, got %q", tt.value, tt.expected, result)
		}
	}
}

// --- Arithmetic Tests ---

func TestRatArithmetic(t *testing.T) {
	tests := []struct {
		code     string
		expected string
	}{
		// Basic rational arithmetic
		{"(+ 1/3 1/6)", "1/2"},
		{"(* 2/3 3/4)", "1/2"},
		{"(- 3/4 1/4)", "1/2"},
		{"(/ 1 3)", "1/3"},
		{"(/ 2 4)", "1/2"},

		// Simplification to integer
		{"(+ 1/2 1/2)", "1"},
		{"(* 1/3 3)", "1"},
		{"(- 3/2 1/2)", "1"},

		// Mixed int64 and rat
		{"(+ 1/3 1)", "4/3"},
		{"(* 2 3/4)", "3/2"},
		{"(- 1 1/3)", "2/3"},
		{"(/ 1 3)", "1/3"},

		// Unary minus and reciprocal
		{"(- 3/4)", "-3/4"},
		{"(/ 3/4)", "4/3"},
		{"(/ 2)", "1/2"},

		// Multiple arguments
		{"(+ 1/6 1/6 1/6)", "1/2"},
		{"(* 1/2 1/3 1/4)", "1/24"},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		str := Stringify(result)
		if str != tt.expected {
			t.Errorf("%s: expected %s, got %s (%T)", tt.code, tt.expected, str, result)
		}
	}
}

func TestRatMixedWithFloat(t *testing.T) {
	s := New()
	// Mixing rat with float64 produces float64
	result := evalScheme(s, "(+ 1/3 0.5)")
	if _, ok := result.(float64); !ok {
		t.Errorf("(+ 1/3 0.5): expected float64, got %T: %v", result, result)
	}

	s = New()
	result = evalScheme(s, "(* 1/3 1.0)")
	if _, ok := result.(float64); !ok {
		t.Errorf("(* 1/3 1.0): expected float64, got %T: %v", result, result)
	}
}

// --- Comparison Tests ---

func TestRatComparisons(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		{"(= 1/2 2/4)", true},
		{"(= 1/2 1/3)", false},
		{"(< 1/3 1/2)", true},
		{"(< 1/2 1/3)", false},
		{"(> 1/2 1/3)", true},
		{"(<= 1/3 1/3)", true},
		{"(>= 1/2 1/3)", true},
		// Mixed exact comparisons
		{"(= 2/2 1)", true},
		{"(< 0 1/3)", true},
		{"(> 1 1/3)", true},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

// --- Predicate Tests ---

func TestRatPredicates(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		// Type predicates
		{"(number? 3/4)", true},
		{"(rational? 3/4)", true},
		{"(real? 3/4)", true},
		{"(complex? 3/4)", true},
		{"(integer? 3/4)", false},
		{"(integer? 4/2)", true},

		// Exactness
		{"(exact? 3/4)", true},
		{"(inexact? 3/4)", false},
		{"(exact-integer? 3/4)", false},
		{"(exact-integer? 4/2)", true},

		// Sign predicates
		{"(zero? 0/1)", true},
		{"(positive? 3/4)", true},
		{"(positive? -3/4)", false},
		{"(negative? -3/4)", true},
		{"(negative? 3/4)", false},

		// Finiteness
		{"(finite? 3/4)", true},
		{"(infinite? 3/4)", false},
		{"(nan? 3/4)", false},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

// --- Conversion Tests ---

func TestRatConversions(t *testing.T) {
	// inexact converts rational to float64
	s := New()
	result := evalScheme(s, "(inexact 1/3)")
	f, ok := result.(float64)
	if !ok {
		t.Fatalf("(inexact 1/3): expected float64, got %T: %v", result, result)
	}
	if f < 0.333 || f > 0.334 {
		t.Errorf("(inexact 1/3): expected ~0.333, got %v", f)
	}

	// exact converts float to rational
	s = New()
	result = evalScheme(s, "(exact 0.5)")
	str := Stringify(result)
	if str != "1/2" {
		t.Errorf("(exact 0.5): expected 1/2, got %s", str)
	}

	// exact on already-exact value is identity
	s = New()
	result = evalScheme(s, "(exact 3/4)")
	str = Stringify(result)
	if str != "3/4" {
		t.Errorf("(exact 3/4): expected 3/4, got %s", str)
	}
}

// --- Math Function Tests ---

func TestRatFloorCeiling(t *testing.T) {
	tests := []struct {
		code     string
		expected int64
	}{
		{"(floor 7/3)", 2},
		{"(floor -7/3)", -3},
		{"(floor 4/2)", 2},
		{"(ceiling 7/3)", 3},
		{"(ceiling -7/3)", -2},
		{"(ceiling 4/2)", 2},
		{"(truncate 7/3)", 2},
		{"(truncate -7/3)", -2},
		{"(round 7/4)", 2},   // 1.75 rounds to 2
		{"(round 3/2)", 2},   // 1.5 rounds to even (2)
		{"(round 5/2)", 2},   // 2.5 rounds to even (2)
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %d, got %v (%T)", tt.code, tt.expected, result, result)
		}
	}
}

func TestRatAbs(t *testing.T) {
	tests := []struct {
		code     string
		expected string
	}{
		{"(abs 3/4)", "3/4"},
		{"(abs -3/4)", "3/4"},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if Stringify(result) != tt.expected {
			t.Errorf("%s: expected %s, got %s", tt.code, tt.expected, Stringify(result))
		}
	}
}

func TestRatExpt(t *testing.T) {
	tests := []struct {
		code     string
		expected string
	}{
		{"(expt 2/3 3)", "8/27"},
		{"(expt 1/2 4)", "1/16"},
		{"(expt 3/2 0)", "1"},
		{"(expt 3/2 1)", "3/2"},
		{"(expt 2/3 2)", "4/9"},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if Stringify(result) != tt.expected {
			t.Errorf("%s: expected %s, got %s (%T)", tt.code, tt.expected, Stringify(result), result)
		}
	}
}

func TestRatSquare(t *testing.T) {
	result := evalRat("(square 2/3)")
	if Stringify(result) != "4/9" {
		t.Errorf("(square 2/3): expected 4/9, got %s", Stringify(result))
	}
}

func TestRatNumeratorDenominator(t *testing.T) {
	tests := []struct {
		code     string
		expected int64
	}{
		{"(numerator 3/4)", 3},
		{"(denominator 3/4)", 4},
		{"(numerator -1/3)", -1},
		{"(denominator -1/3)", 3},
		{"(numerator 5)", 5},
		{"(denominator 5)", 1},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %d, got %v (%T)", tt.code, tt.expected, result, result)
		}
	}
}

// --- Equality Tests ---

func TestRatEqvEqual(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		{"(eqv? 1/2 1/2)", true},
		{"(eqv? 1/2 2/4)", true},
		{"(eqv? 1/2 1/3)", false},
		{"(equal? 1/2 2/4)", true},
	}
	for _, tt := range tests {
		result := evalRat(tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

func TestRatIntegerDivision(t *testing.T) {
	// Key behavior change: (/ 1 3) now returns exact 1/3 instead of float64
	s := New()
	result := evalScheme(s, "(/ 1 3)")
	if _, ok := result.(*big.Rat); !ok {
		t.Errorf("(/ 1 3): expected *big.Rat, got %T: %v", result, result)
	}
	if Stringify(result) != "1/3" {
		t.Errorf("(/ 1 3): expected 1/3, got %s", Stringify(result))
	}

	// Evenly divisible should still return int64
	s = New()
	result = evalScheme(s, "(/ 6 3)")
	if result != int64(2) {
		t.Errorf("(/ 6 3): expected int64(2), got %v (%T)", result, result)
	}
}

func TestRatNumberToString(t *testing.T) {
	result := evalRat("(number->string 3/4)")
	if result != "3/4" {
		t.Errorf("(number->string 3/4): expected \"3/4\", got %v", result)
	}
}
