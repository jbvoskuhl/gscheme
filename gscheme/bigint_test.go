package gscheme

import (
	"math"
	"math/big"
	"testing"
)

// --- Overflow Promotion Tests ---

func TestBigIntOverflowAddition(t *testing.T) {
	s := New()
	// Adding two large int64 values that don't overflow stays int64
	result := evalScheme(s, "(+ 4611686018427387903 4611686018427387903)")
	if result != int64(9223372036854775806) {
		t.Errorf("no-overflow addition: expected int64(9223372036854775806), got %v (%T)", result, result)
	}

	// Max int64 + 1 overflows to *big.Int
	result = evalScheme(s, "(+ 9223372036854775807 1)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("max+1: expected *big.Int, got %T: %v", result, result)
	}
	if Stringify(result) != "9223372036854775808" {
		t.Errorf("max+1: expected 9223372036854775808, got %s", Stringify(result))
	}

	// Two values that actually overflow
	result = evalScheme(s, "(+ 4611686018427387904 4611686018427387904)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("overflow addition: expected *big.Int, got %T: %v", result, result)
	}
	if Stringify(result) != "9223372036854775808" {
		t.Errorf("overflow addition: expected 9223372036854775808, got %s", Stringify(result))
	}
}

func TestBigIntOverflowMultiplication(t *testing.T) {
	s := New()
	result := evalScheme(s, "(* 9999999999999999 9999999999999999)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("overflow multiplication: expected *big.Int, got %T: %v", result, result)
	}
	expected := "99999999999999980000000000000001"
	if Stringify(result) != expected {
		t.Errorf("overflow multiplication: expected %s, got %s", expected, Stringify(result))
	}
}

func TestBigIntOverflowSubtraction(t *testing.T) {
	s := New()
	// MinInt64 - 1 overflows
	result := evalScheme(s, "(- -9223372036854775808 1)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("min-1: expected *big.Int, got %T: %v", result, result)
	}
	if Stringify(result) != "-9223372036854775809" {
		t.Errorf("min-1: expected -9223372036854775809, got %s", Stringify(result))
	}
}

func TestBigIntOverflowNegation(t *testing.T) {
	s := New()
	// Negating MinInt64 overflows
	result := evalScheme(s, "(- 0 -9223372036854775808)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("negate MinInt64: expected *big.Int, got %T: %v", result, result)
	}
	if Stringify(result) != "9223372036854775808" {
		t.Errorf("negate MinInt64: expected 9223372036854775808, got %s", Stringify(result))
	}
}

func TestBigIntExpt(t *testing.T) {
	result := evalScheme(New(),"(expt 2 100)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("(expt 2 100): expected *big.Int, got %T: %v", result, result)
	}
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("(expt 2 100): expected %s, got %s", expected, Stringify(result))
	}
}

func TestBigIntExptSmall(t *testing.T) {
	// Small expt should stay int64
	result := evalScheme(New(),"(expt 2 10)")
	if result != int64(1024) {
		t.Errorf("(expt 2 10): expected int64(1024), got %v (%T)", result, result)
	}
}

// --- Simplification Tests ---

func TestBigIntSimplification(t *testing.T) {
	// Multiplying by 0 should give int64(0)
	result := evalScheme(New(),"(* (expt 2 100) 0)")
	if result != int64(0) {
		t.Errorf("big*0: expected int64(0), got %v (%T)", result, result)
	}

	// Subtracting equal big values should give int64(0)
	s := New()
	evalScheme(s, "(define x (expt 2 100))")
	result = evalScheme(s, "(- x x)")
	if result != int64(0) {
		t.Errorf("x-x: expected int64(0), got %v (%T)", result, result)
	}

	// Adding 0 to a small number stays int64
	result = evalScheme(New(),"(+ 42 0)")
	if result != int64(42) {
		t.Errorf("42+0: expected int64(42), got %v (%T)", result, result)
	}
}

// --- Parsing Tests ---

func TestBigIntParsing(t *testing.T) {
	// Parse a number too large for int64
	input := NewInputPortFromString("99999999999999999999")
	result := input.Read()
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("Parsing large int: expected *big.Int, got %v (%T)", result, result)
	}
	if Stringify(result) != "99999999999999999999" {
		t.Errorf("Parsing large int: expected 99999999999999999999, got %s", Stringify(result))
	}

	// Parse a number that fits in int64
	input = NewInputPortFromString("42")
	result = input.Read()
	if result != int64(42) {
		t.Errorf("Parsing small int: expected int64(42), got %v (%T)", result, result)
	}

	// Parse negative large number
	input = NewInputPortFromString("-99999999999999999999")
	result = input.Read()
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("Parsing large negative int: expected *big.Int, got %v (%T)", result, result)
	}
}

// --- Stringify Tests ---

func TestBigIntStringify(t *testing.T) {
	result := evalScheme(New(),"(expt 2 100)")
	str := Stringify(result)
	expected := "1267650600228229401496703205376"
	if str != expected {
		t.Errorf("Stringify big: expected %s, got %s", expected, str)
	}
}

// --- Predicate Tests ---

func TestBigIntPredicates(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		{"(number? (expt 2 100))", true},
		{"(integer? (expt 2 100))", true},
		{"(rational? (expt 2 100))", true},
		{"(real? (expt 2 100))", true},
		{"(complex? (expt 2 100))", true},
		{"(exact? (expt 2 100))", true},
		{"(inexact? (expt 2 100))", false},
		{"(exact-integer? (expt 2 100))", true},
		{"(finite? (expt 2 100))", true},
		{"(infinite? (expt 2 100))", false},
		{"(nan? (expt 2 100))", false},
		{"(positive? (expt 2 100))", true},
		{"(negative? (expt 2 100))", false},
		{"(zero? (expt 2 100))", false},
		{"(even? (expt 2 100))", true},
		{"(odd? (expt 2 100))", false},
		{"(odd? (+ (expt 2 100) 1))", true},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

// --- Comparison Tests ---

func TestBigIntComparisons(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		{"(= (expt 2 64) (expt 2 64))", true},
		{"(= (expt 2 64) (expt 2 63))", false},
		{"(< (expt 2 63) (expt 2 64))", true},
		{"(> (expt 2 64) (expt 2 63))", true},
		{"(<= (expt 2 64) (expt 2 64))", true},
		{"(>= (expt 2 64) (expt 2 64))", true},
		// Cross-type: big.Int with int64
		{"(> (expt 2 100) 1000)", true},
		{"(< 1000 (expt 2 100))", true},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

// --- Mixed Arithmetic Tests ---

func TestBigIntMixedArithmetic(t *testing.T) {
	// big.Int + int64
	result := evalScheme(New(),"(+ (expt 2 100) 1)")
	expected := "1267650600228229401496703205377"
	if Stringify(result) != expected {
		t.Errorf("big+1: expected %s, got %s", expected, Stringify(result))
	}

	// big.Int * int64
	result = evalScheme(New(),"(* (expt 2 100) 2)")
	expected = "2535301200456458802993406410752"
	if Stringify(result) != expected {
		t.Errorf("big*2: expected %s, got %s", expected, Stringify(result))
	}

	// big.Int with float64 produces float64
	result = evalScheme(New(),"(+ (expt 2 100) 0.5)")
	if _, ok := result.(float64); !ok {
		t.Errorf("big+0.5: expected float64, got %T", result)
	}

	// big.Int with rational
	result = evalScheme(New(),"(+ (expt 2 100) 1/2)")
	str := Stringify(result)
	if _, ok := result.(*big.Rat); !ok {
		t.Errorf("big+1/2: expected *big.Rat, got %T: %v", result, result)
	}
	_ = str
}

// --- Division Tests ---

func TestBigIntDivision(t *testing.T) {
	// Exact division of big integers
	s := New()
	evalScheme(s, "(define x (expt 2 100))")
	evalScheme(s, "(define y (expt 2 50))")
	result := evalScheme(s, "(/ x y)")
	expected := evalScheme(s, "(expt 2 50)")
	if Stringify(result) != Stringify(expected) {
		t.Errorf("2^100/2^50: expected %s, got %s", Stringify(expected), Stringify(result))
	}

	// Non-exact division of big integers gives rational
	result = evalScheme(New(),"(/ (+ (expt 2 100) 1) 2)")
	if _, ok := result.(*big.Rat); !ok {
		t.Errorf("(2^100+1)/2: expected *big.Rat, got %T: %v", result, result)
	}
}

// --- Math Function Tests ---

func TestBigIntAbs(t *testing.T) {
	result := evalScheme(New(),"(abs (- 0 (expt 2 100)))")
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("abs(-2^100): expected %s, got %s", expected, Stringify(result))
	}

	// abs(MinInt64) should produce *big.Int since -MinInt64 overflows
	result = evalScheme(New(),"(abs -9223372036854775808)")
	if Stringify(result) != "9223372036854775808" {
		t.Errorf("abs(MinInt64): expected 9223372036854775808, got %s", Stringify(result))
	}
}

func TestBigIntSquare(t *testing.T) {
	result := evalScheme(New(),"(square (expt 2 50))")
	expected := evalScheme(New(),"(expt 2 100)")
	if Stringify(result) != Stringify(expected) {
		t.Errorf("square(2^50): expected %s, got %s", Stringify(expected), Stringify(result))
	}
}

func TestBigIntFloorCeilingTruncateRound(t *testing.T) {
	// big.Int is already an integer, so these should return it unchanged
	tests := []struct {
		code     string
		expected string
	}{
		{"(floor (expt 2 100))", "1267650600228229401496703205376"},
		{"(ceiling (expt 2 100))", "1267650600228229401496703205376"},
		{"(truncate (expt 2 100))", "1267650600228229401496703205376"},
		{"(round (expt 2 100))", "1267650600228229401496703205376"},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if Stringify(result) != tt.expected {
			t.Errorf("%s: expected %s, got %s", tt.code, tt.expected, Stringify(result))
		}
	}
}

func TestBigIntQuotientRemainderModulo(t *testing.T) {
	tests := []struct {
		code     string
		expected string
	}{
		{"(quotient (expt 2 100) (expt 2 50))", "1125899906842624"},
		{"(remainder (expt 2 100) 3)", "1"},
		{"(modulo (expt 2 100) 3)", "1"},
		{"(floor-quotient (expt 2 100) 3)", "422550200076076467165567735125"},
		{"(floor-remainder (expt 2 100) 3)", "1"},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if Stringify(result) != tt.expected {
			t.Errorf("%s: expected %s, got %s", tt.code, tt.expected, Stringify(result))
		}
	}
}

func TestBigIntGcdLcm(t *testing.T) {
	tests := []struct {
		code     string
		expected string
	}{
		{"(gcd (expt 2 100) (expt 2 50))", "1125899906842624"},
		{"(lcm 12 18)", "36"},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if Stringify(result) != tt.expected {
			t.Errorf("%s: expected %s, got %s", tt.code, tt.expected, Stringify(result))
		}
	}
}

func TestBigIntNumeratorDenominator(t *testing.T) {
	result := evalScheme(New(),"(numerator (expt 2 100))")
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("numerator(2^100): expected %s, got %s", expected, Stringify(result))
	}

	result = evalScheme(New(),"(denominator (expt 2 100))")
	if result != int64(1) {
		t.Errorf("denominator(2^100): expected 1, got %v (%T)", result, result)
	}
}

func TestBigIntExactInexact(t *testing.T) {
	// exact->inexact on big.Int gives float64
	result := evalScheme(New(),"(inexact (expt 2 100))")
	if _, ok := result.(float64); !ok {
		t.Errorf("(inexact (expt 2 100)): expected float64, got %T", result)
	}
	f := result.(float64)
	expected := math.Pow(2, 100)
	if f != expected {
		t.Errorf("(inexact (expt 2 100)): expected %v, got %v", expected, f)
	}

	// inexact->exact on big.Int returns itself
	result = evalScheme(New(),"(exact (expt 2 100))")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("(exact (expt 2 100)): expected *big.Int, got %T", result)
	}
}

// --- Eqv and Equal Tests ---

func TestBigIntEqvEqual(t *testing.T) {
	tests := []struct {
		code     string
		expected bool
	}{
		{"(eqv? (expt 2 100) (expt 2 100))", true},
		{"(eqv? (expt 2 100) (expt 2 99))", false},
		{"(equal? (expt 2 100) (expt 2 100))", true},
		{"(equal? (expt 2 100) (expt 2 99))", false},
	}
	for _, tt := range tests {
		result := evalScheme(New(),tt.code)
		if result != tt.expected {
			t.Errorf("%s: expected %v, got %v", tt.code, tt.expected, result)
		}
	}
}

// --- Number->String and String->Number Tests ---

func TestBigIntNumberToString(t *testing.T) {
	result := evalScheme(New(),"(number->string (expt 2 100))")
	expected := "1267650600228229401496703205376"
	if result != expected {
		t.Errorf("(number->string (expt 2 100)): expected %s, got %v", expected, result)
	}

	// With radix
	result = evalScheme(New(),"(number->string (expt 2 100) 16)")
	expected = "10000000000000000000000000"
	if result != expected {
		t.Errorf("(number->string (expt 2 100) 16): expected %s, got %v", expected, result)
	}
}

func TestBigIntStringToNumber(t *testing.T) {
	result := evalScheme(New(),"(string->number \"99999999999999999999\")")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("string->number large: expected *big.Int, got %T: %v", result, result)
	}
	if Stringify(result) != "99999999999999999999" {
		t.Errorf("string->number large: expected 99999999999999999999, got %s", Stringify(result))
	}

	// With non-decimal radix
	result = evalScheme(New(),"(string->number \"10000000000000000000000000\" 16)")
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("string->number hex large: expected *big.Int, got %T: %v", result, result)
	}
	expected := evalScheme(New(),"(expt 2 100)")
	if Stringify(result) != Stringify(expected) {
		t.Errorf("string->number hex: expected %s, got %s", Stringify(expected), Stringify(result))
	}
}

// --- Complex Number Interaction Tests ---

func TestBigIntComplexInteraction(t *testing.T) {
	// real-part of big.Int returns itself
	result := evalScheme(New(),"(real-part (expt 2 100))")
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("real-part(2^100): expected %s, got %s", expected, Stringify(result))
	}

	// imag-part of big.Int returns 0
	result = evalScheme(New(),"(imag-part (expt 2 100))")
	if result != int64(0) {
		t.Errorf("imag-part(2^100): expected 0, got %v", result)
	}
}

// --- Overflow-Checked Arithmetic Unit Tests ---

func TestAddInt64Overflow(t *testing.T) {
	// No overflow
	result := addInt64(1, 2)
	if result != int64(3) {
		t.Errorf("addInt64(1,2): expected int64(3), got %v (%T)", result, result)
	}

	// Positive overflow
	result = addInt64(math.MaxInt64, 1)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("addInt64(MaxInt64,1): expected *big.Int, got %T", result)
	}

	// Negative overflow
	result = addInt64(math.MinInt64, -1)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("addInt64(MinInt64,-1): expected *big.Int, got %T", result)
	}
}

func TestSubInt64Overflow(t *testing.T) {
	// No overflow
	result := subInt64(5, 3)
	if result != int64(2) {
		t.Errorf("subInt64(5,3): expected int64(2), got %v (%T)", result, result)
	}

	// Overflow
	result = subInt64(math.MinInt64, 1)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("subInt64(MinInt64,1): expected *big.Int, got %T", result)
	}
}

func TestMulInt64Overflow(t *testing.T) {
	// No overflow
	result := mulInt64(3, 4)
	if result != int64(12) {
		t.Errorf("mulInt64(3,4): expected int64(12), got %v (%T)", result, result)
	}

	// Overflow
	result = mulInt64(math.MaxInt64, 2)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("mulInt64(MaxInt64,2): expected *big.Int, got %T", result)
	}

	// MinInt64 * -1 overflow
	result = mulInt64(math.MinInt64, -1)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("mulInt64(MinInt64,-1): expected *big.Int, got %T", result)
	}

	// Zero
	result = mulInt64(math.MaxInt64, 0)
	if result != int64(0) {
		t.Errorf("mulInt64(MaxInt64,0): expected int64(0), got %v (%T)", result, result)
	}
}

func TestNegInt64Overflow(t *testing.T) {
	result := negInt64(5)
	if result != int64(-5) {
		t.Errorf("negInt64(5): expected int64(-5), got %v (%T)", result, result)
	}

	result = negInt64(math.MinInt64)
	if _, ok := result.(*big.Int); !ok {
		t.Errorf("negInt64(MinInt64): expected *big.Int, got %T", result)
	}
}

// --- Large Rational Parsing ---

func TestBigIntRationalParsing(t *testing.T) {
	// Parse a rational with a large numerator
	input := NewInputPortFromString("99999999999999999999/3")
	result := input.Read()
	expected := "33333333333333333333"
	if Stringify(result) != expected {
		t.Errorf("Parsing large rational: expected %s, got %s (%T)", expected, Stringify(result), result)
	}
}

// --- Max/Min Tests ---

func TestBigIntMaxMin(t *testing.T) {
	result := evalScheme(New(),"(max (expt 2 100) 42)")
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("(max (expt 2 100) 42): expected %s, got %s", expected, Stringify(result))
	}

	result = evalScheme(New(),"(min (expt 2 100) 42)")
	if result != int64(42) {
		t.Errorf("(min (expt 2 100) 42): expected 42, got %v (%T)", result, result)
	}
}

// --- Magnitude and Angle Tests ---

func TestBigIntMagnitudeAngle(t *testing.T) {
	result := evalScheme(New(),"(magnitude (- 0 (expt 2 100)))")
	expected := "1267650600228229401496703205376"
	if Stringify(result) != expected {
		t.Errorf("magnitude(-2^100): expected %s, got %s", expected, Stringify(result))
	}

	result = evalScheme(New(),"(angle (expt 2 100))")
	if result != float64(0) {
		t.Errorf("angle(2^100): expected 0, got %v", result)
	}

	result = evalScheme(New(),"(angle (- 0 (expt 2 100)))")
	if result != math.Pi {
		t.Errorf("angle(-2^100): expected pi, got %v", result)
	}
}
