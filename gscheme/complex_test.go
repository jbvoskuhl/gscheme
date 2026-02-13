package gscheme

import (
	"math"
	"testing"
)

func TestParseComplexNumbers(t *testing.T) {
	tests := []struct {
		input    string
		expected interface{}
	}{
		{"3+4i", complex(3, 4)},
		{"3-4i", complex(3, -4)},
		{"+5i", complex(0, 5)},
		{"-5i", complex(0, -5)},
		{"+i", complex(0, 1)},
		{"-i", complex(0, -1)},
		{"3.5+2.5i", complex(3.5, 2.5)},
		{"0+1i", complex(0, 1)},
		{"1+0i", complex(1, 0)},
	}

	for _, tt := range tests {
		port := NewInputPortFromString(tt.input)
		result := port.Read()
		if result != tt.expected {
			t.Errorf("parseNumber(%q) = %v, want %v", tt.input, result, tt.expected)
		}
	}
}

func TestBareIParsesAsSymbol(t *testing.T) {
	// Per R7RS, bare "i" is a valid identifier (symbol), not a complex number.
	port := NewInputPortFromString("i")
	result := port.Read()
	if result != Symbol("i") {
		t.Errorf("Read(\"i\") = %v (%T), want symbol i", result, result)
	}
}

func TestDigitPrefixedNonNumberIsError(t *testing.T) {
	// Per R7RS, tokens starting with a digit that aren't valid numbers are read errors.
	inputs := []string{"1i", "5i", "1abc", "3x"}
	for _, input := range inputs {
		port := NewInputPortFromString(input)
		result := port.Read()
		if _, ok := result.(Error); !ok {
			t.Errorf("Read(%q) = %v (%T), want Error", input, result, result)
		}
	}
}

func TestStringifyComplex(t *testing.T) {
	tests := []struct {
		input    complex128
		expected string
	}{
		{complex(3, 4), "3+4i"},
		{complex(3, -4), "3-4i"},
		{complex(0, 5), "+5i"},
		{complex(0, -5), "-5i"},
		{complex(0, 1), "+i"},
		{complex(0, -1), "-i"},
		{complex(5, 0), "5"},
		{complex(3, 1), "3+i"},
		{complex(3, -1), "3-i"},
	}

	for _, tt := range tests {
		result := Stringify(tt.input)
		if result != tt.expected {
			t.Errorf("Stringify(%v) = %q, want %q", tt.input, result, tt.expected)
		}
	}
}

func TestComplexP(t *testing.T) {
	scheme := New()

	// Real numbers are complex
	result := scheme.EvalGlobal(List(Symbol("complex?"), float64(42)))
	if result != true {
		t.Errorf("Expected (complex? 42) to be true but was: %v", result)
	}

	// Complex numbers are complex
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("complex?"), Symbol("c")))
	if result != true {
		t.Errorf("Expected (complex? 3+4i) to be true but was: %v", result)
	}

	// Non-numbers are not complex
	result = scheme.EvalGlobal(List(Symbol("complex?"), Symbol("x")))
	if result != false {
		t.Errorf("Expected (complex? 'x) to be false but was: %v", result)
	}
}

func TestRealP(t *testing.T) {
	scheme := New()

	// Real numbers are real
	result := scheme.EvalGlobal(List(Symbol("real?"), float64(42)))
	if result != true {
		t.Errorf("Expected (real? 42) to be true but was: %v", result)
	}

	// Complex with zero imaginary part is real
	scheme.Environment().Define(Symbol("c"), complex(3, 0))
	result = scheme.EvalGlobal(List(Symbol("real?"), Symbol("c")))
	if result != true {
		t.Errorf("Expected (real? 3+0i) to be true but was: %v", result)
	}

	// Complex with non-zero imaginary part is not real
	scheme.Environment().Define(Symbol("c2"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("real?"), Symbol("c2")))
	if result != false {
		t.Errorf("Expected (real? 3+4i) to be false but was: %v", result)
	}
}

func TestMakeRectangular(t *testing.T) {
	scheme := New()

	// (make-rectangular 3 4) => 3+4i
	result := scheme.EvalGlobal(List(Symbol("make-rectangular"), float64(3), float64(4)))
	if result != complex(3, 4) {
		t.Errorf("Expected (make-rectangular 3 4) to be 3+4i but was: %v", result)
	}

	// (make-rectangular 5 0) => 5 (simplified to real)
	result = scheme.EvalGlobal(List(Symbol("make-rectangular"), float64(5), float64(0)))
	if result != float64(5) {
		t.Errorf("Expected (make-rectangular 5 0) to be 5 but was: %v", result)
	}
}

func TestMakePolar(t *testing.T) {
	scheme := New()

	// (make-polar 1 0) => 1
	result := scheme.EvalGlobal(List(Symbol("make-polar"), float64(1), float64(0)))
	if result != float64(1) {
		t.Errorf("Expected (make-polar 1 0) to be 1 but was: %v", result)
	}

	// (make-polar 1 pi/2) => approximately i
	result = scheme.EvalGlobal(List(Symbol("make-polar"), float64(1), float64(math.Pi/2)))
	c, ok := result.(complex128)
	if !ok {
		t.Errorf("Expected complex result from (make-polar 1 pi/2) but got: %v", result)
	} else {
		if math.Abs(real(c)) > 1e-10 || math.Abs(imag(c)-1) > 1e-10 {
			t.Errorf("Expected (make-polar 1 pi/2) to be approximately i but was: %v", result)
		}
	}
}

func TestRealPart(t *testing.T) {
	scheme := New()

	// Real number
	result := scheme.EvalGlobal(List(Symbol("real-part"), float64(42)))
	if result != float64(42) {
		t.Errorf("Expected (real-part 42) to be 42 but was: %v", result)
	}

	// Complex number
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("real-part"), Symbol("c")))
	if result != float64(3) {
		t.Errorf("Expected (real-part 3+4i) to be 3 but was: %v", result)
	}
}

func TestImagPart(t *testing.T) {
	scheme := New()

	// Real number has zero imaginary part
	result := scheme.EvalGlobal(List(Symbol("imag-part"), float64(42)))
	if result != float64(0) {
		t.Errorf("Expected (imag-part 42) to be 0 but was: %v", result)
	}

	// Complex number
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("imag-part"), Symbol("c")))
	if result != float64(4) {
		t.Errorf("Expected (imag-part 3+4i) to be 4 but was: %v", result)
	}
}

func TestMagnitude(t *testing.T) {
	scheme := New()

	// Real number
	result := scheme.EvalGlobal(List(Symbol("magnitude"), float64(-5)))
	if result != float64(5) {
		t.Errorf("Expected (magnitude -5) to be 5 but was: %v", result)
	}

	// Complex number: |3+4i| = 5
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result = scheme.EvalGlobal(List(Symbol("magnitude"), Symbol("c")))
	if result != float64(5) {
		t.Errorf("Expected (magnitude 3+4i) to be 5 but was: %v", result)
	}
}

func TestAngle(t *testing.T) {
	scheme := New()

	// Positive real has angle 0
	result := scheme.EvalGlobal(List(Symbol("angle"), float64(5)))
	if result != float64(0) {
		t.Errorf("Expected (angle 5) to be 0 but was: %v", result)
	}

	// Negative real has angle pi
	result = scheme.EvalGlobal(List(Symbol("angle"), float64(-5)))
	if result != math.Pi {
		t.Errorf("Expected (angle -5) to be pi but was: %v", result)
	}

	// i has angle pi/2
	scheme.Environment().Define(Symbol("c"), complex(0, 1))
	result = scheme.EvalGlobal(List(Symbol("angle"), Symbol("c")))
	f, ok := result.(float64)
	if !ok || math.Abs(f-math.Pi/2) > 1e-10 {
		t.Errorf("Expected (angle i) to be pi/2 but was: %v", result)
	}
}

func TestComplexArithmetic(t *testing.T) {
	scheme := New()

	// Addition: (+ 1+2i 3+4i) => 4+6i
	scheme.Environment().Define(Symbol("a"), complex(1, 2))
	scheme.Environment().Define(Symbol("b"), complex(3, 4))
	result := scheme.EvalGlobal(List(Symbol("+"), Symbol("a"), Symbol("b")))
	if result != complex(4, 6) {
		t.Errorf("Expected (+ 1+2i 3+4i) to be 4+6i but was: %v", result)
	}

	// Subtraction
	result = scheme.EvalGlobal(List(Symbol("-"), Symbol("b"), Symbol("a")))
	if result != complex(2, 2) {
		t.Errorf("Expected (- 3+4i 1+2i) to be 2+2i but was: %v", result)
	}

	// Multiplication: (1+2i)(3+4i) = 3+4i+6i+8i² = 3+10i-8 = -5+10i
	result = scheme.EvalGlobal(List(Symbol("*"), Symbol("a"), Symbol("b")))
	if result != complex(-5, 10) {
		t.Errorf("Expected (* 1+2i 3+4i) to be -5+10i but was: %v", result)
	}

	// Division
	result = scheme.EvalGlobal(List(Symbol("/"), Symbol("b"), Symbol("a")))
	c, ok := result.(complex128)
	if !ok {
		t.Errorf("Expected complex result from division but got: %v", result)
	} else {
		// (3+4i)/(1+2i) = (3+4i)(1-2i)/((1+2i)(1-2i)) = (3-6i+4i+8)/(1+4) = (11-2i)/5 = 2.2-0.4i
		expected := complex(2.2, -0.4)
		if math.Abs(real(c)-real(expected)) > 1e-10 || math.Abs(imag(c)-imag(expected)) > 1e-10 {
			t.Errorf("Expected (/ 3+4i 1+2i) to be approximately 2.2-0.4i but was: %v", result)
		}
	}
}

func TestMixedArithmetic(t *testing.T) {
	scheme := New()

	// Real + Complex
	scheme.Environment().Define(Symbol("c"), complex(3, 4))
	result := scheme.EvalGlobal(List(Symbol("+"), float64(2), Symbol("c")))
	if result != complex(5, 4) {
		t.Errorf("Expected (+ 2 3+4i) to be 5+4i but was: %v", result)
	}

	// Complex * Real
	result = scheme.EvalGlobal(List(Symbol("*"), Symbol("c"), float64(2)))
	if result != complex(6, 8) {
		t.Errorf("Expected (* 3+4i 2) to be 6+8i but was: %v", result)
	}
}

func TestSqrt(t *testing.T) {
	scheme := New()

	// Positive real
	result := scheme.EvalGlobal(List(Symbol("sqrt"), float64(4)))
	if result != float64(2) {
		t.Errorf("Expected (sqrt 4) to be 2 but was: %v", result)
	}

	// Negative real returns complex
	result = scheme.EvalGlobal(List(Symbol("sqrt"), float64(-1)))
	if result != complex(0, 1) {
		t.Errorf("Expected (sqrt -1) to be i but was: %v", result)
	}

	// Complex number
	scheme.Environment().Define(Symbol("c"), complex(0, 1))
	result = scheme.EvalGlobal(List(Symbol("sqrt"), Symbol("c")))
	c, ok := result.(complex128)
	if !ok {
		t.Errorf("Expected complex result from (sqrt i) but got: %v", result)
	} else {
		// sqrt(i) = (1+i)/sqrt(2)
		expected := complex(math.Sqrt(2)/2, math.Sqrt(2)/2)
		if math.Abs(real(c)-real(expected)) > 1e-10 || math.Abs(imag(c)-imag(expected)) > 1e-10 {
			t.Errorf("Expected (sqrt i) to be approximately (1+i)/sqrt(2) but was: %v", result)
		}
	}
}

func TestTrigFunctions(t *testing.T) {
	scheme := New()

	// sin(0) = 0
	result := scheme.EvalGlobal(List(Symbol("sin"), float64(0)))
	if result != float64(0) {
		t.Errorf("Expected (sin 0) to be 0 but was: %v", result)
	}

	// cos(0) = 1
	result = scheme.EvalGlobal(List(Symbol("cos"), float64(0)))
	if result != float64(1) {
		t.Errorf("Expected (cos 0) to be 1 but was: %v", result)
	}

	// tan(0) = 0
	result = scheme.EvalGlobal(List(Symbol("tan"), float64(0)))
	if result != float64(0) {
		t.Errorf("Expected (tan 0) to be 0 but was: %v", result)
	}

	// atan(1) = pi/4
	result = scheme.EvalGlobal(List(Symbol("atan"), float64(1)))
	f, ok := result.(float64)
	if !ok || math.Abs(f-math.Pi/4) > 1e-10 {
		t.Errorf("Expected (atan 1) to be pi/4 but was: %v", result)
	}

	// atan2(1, 1) = pi/4
	result = scheme.EvalGlobal(List(Symbol("atan"), float64(1), float64(1)))
	f, ok = result.(float64)
	if !ok || math.Abs(f-math.Pi/4) > 1e-10 {
		t.Errorf("Expected (atan 1 1) to be pi/4 but was: %v", result)
	}
}

func TestExpLog(t *testing.T) {
	scheme := New()

	// exp(0) = 1
	result := scheme.EvalGlobal(List(Symbol("exp"), float64(0)))
	if result != float64(1) {
		t.Errorf("Expected (exp 0) to be 1 but was: %v", result)
	}

	// log(1) = 0
	result = scheme.EvalGlobal(List(Symbol("log"), float64(1)))
	if result != float64(0) {
		t.Errorf("Expected (log 1) to be 0 but was: %v", result)
	}

	// log(e) = 1
	result = scheme.EvalGlobal(List(Symbol("log"), float64(math.E)))
	f, ok := result.(float64)
	if !ok || math.Abs(f-1) > 1e-10 {
		t.Errorf("Expected (log e) to be 1 but was: %v", result)
	}
}
