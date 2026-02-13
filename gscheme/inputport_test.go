package gscheme

import (
	"testing"
)

func TestReadNumber(t *testing.T) {
	input := NewInputPortFromString("42")
	result := input.Read()
	if result != int64(42) {
		t.Errorf("Expected 42, got %v (%T)", result, result)
	}
}

func TestReadNegativeNumber(t *testing.T) {
	input := NewInputPortFromString("-3.14")
	result := input.Read()
	if result != float64(-3.14) {
		t.Errorf("Expected -3.14, got %v (%T)", result, result)
	}
}

func TestReadSymbol(t *testing.T) {
	input := NewInputPortFromString("hello")
	result := input.Read()
	if result != Symbol("hello") {
		t.Errorf("Expected symbol 'hello', got %v (%T)", result, result)
	}
}

func TestReadSymbolCaseInsensitive(t *testing.T) {
	input := NewInputPortFromString("HELLO")
	result := input.Read()
	if result != Symbol("hello") {
		t.Errorf("Expected symbol 'hello', got %v (%T)", result, result)
	}
}

func TestReadString(t *testing.T) {
	input := NewInputPortFromString(`"hello world"`)
	result := input.Read()
	if result != "hello world" {
		t.Errorf("Expected string 'hello world', got %v (%T)", result, result)
	}
}

func TestReadStringWithEscapes(t *testing.T) {
	input := NewInputPortFromString(`"hello\nworld"`)
	result := input.Read()
	if result != "hello\nworld" {
		t.Errorf("Expected string with newline, got %v (%T)", result, result)
	}
}

func TestReadTrue(t *testing.T) {
	input := NewInputPortFromString("#t")
	result := input.Read()
	if result != true {
		t.Errorf("Expected true, got %v (%T)", result, result)
	}
}

func TestReadFalse(t *testing.T) {
	input := NewInputPortFromString("#f")
	result := input.Read()
	if result != false {
		t.Errorf("Expected false, got %v (%T)", result, result)
	}
}

func TestReadCharacter(t *testing.T) {
	input := NewInputPortFromString(`#\a`)
	result := input.Read()
	if result != 'a' {
		t.Errorf("Expected character 'a', got %v (%T)", result, result)
	}
}

func TestReadCharacterSpace(t *testing.T) {
	input := NewInputPortFromString(`#\space`)
	result := input.Read()
	if result != ' ' {
		t.Errorf("Expected space character, got %v (%T)", result, result)
	}
}

func TestReadCharacterNewline(t *testing.T) {
	input := NewInputPortFromString(`#\newline`)
	result := input.Read()
	if result != '\n' {
		t.Errorf("Expected newline character, got %v (%T)", result, result)
	}
}

func TestReadEmptyList(t *testing.T) {
	input := NewInputPortFromString("()")
	result := input.Read()
	if result != nil {
		t.Errorf("Expected nil (empty list), got %v (%T)", result, result)
	}
}

func TestReadSimpleList(t *testing.T) {
	input := NewInputPortFromString("(1 2 3)")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != int64(1) {
		t.Errorf("Expected first element 1, got %v", First(pair))
	}
	if First(Rest(pair)) != int64(2) {
		t.Errorf("Expected second element 2, got %v", First(Rest(pair)))
	}
	if First(Rest(Rest(pair))) != int64(3) {
		t.Errorf("Expected third element 3, got %v", First(Rest(Rest(pair))))
	}
}

func TestReadNestedList(t *testing.T) {
	input := NewInputPortFromString("(+ (- 3 2) 1)")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != Symbol("+") {
		t.Errorf("Expected first element '+', got %v", First(pair))
	}
	nested := First(Rest(pair))
	nestedPair, ok := nested.(Pair)
	if !ok {
		t.Fatalf("Expected nested Pair, got %v (%T)", nested, nested)
	}
	if First(nestedPair) != Symbol("-") {
		t.Errorf("Expected '-' in nested list, got %v", First(nestedPair))
	}
}

func TestReadQuote(t *testing.T) {
	input := NewInputPortFromString("'x")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != Symbol("quote") {
		t.Errorf("Expected 'quote', got %v", First(pair))
	}
	if First(Rest(pair)) != Symbol("x") {
		t.Errorf("Expected 'x', got %v", First(Rest(pair)))
	}
}

func TestReadQuasiquote(t *testing.T) {
	input := NewInputPortFromString("`x")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != Symbol("quasiquote") {
		t.Errorf("Expected 'quasiquote', got %v", First(pair))
	}
}

func TestReadUnquote(t *testing.T) {
	input := NewInputPortFromString(",x")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != Symbol("unquote") {
		t.Errorf("Expected 'unquote', got %v", First(pair))
	}
}

func TestReadUnquoteSplicing(t *testing.T) {
	input := NewInputPortFromString(",@x")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != Symbol("unquote-splicing") {
		t.Errorf("Expected 'unquote-splicing', got %v", First(pair))
	}
}

func TestReadDottedPair(t *testing.T) {
	input := NewInputPortFromString("(1 . 2)")
	result := input.Read()
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != int64(1) {
		t.Errorf("Expected first element 1, got %v", First(pair))
	}
	if Rest(pair) != int64(2) {
		t.Errorf("Expected rest element 2, got %v", Rest(pair))
	}
}

func TestReadComment(t *testing.T) {
	input := NewInputPortFromString("; this is a comment\n42")
	result := input.Read()
	if result != int64(42) {
		t.Errorf("Expected 42, got %v (%T)", result, result)
	}
}

func TestReadVector(t *testing.T) {
	input := NewInputPortFromString("#(1 2 3)")
	result := input.Read()
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected []interface{}, got %v (%T)", result, result)
	}
	if len(vec) != 3 {
		t.Errorf("Expected vector of length 3, got %d", len(vec))
	}
	if vec[0] != int64(1) {
		t.Errorf("Expected first element 1, got %v", vec[0])
	}
}

func TestReadMultipleExpressions(t *testing.T) {
	input := NewInputPortFromString("1 2 3")
	if input.Read() != int64(1) {
		t.Error("Expected 1")
	}
	if input.Read() != int64(2) {
		t.Error("Expected 2")
	}
	if input.Read() != int64(3) {
		t.Error("Expected 3")
	}
	if !IsEOF(input.Read()) {
		t.Error("Expected EOF")
	}
}

func TestReadEOF(t *testing.T) {
	input := NewInputPortFromString("")
	result := input.Read()
	if !IsEOF(result) {
		t.Errorf("Expected EOF, got %v (%T)", result, result)
	}
}

func TestLoadCodeWithArithmetic(t *testing.T) {
	s := New()
	// LoadCode should evaluate expressions - test with arithmetic that works
	input := NewInputPortFromString("(+ 1 2)")
	expr := input.Read()
	result := s.EvalGlobal(expr)
	if result != int64(3) {
		t.Errorf("Expected 3, got %v (%T)", result, result)
	}
}

func TestLoadCodeQuote(t *testing.T) {
	s := New()
	// Test that quoted expressions are returned unevaluated
	input := NewInputPortFromString("'(1 2 3)")
	expr := input.Read()
	result := s.EvalGlobal(expr)
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected Pair, got %v (%T)", result, result)
	}
	if First(pair) != int64(1) {
		t.Errorf("Expected first element 1, got %v", First(pair))
	}
}

func TestReadPlus(t *testing.T) {
	input := NewInputPortFromString("+")
	result := input.Read()
	if result != Symbol("+") {
		t.Errorf("Expected symbol '+', got %v (%T)", result, result)
	}
}

func TestReadMinus(t *testing.T) {
	input := NewInputPortFromString("-")
	result := input.Read()
	if result != Symbol("-") {
		t.Errorf("Expected symbol '-', got %v (%T)", result, result)
	}
}

func TestReadCharacterNamedAll(t *testing.T) {
	tests := []struct {
		input    string
		expected rune
	}{
		{`#\alarm`, '\x07'},
		{`#\backspace`, '\x08'},
		{`#\delete`, '\x7F'},
		{`#\escape`, '\x1B'},
		{`#\newline`, '\n'},
		{`#\null`, '\x00'},
		{`#\return`, '\r'},
		{`#\space`, ' '},
		{`#\tab`, '\t'},
		// Case insensitive
		{`#\Alarm`, '\x07'},
		{`#\SPACE`, ' '},
		{`#\Tab`, '\t'},
	}
	for _, tt := range tests {
		result := NewInputPortFromString(tt.input).Read()
		if result != tt.expected {
			t.Errorf("Input %s: expected %U, got %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestReadCharacterHex(t *testing.T) {
	tests := []struct {
		input    string
		expected rune
	}{
		{`#\x41`, 'A'},
		{`#\x03BB`, '\u03BB'}, // λ
		{`#\x0`, '\x00'},
		{`#\x61`, 'a'},
	}
	for _, tt := range tests {
		result := NewInputPortFromString(tt.input).Read()
		if result != tt.expected {
			t.Errorf("Input %s: expected %U, got %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestReadStringHexEscape(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{`"\x41;"`, "A"},
		{`"\x03BB;world"`, "\u03BBworld"},
		{`"hello\x21;"`, "hello!"},
	}
	for _, tt := range tests {
		result := NewInputPortFromString(tt.input).Read()
		if result != tt.expected {
			t.Errorf("Input %s: expected %q, got %v (%T)", tt.input, tt.expected, result, result)
		}
	}
}

func TestReadStringAlarmBackspace(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{`"\a"`, "\x07"},
		{`"\b"`, "\x08"},
		{`"\|"`, "|"},
	}
	for _, tt := range tests {
		result := NewInputPortFromString(tt.input).Read()
		if result != tt.expected {
			t.Errorf("Input %s: expected %q, got %q", tt.input, tt.expected, result)
		}
	}
}

func TestEvalParsedExpression(t *testing.T) {
	s := New()
	input := NewInputPortFromString("(+ 1 2)")
	expr := input.Read()
	result := s.EvalGlobal(expr)
	if result != int64(3) {
		t.Errorf("Expected 3, got %v (%T)", result, result)
	}
}
