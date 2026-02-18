package gscheme

import "testing"

func TestReadFromStringPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read (open-input-string "42"))`)
	if result != int64(42) {
		t.Errorf("Expected 42, got: %v (%T)", result, result)
	}
}

func TestIOReadMultipleExpressions(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-input-string "(+ 1 2) hello"))
		(define first-expr (read p))
		(define second-expr (read p))
		(list first-expr second-expr)
	`)
	expected := "(+ 1 2) hello"
	got := Stringify(result)
	if got != "((+ 1 2) hello)" {
		t.Errorf("Expected ((+ 1 2) hello), got: %s", got)
	}
	_ = expected
}

func TestIOReadEOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read (open-input-string "")))`)
	if result != true {
		t.Errorf("Expected #t for eof-object? on empty input, got: %v", result)
	}
}

func TestWriteToStringPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write "hello" p)
		(get-output-string p)
	`)
	if result != `"hello"` {
		t.Errorf(`Expected "\"hello\"", got: %v`, result)
	}
}

func TestDisplayToStringPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(display "hello" p)
		(get-output-string p)
	`)
	if result != "hello" {
		t.Errorf("Expected hello, got: %v", result)
	}
}

func TestWriteVsDisplay(t *testing.T) {
	s := New()
	// write should quote strings
	writeResult := evalScheme(s, `
		(define p (open-output-string))
		(write "hi" p)
		(get-output-string p)
	`)
	if writeResult != `"hi"` {
		t.Errorf(`write: expected "\"hi\"", got: %v`, writeResult)
	}

	// display should not quote strings
	displayResult := evalScheme(s, `
		(define p2 (open-output-string))
		(display "hi" p2)
		(get-output-string p2)
	`)
	if displayResult != "hi" {
		t.Errorf("display: expected hi, got: %v", displayResult)
	}
}

func TestWriteCharDisplay(t *testing.T) {
	s := New()
	// write should show #\a notation
	writeResult := evalScheme(s, `
		(define p (open-output-string))
		(write #\a p)
		(get-output-string p)
	`)
	if writeResult != `#\a` {
		t.Errorf(`write char: expected "#\\a", got: %v`, writeResult)
	}

	// display should show raw character
	displayResult := evalScheme(s, `
		(define p2 (open-output-string))
		(display #\a p2)
		(get-output-string p2)
	`)
	if displayResult != "a" {
		t.Errorf("display char: expected a, got: %v", displayResult)
	}
}

func TestReadChar(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-char (open-input-string "abc"))`)
	if result != 'a' {
		t.Errorf("Expected #\\a, got: %v", result)
	}
}

func TestPeekChar(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-input-string "xy"))
		(define c1 (peek-char p))
		(define c2 (peek-char p))
		(define c3 (read-char p))
		(list c1 c2 c3)
	`)
	got := Stringify(result)
	if got != "(#\\x #\\x #\\x)" {
		t.Errorf("peek-char should not consume: expected (#\\x #\\x #\\x), got: %s", got)
	}
}

func TestReadCharEOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read-char (open-input-string "")))`)
	if result != true {
		t.Errorf("Expected #t for eof-object? on read-char from empty, got: %v", result)
	}
}

func TestReadLine(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-line (open-input-string "hello world\ngoodbye"))`)
	if result != "hello world" {
		t.Errorf("Expected hello world, got: %v", result)
	}
}

func TestReadLineEOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read-line (open-input-string "")))`)
	if result != true {
		t.Errorf("Expected #t for eof-object? on read-line from empty, got: %v", result)
	}
}

func TestReadLineNoNewline(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-line (open-input-string "no newline"))`)
	if result != "no newline" {
		t.Errorf("Expected 'no newline', got: %v", result)
	}
}

func TestNewline(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(newline p)
		(get-output-string p)
	`)
	if result != "\n" {
		t.Errorf("Expected newline, got: %q", result)
	}
}

func TestWriteChar(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-char #\Z p)
		(get-output-string p)
	`)
	if result != "Z" {
		t.Errorf("Expected Z, got: %v", result)
	}
}

func TestCharReady(t *testing.T) {
	s := New()
	result := evalScheme(s, `(char-ready? (open-input-string "x"))`)
	if result != true {
		t.Errorf("Expected #t for char-ready?, got: %v", result)
	}
}

func TestClosePort(t *testing.T) {
	s := New()
	// Should not panic
	result := evalScheme(s, `
		(define p (open-input-string "x"))
		(close-port p)
	`)
	if result != nil {
		t.Errorf("Expected nil from close-port, got: %v", result)
	}
}

func TestCloseOutputPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(close-output-port p)
	`)
	if result != nil {
		t.Errorf("Expected nil from close-output-port, got: %v", result)
	}
}

func TestCurrentInputPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `(input-port? (current-input-port))`)
	if result != true {
		t.Errorf("Expected #t for input-port? on current-input-port, got: %v", result)
	}
}

func TestCurrentOutputPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `(output-port? (current-output-port))`)
	if result != true {
		t.Errorf("Expected #t for output-port? on current-output-port, got: %v", result)
	}
}

func TestEofObject(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (eof-object))`)
	if result != true {
		t.Errorf("Expected #t for eof-object? on (eof-object), got: %v", result)
	}
}

func TestInputPortPredicate(t *testing.T) {
	s := New()
	result := evalScheme(s, `(input-port? (open-input-string ""))`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestOutputPortPredicate(t *testing.T) {
	s := New()
	result := evalScheme(s, `(output-port? (open-output-string))`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestPortPredicate(t *testing.T) {
	s := New()
	result := evalScheme(s, `(port? (open-input-string ""))`)
	if result != true {
		t.Errorf("Expected #t for port? on input port, got: %v", result)
	}
	result = evalScheme(s, `(port? (open-output-string))`)
	if result != true {
		t.Errorf("Expected #t for port? on output port, got: %v", result)
	}
	result = evalScheme(s, `(port? 42)`)
	if result != false {
		t.Errorf("Expected #f for port? on number, got: %v", result)
	}
}

func TestOpenInputString(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-input-string "(a b c)"))
		(read p)
	`)
	got := Stringify(result)
	if got != "(a b c)" {
		t.Errorf("Expected (a b c), got: %s", got)
	}
}

func TestGetOutputString(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(display 42 p)
		(display " " p)
		(display "hello" p)
		(get-output-string p)
	`)
	if result != "42 hello" {
		t.Errorf("Expected '42 hello', got: %v", result)
	}
}

func TestFlushOutputPort(t *testing.T) {
	s := New()
	// Should not error
	result := evalScheme(s, `
		(define p (open-output-string))
		(display "x" p)
		(flush-output-port p)
		(get-output-string p)
	`)
	if result != "x" {
		t.Errorf("Expected x, got: %v", result)
	}
}

func TestDisplayList(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(display '(1 "two" #\3) p)
		(get-output-string p)
	`)
	if result != "(1 two 3)" {
		t.Errorf("Expected (1 two 3), got: %v", result)
	}
}

func TestWriteList(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write '(1 "two" #\3) p)
		(get-output-string p)
	`)
	if result != `(1 "two" #\3)` {
		t.Errorf(`Expected (1 "two" #\3), got: %v`, result)
	}
}

func TestDisplayNumber(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(display 3.14 p)
		(get-output-string p)
	`)
	if result != "3.14" {
		t.Errorf("Expected 3.14, got: %v", result)
	}
}

func TestDisplayBoolean(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(display #t p)
		(display #f p)
		(get-output-string p)
	`)
	if result != "#t#f" {
		t.Errorf("Expected #t#f, got: %v", result)
	}
}

func TestMultipleWritesSamePort(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-char #\H p)
		(write-char #\i p)
		(write-char #\! p)
		(get-output-string p)
	`)
	if result != "Hi!" {
		t.Errorf("Expected Hi!, got: %v", result)
	}
}
