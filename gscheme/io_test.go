package gscheme

import (
	"os"
	"path/filepath"
	"testing"
)

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

// --- Port open/closed predicates ---

func TestInputPortOpen(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-input-string "x"))
		(define before (input-port-open? p))
		(close-input-port p)
		(define after (input-port-open? p))
		(list before after)
	`)
	got := Stringify(result)
	if got != "(#t #f)" {
		t.Errorf("Expected (#t #f), got: %s", got)
	}
}

func TestOutputPortOpen(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(define before (output-port-open? p))
		(close-output-port p)
		(define after (output-port-open? p))
		(list before after)
	`)
	got := Stringify(result)
	if got != "(#t #f)" {
		t.Errorf("Expected (#t #f), got: %s", got)
	}
}

func TestTextualPortPredicate(t *testing.T) {
	s := New()
	result := evalScheme(s, `(textual-port? (open-input-string ""))`)
	if result != true {
		t.Errorf("Expected #t for textual-port?, got: %v", result)
	}
	result = evalScheme(s, `(textual-port? (open-output-string))`)
	if result != true {
		t.Errorf("Expected #t for textual-port? on output, got: %v", result)
	}
}

func TestBinaryPortPredicate(t *testing.T) {
	s := New()
	result := evalScheme(s, `(binary-port? (open-input-string ""))`)
	if result != true {
		t.Errorf("Expected #t for binary-port?, got: %v", result)
	}
}

// --- Byte I/O ---

func TestReadU8(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-u8 (open-input-bytevector (bytevector 65 66 67)))`)
	if result != int64(65) {
		t.Errorf("Expected 65, got: %v (%T)", result, result)
	}
}

func TestPeekU8(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-input-bytevector (bytevector 42)))
		(define a (peek-u8 p))
		(define b (peek-u8 p))
		(define c (read-u8 p))
		(list a b c)
	`)
	got := Stringify(result)
	if got != "(42 42 42)" {
		t.Errorf("Expected (42 42 42), got: %s", got)
	}
}

func TestReadU8EOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read-u8 (open-input-bytevector (bytevector))))`)
	if result != true {
		t.Errorf("Expected #t for eof-object? on read-u8 from empty, got: %v", result)
	}
}

func TestU8Ready(t *testing.T) {
	s := New()
	result := evalScheme(s, `(u8-ready? (open-input-bytevector (bytevector 1)))`)
	if result != true {
		t.Errorf("Expected #t for u8-ready?, got: %v", result)
	}
}

func TestWriteU8(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-bytevector))
		(write-u8 65 p)
		(write-u8 66 p)
		(get-output-bytevector p)
	`)
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected []uint8, got: %T", result)
	}
	if len(bv) != 2 || bv[0] != 65 || bv[1] != 66 {
		t.Errorf("Expected (bytevector 65 66), got: %v", bv)
	}
}

// --- String I/O ---

func TestIOReadString(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-string 5 (open-input-string "hello world"))`)
	if result != "hello" {
		t.Errorf("Expected hello, got: %v", result)
	}
}

func TestReadStringEOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read-string 5 (open-input-string "")))`)
	if result != true {
		t.Errorf("Expected #t for eof on read-string, got: %v", result)
	}
}

func TestReadStringPartial(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-string 10 (open-input-string "hi"))`)
	if result != "hi" {
		t.Errorf("Expected hi, got: %v", result)
	}
}

func TestWriteString(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-string "hello" p)
		(get-output-string p)
	`)
	if result != "hello" {
		t.Errorf("Expected hello, got: %v", result)
	}
}

func TestWriteStringSubstring(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-string "hello world" p 6 11)
		(get-output-string p)
	`)
	if result != "world" {
		t.Errorf("Expected world, got: %v", result)
	}
}

func TestWriteStringStart(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-string "abcdef" p 2)
		(get-output-string p)
	`)
	if result != "cdef" {
		t.Errorf("Expected cdef, got: %v", result)
	}
}

// --- Bytevector I/O ---

func TestReadBytevector(t *testing.T) {
	s := New()
	result := evalScheme(s, `(read-bytevector 3 (open-input-bytevector (bytevector 10 20 30 40 50)))`)
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected []uint8, got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 10 || bv[1] != 20 || bv[2] != 30 {
		t.Errorf("Expected (bytevector 10 20 30), got: %v", bv)
	}
}

func TestReadBytevectorEOF(t *testing.T) {
	s := New()
	result := evalScheme(s, `(eof-object? (read-bytevector 5 (open-input-bytevector (bytevector))))`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestReadBytevectorMut(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define bv (make-bytevector 5 0))
		(define p (open-input-bytevector (bytevector 1 2 3)))
		(define n (read-bytevector! bv p))
		(list n bv)
	`)
	got := Stringify(result)
	if got != "(3 #u8(1 2 3 0 0))" {
		t.Errorf("Expected (3 #u8(1 2 3 0 0)), got: %s", got)
	}
}

func TestWriteBytevector(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-bytevector))
		(write-bytevector (bytevector 1 2 3 4 5) p)
		(get-output-bytevector p)
	`)
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected []uint8, got: %T", result)
	}
	if len(bv) != 5 || bv[0] != 1 || bv[4] != 5 {
		t.Errorf("Expected (bytevector 1 2 3 4 5), got: %v", bv)
	}
}

func TestWriteBytevectorSlice(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-bytevector))
		(write-bytevector (bytevector 10 20 30 40 50) p 1 4)
		(get-output-bytevector p)
	`)
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected []uint8, got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 20 || bv[1] != 30 || bv[2] != 40 {
		t.Errorf("Expected (bytevector 20 30 40), got: %v", bv)
	}
}

func TestOpenInputBytevector(t *testing.T) {
	s := New()
	result := evalScheme(s, `(input-port? (open-input-bytevector (bytevector 1 2 3)))`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestOpenOutputBytevector(t *testing.T) {
	s := New()
	result := evalScheme(s, `(output-port? (open-output-bytevector))`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestBytevectorPortRoundTrip(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define out (open-output-bytevector))
		(write-u8 72 out)
		(write-u8 105 out)
		(define bv (get-output-bytevector out))
		(define in (open-input-bytevector bv))
		(define a (read-u8 in))
		(define b (read-u8 in))
		(list a b)
	`)
	got := Stringify(result)
	if got != "(72 105)" {
		t.Errorf("Expected (72 105), got: %s", got)
	}
}

// --- Error port ---

func TestCurrentErrorPort(t *testing.T) {
	s := New()
	result := evalScheme(s, `(output-port? (current-error-port))`)
	if result != true {
		t.Errorf("Expected #t for output-port? on current-error-port, got: %v", result)
	}
}

// --- File operations ---

func TestFileExists(t *testing.T) {
	s := New()
	// Create a temp file
	tmp := filepath.Join(t.TempDir(), "test-exists.txt")
	os.WriteFile(tmp, []byte("hi"), 0644)

	result := evalScheme(s, `(file-exists? "`+tmp+`")`)
	if result != true {
		t.Errorf("Expected #t for existing file, got: %v", result)
	}

	result = evalScheme(s, `(file-exists? "`+tmp+`.nope")`)
	if result != false {
		t.Errorf("Expected #f for non-existing file, got: %v", result)
	}
}

func TestDeleteFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-delete.txt")
	os.WriteFile(tmp, []byte("hi"), 0644)

	evalScheme(s, `(delete-file "`+tmp+`")`)

	if _, err := os.Stat(tmp); err == nil {
		t.Errorf("File should have been deleted")
	}
}

func TestCallWithInputFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-call-in.txt")
	os.WriteFile(tmp, []byte("42"), 0644)

	result := evalScheme(s, `(call-with-input-file "`+tmp+`" (lambda (p) (read p)))`)
	if result != int64(42) {
		t.Errorf("Expected 42, got: %v (%T)", result, result)
	}
}

func TestCallWithOutputFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-call-out.txt")

	evalScheme(s, `(call-with-output-file "`+tmp+`" (lambda (p) (display "hello" p)))`)

	data, err := os.ReadFile(tmp)
	if err != nil {
		t.Fatalf("Could not read output file: %v", err)
	}
	if string(data) != "hello" {
		t.Errorf("Expected hello, got: %s", string(data))
	}
}

func TestWithInputFromFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-with-in.txt")
	os.WriteFile(tmp, []byte("99"), 0644)

	result := evalScheme(s, `(with-input-from-file "`+tmp+`" (lambda () (read)))`)
	if result != int64(99) {
		t.Errorf("Expected 99, got: %v (%T)", result, result)
	}
}

func TestWithOutputToFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-with-out.txt")

	evalScheme(s, `(with-output-to-file "`+tmp+`" (lambda () (display "output")))`)

	data, err := os.ReadFile(tmp)
	if err != nil {
		t.Fatalf("Could not read output file: %v", err)
	}
	if string(data) != "output" {
		t.Errorf("Expected output, got: %s", string(data))
	}
}

func TestWithInputFromFileRestoresPort(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-restore-in.txt")
	os.WriteFile(tmp, []byte("data"), 0644)

	// Get original port, call with-input-from-file, verify port is restored
	result := evalScheme(s, `
		(define orig (current-input-port))
		(with-input-from-file "`+tmp+`" (lambda () (read)))
		(eq? orig (current-input-port))
	`)
	if result != true {
		t.Errorf("Expected current-input-port to be restored, got: %v", result)
	}
}

func TestWithOutputToFileRestoresPort(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-restore-out.txt")

	result := evalScheme(s, `
		(define orig (current-output-port))
		(with-output-to-file "`+tmp+`" (lambda () (display "x")))
		(eq? orig (current-output-port))
	`)
	if result != true {
		t.Errorf("Expected current-output-port to be restored, got: %v", result)
	}
}

// --- Write library ---

func TestWriteShared(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-shared '(1 2 3) p)
		(get-output-string p)
	`)
	if result != "(1 2 3)" {
		t.Errorf("Expected (1 2 3), got: %v", result)
	}
}

func TestWriteSimple(t *testing.T) {
	s := New()
	result := evalScheme(s, `
		(define p (open-output-string))
		(write-simple "hi" p)
		(get-output-string p)
	`)
	if result != `"hi"` {
		t.Errorf(`Expected "\"hi\"", got: %v`, result)
	}
}

// --- Binary file I/O ---

func TestOpenBinaryInputFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-bin-in.dat")
	os.WriteFile(tmp, []byte{0x48, 0x65, 0x6C}, 0644)

	result := evalScheme(s, `
		(define p (open-binary-input-file "`+tmp+`"))
		(define b (read-u8 p))
		(close-input-port p)
		b
	`)
	if result != int64(0x48) {
		t.Errorf("Expected 72, got: %v", result)
	}
}

func TestOpenBinaryOutputFile(t *testing.T) {
	s := New()
	tmp := filepath.Join(t.TempDir(), "test-bin-out.dat")

	evalScheme(s, `
		(define p (open-binary-output-file "`+tmp+`"))
		(write-u8 65 p)
		(write-u8 66 p)
		(close-output-port p)
	`)

	data, err := os.ReadFile(tmp)
	if err != nil {
		t.Fatalf("Could not read binary output file: %v", err)
	}
	if len(data) != 2 || data[0] != 65 || data[1] != 66 {
		t.Errorf("Expected [65 66], got: %v", data)
	}
}
