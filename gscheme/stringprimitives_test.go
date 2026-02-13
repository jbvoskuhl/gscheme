package gscheme

import "testing"

func TestInstallStringPrimitives(t *testing.T) {
	environment := NewRootEnvironment()
	installStringPrimitives(environment)
	symbols := []Symbol{
		"make-string", "string", "string-length", "string-ref", "substring", "string-copy",
		"string=?", "string<?", "string>?", "string<=?", "string>=?",
		"string-ci=?", "string-ci<?", "string-ci>?", "string-ci<=?", "string-ci>=?",
		"string-upcase", "string-downcase", "string-foldcase",
		"string-append", "number->string",
		"string->list", "list->string", "string->number",
		"string-map", "string-for-each",
	}
	for _, symbol := range symbols {
		_, ok := environment.Lookup(symbol)
		if !ok {
			t.Errorf("Expected to find symbol %s but it was not found.", symbol)
		}
	}
}

func TestMakeString(t *testing.T) {
	interpreter := New()

	// (make-string 3 #\a) => "aaa"
	result := interpreter.EvalGlobal(List(Symbol("make-string"), float64(3), 'a'))
	if result != "aaa" {
		t.Errorf("Expected \"aaa\" but got: %v", result)
	}

	// (make-string 3) => "   " (default space)
	result = interpreter.EvalGlobal(List(Symbol("make-string"), float64(3)))
	if result != "   " {
		t.Errorf("Expected \"   \" but got: %v", result)
	}

	// (make-string 0) => ""
	result = interpreter.EvalGlobal(List(Symbol("make-string"), float64(0)))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestStringFromChars(t *testing.T) {
	interpreter := New()

	// (string #\a #\b #\c) => "abc"
	result := interpreter.EvalGlobal(List(Symbol("string"), 'a', 'b', 'c'))
	if result != "abc" {
		t.Errorf("Expected \"abc\" but got: %v", result)
	}

	// (string) => ""
	result = interpreter.EvalGlobal(List(Symbol("string")))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestStringLength(t *testing.T) {
	interpreter := New()

	// (string-length "hello") => 5
	result := interpreter.EvalGlobal(List(Symbol("string-length"), "hello"))
	if result != int64(5) {
		t.Errorf("Expected 5 but got: %v", result)
	}

	// (string-length "") => 0
	result = interpreter.EvalGlobal(List(Symbol("string-length"), ""))
	if result != int64(0) {
		t.Errorf("Expected 0 but got: %v", result)
	}
}

func TestStringRef(t *testing.T) {
	interpreter := New()

	// (string-ref "hello" 1) => #\e
	result := interpreter.EvalGlobal(List(Symbol("string-ref"), "hello", float64(1)))
	if result != 'e' {
		t.Errorf("Expected #\\e but got: %v", result)
	}

	// (string-ref "hello" 0) => #\h
	result = interpreter.EvalGlobal(List(Symbol("string-ref"), "hello", float64(0)))
	if result != 'h' {
		t.Errorf("Expected #\\h but got: %v", result)
	}
}

func TestSubstring(t *testing.T) {
	interpreter := New()

	// (substring "hello world" 6 11) => "world"
	result := interpreter.EvalGlobal(List(Symbol("substring"), "hello world", float64(6), float64(11)))
	if result != "world" {
		t.Errorf("Expected \"world\" but got: %v", result)
	}

	// (substring "hello" 0 0) => ""
	result = interpreter.EvalGlobal(List(Symbol("substring"), "hello", float64(0), float64(0)))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestStringCopy(t *testing.T) {
	interpreter := New()

	// (string-copy "hello") => "hello"
	result := interpreter.EvalGlobal(List(Symbol("string-copy"), "hello"))
	if result != "hello" {
		t.Errorf("Expected \"hello\" but got: %v", result)
	}
}

func TestStringEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string=?"), "abc", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string=? \"abc\" \"abc\") but got: %v", result)
	}

	result = interpreter.EvalGlobal(List(Symbol("string=?"), "abc", "def"))
	if result != false {
		t.Errorf("Expected #f for (string=? \"abc\" \"def\") but got: %v", result)
	}

	// Variadic: (string=? "a" "a" "a") => #t
	result = interpreter.EvalGlobal(List(Symbol("string=?"), "a", "a", "a"))
	if result != true {
		t.Errorf("Expected #t for (string=? \"a\" \"a\" \"a\") but got: %v", result)
	}
}

func TestStringLessThan(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string<?"), "abc", "def"))
	if result != true {
		t.Errorf("Expected #t for (string<? \"abc\" \"def\") but got: %v", result)
	}

	result = interpreter.EvalGlobal(List(Symbol("string<?"), "def", "abc"))
	if result != false {
		t.Errorf("Expected #f for (string<? \"def\" \"abc\") but got: %v", result)
	}
}

func TestStringGreaterThan(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string>?"), "def", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string>? \"def\" \"abc\") but got: %v", result)
	}
}

func TestStringLessThanOrEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string<=?"), "abc", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string<=? \"abc\" \"abc\") but got: %v", result)
	}

	result = interpreter.EvalGlobal(List(Symbol("string<=?"), "abc", "def"))
	if result != true {
		t.Errorf("Expected #t for (string<=? \"abc\" \"def\") but got: %v", result)
	}
}

func TestStringGreaterThanOrEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string>=?"), "def", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string>=? \"def\" \"abc\") but got: %v", result)
	}

	result = interpreter.EvalGlobal(List(Symbol("string>=?"), "abc", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string>=? \"abc\" \"abc\") but got: %v", result)
	}
}

func TestStringCIEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-ci=?"), "ABC", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string-ci=? \"ABC\" \"abc\") but got: %v", result)
	}

	result = interpreter.EvalGlobal(List(Symbol("string-ci=?"), "abc", "def"))
	if result != false {
		t.Errorf("Expected #f for (string-ci=? \"abc\" \"def\") but got: %v", result)
	}
}

func TestStringCILessThan(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-ci<?"), "ABC", "def"))
	if result != true {
		t.Errorf("Expected #t for (string-ci<? \"ABC\" \"def\") but got: %v", result)
	}
}

func TestStringCIGreaterThan(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-ci>?"), "DEF", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string-ci>? \"DEF\" \"abc\") but got: %v", result)
	}
}

func TestStringCILessThanOrEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-ci<=?"), "ABC", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string-ci<=? \"ABC\" \"abc\") but got: %v", result)
	}
}

func TestStringCIGreaterThanOrEqual(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-ci>=?"), "ABC", "abc"))
	if result != true {
		t.Errorf("Expected #t for (string-ci>=? \"ABC\" \"abc\") but got: %v", result)
	}
}

func TestStringUpcase(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-upcase"), "hello"))
	if result != "HELLO" {
		t.Errorf("Expected \"HELLO\" but got: %v", result)
	}
}

func TestStringDowncase(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-downcase"), "HELLO"))
	if result != "hello" {
		t.Errorf("Expected \"hello\" but got: %v", result)
	}
}

func TestStringFoldcase(t *testing.T) {
	interpreter := New()

	result := interpreter.EvalGlobal(List(Symbol("string-foldcase"), "HELLO"))
	if result != "hello" {
		t.Errorf("Expected \"hello\" but got: %v", result)
	}
}

func TestStringAppend(t *testing.T) {
	interpreter := New()

	// (string-append "hello" " " "world") => "hello world"
	result := interpreter.EvalGlobal(List(Symbol("string-append"), "hello", " ", "world"))
	if result != "hello world" {
		t.Errorf("Expected \"hello world\" but got: %v", result)
	}

	// (string-append) => ""
	result = interpreter.EvalGlobal(List(Symbol("string-append")))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestNumberToString(t *testing.T) {
	interpreter := New()

	// (number->string 42) => "42"
	result := interpreter.EvalGlobal(List(Symbol("number->string"), float64(42)))
	if result != "42" {
		t.Errorf("Expected \"42\" but got: %v", result)
	}

	// (number->string 3.14) => "3.14"
	result = interpreter.EvalGlobal(List(Symbol("number->string"), float64(3.14)))
	if result != "3.14" {
		t.Errorf("Expected \"3.14\" but got: %v", result)
	}

	// (number->string 255 16) => "ff"
	result = interpreter.EvalGlobal(List(Symbol("number->string"), float64(255), float64(16)))
	if result != "ff" {
		t.Errorf("Expected \"ff\" but got: %v", result)
	}

	// (number->string 8 2) => "1000"
	result = interpreter.EvalGlobal(List(Symbol("number->string"), float64(8), float64(2)))
	if result != "1000" {
		t.Errorf("Expected \"1000\" but got: %v", result)
	}
}

func TestStringToList(t *testing.T) {
	interpreter := New()

	// (string->list "abc") => (#\a #\b #\c)
	result := interpreter.EvalGlobal(List(Symbol("string->list"), "abc"))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != 'a' || First(Rest(pair)) != 'b' || First(Rest(Rest(pair))) != 'c' {
		t.Errorf("Expected (#\\a #\\b #\\c) but got: %v", result)
	}

	// (string->list "") => ()
	result = interpreter.EvalGlobal(List(Symbol("string->list"), ""))
	if result != nil {
		t.Errorf("Expected () but got: %v", result)
	}
}

func TestListToString(t *testing.T) {
	interpreter := New()

	// (list->string '(#\a #\b #\c)) => "abc"
	result := interpreter.EvalGlobal(List(Symbol("list->string"),
		List(Symbol("quote"), List('a', 'b', 'c'))))
	if result != "abc" {
		t.Errorf("Expected \"abc\" but got: %v", result)
	}

	// (list->string '()) => ""
	result = interpreter.EvalGlobal(List(Symbol("list->string"),
		List(Symbol("quote"), nil)))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestStringToNumber(t *testing.T) {
	interpreter := New()

	// (string->number "42") => 42
	result := interpreter.EvalGlobal(List(Symbol("string->number"), "42"))
	if result != int64(42) {
		t.Errorf("Expected 42 but got: %v", result)
	}

	// (string->number "3.14") => 3.14
	result = interpreter.EvalGlobal(List(Symbol("string->number"), "3.14"))
	if result != float64(3.14) {
		t.Errorf("Expected 3.14 but got: %v", result)
	}

	// (string->number "ff" 16) => 255
	result = interpreter.EvalGlobal(List(Symbol("string->number"), "ff", float64(16)))
	if result != int64(255) {
		t.Errorf("Expected 255 but got: %v", result)
	}

	// (string->number "not-a-number") => #f
	result = interpreter.EvalGlobal(List(Symbol("string->number"), "not-a-number"))
	if result != false {
		t.Errorf("Expected #f but got: %v", result)
	}
}

func TestStringMap(t *testing.T) {
	interpreter := New()

	// (string-map char-upcase "hello") => "HELLO"
	result := interpreter.EvalGlobal(List(Symbol("string-map"),
		Symbol("char-upcase"), "hello"))
	if result != "HELLO" {
		t.Errorf("Expected \"HELLO\" but got: %v", result)
	}

	// (string-map char-upcase "") => ""
	result = interpreter.EvalGlobal(List(Symbol("string-map"),
		Symbol("char-upcase"), ""))
	if result != "" {
		t.Errorf("Expected empty string but got: %v", result)
	}
}

func TestStringForEach(t *testing.T) {
	interpreter := New()

	// Use string-for-each to accumulate characters into a list via side effect
	// (let ((result '())) (string-for-each (lambda (c) (set! result (cons c result))) "abc") result)
	// => (#\c #\b #\a)
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("result"), List(Symbol("quote"), nil))),
		List(Symbol("string-for-each"),
			List(Symbol("lambda"), List(Symbol("c")),
				List(Symbol("set!"), Symbol("result"),
					List(Symbol("cons"), Symbol("c"), Symbol("result")))),
			"abc"),
		Symbol("result")))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != 'c' || First(Rest(pair)) != 'b' || First(Rest(Rest(pair))) != 'a' {
		t.Errorf("Expected (#\\c #\\b #\\a) but got: %v", result)
	}

	// (string-for-each (lambda (c) c) "") => nil
	result = interpreter.EvalGlobal(List(Symbol("string-for-each"),
		List(Symbol("lambda"), List(Symbol("c")), Symbol("c")),
		""))
	if result != nil {
		t.Errorf("Expected nil but got: %v", result)
	}
}
