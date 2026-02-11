package gscheme

import "testing"

func TestSymbolString(t *testing.T) {
	s := Symbol("car")
	if Stringify(s) != "car" {
		t.Errorf("A symbol car doesn't stringify as car instead it was: %v.", Stringify(s))
	}
}

func TestIsSymbol(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol?"), float64(23))
	result := interpreter.EvalGlobal(expression)
	if false != result {
		t.Errorf("Expected (symbol? 23) to evaluate to #f, but instead was: %v.", result)
	}
}

func TestIsSymbolWithSymbol(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol?"), List(Symbol("quote"), Symbol("foo")))
	result := interpreter.EvalGlobal(expression)
	if true != result {
		t.Errorf("Expected (symbol? 'foo) to evaluate to #t, but instead was: %v.", result)
	}
}

func TestIsSymbolWithString(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol?"), "hello")
	result := interpreter.EvalGlobal(expression)
	if false != result {
		t.Errorf("Expected (symbol? \"hello\") to evaluate to #f, but instead was: %v.", result)
	}
}

func TestIsSymbolWithBoolean(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol?"), true)
	result := interpreter.EvalGlobal(expression)
	if false != result {
		t.Errorf("Expected (symbol? #t) to evaluate to #f, but instead was: %v.", result)
	}
}

func TestIsSymbolWithPair(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol?"), List(Symbol("quote"), List(Symbol("a"), Symbol("b"))))
	result := interpreter.EvalGlobal(expression)
	if false != result {
		t.Errorf("Expected (symbol? '(a b)) to evaluate to #f, but instead was: %v.", result)
	}
}

func TestSymbolToString(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol->string"), List(Symbol("quote"), Symbol("hello")))
	result := interpreter.EvalGlobal(expression)
	if "hello" != result {
		t.Errorf("Expected (symbol->string 'hello) to evaluate to \"hello\", but instead was: %v.", result)
	}
}

func TestStringToSymbol(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("string->symbol"), "world")
	result := interpreter.EvalGlobal(expression)
	if Symbol("world") != result {
		t.Errorf("Expected (string->symbol \"world\") to evaluate to symbol world, but instead was: %v.", result)
	}
}

func TestConversion(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol->string"), List(Symbol("string->symbol"), "test"))
	result := interpreter.EvalGlobal(expression)
	if "test" != result {
		t.Errorf("Expected (symbol->string (string->symbol \"test\")) to evaluate to \"test\", but instead was: %v.", result)
	}
}

func TestSymbolToStringWithNonSymbol(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol->string"), float64(42))
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected (symbol->string 42) to evaluate to error, but instead was: %v.", result)
	}
}

func TestStringToSymbolWithNonString(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("string->symbol"), float64(42))
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected (string->symbol 42) to evaluate to error, but instead was: %v.", result)
	}
}

func TestSymbolStringMethod(t *testing.T) {
	s := Symbol("lambda")
	if s.String() != "lambda" {
		t.Errorf("Expected Symbol.String() to return \"lambda\", but instead was: %v.", s.String())
	}
}

func TestSymbolEmptyString(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("string->symbol"), "")
	result := interpreter.EvalGlobal(expression)
	if Symbol("") != result {
		t.Errorf("Expected (string->symbol \"\") to evaluate to empty symbol, but instead was: %v.", result)
	}
}
