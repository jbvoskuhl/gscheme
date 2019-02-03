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

func TestConversion(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol->string"), List(Symbol("string->symbol"), "test"))
	result := interpreter.EvalGlobal(expression)
	if "test" != result {
		t.Errorf("Expected (symbol->string (string->symbol \"test\")) to evaluate to \"test\", but instead was: %v.", result)
	}
}
