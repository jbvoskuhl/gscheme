package gscheme

import "testing"

func TestStringConstraint(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("string->symbol"), 45)
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected that the value 45 passed into a primitive expecting string would evaluate to error.")
	}
}

func TestSymbolConstraint(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("symbol->string"), 12)
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected that the value 12 passed into a primitive expecting symbol would evaluate to error.")
	}
}
