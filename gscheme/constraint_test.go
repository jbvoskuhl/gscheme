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

func TestCharConstraint(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char->integer"), 42)
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected that the value 42 passed into a primitive expecting character would evaluate to error.")
	}
}

func TestIntegerConstraint(t *testing.T) {
	interpreter := New()
	numbers := []interface{}{
		uint(65), uint8(65), uint16(65), uint32(65), uint64(65), int(65), int8(65), int16(65), int32(65), int64(65)}
	for _, number := range numbers {
		expression := List(Symbol("integer->char"), number)
		result := interpreter.EvalGlobal(expression)
		if result != 'A' {
			t.Errorf("Expected that the value %t passed into a primitive expecting integer would evaluate to #\\A.", result)
		}
	}
	expression := List(Symbol("integer->char"), "hello")
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected that \"hello\" passed into a primitive expecting integer would evaluate to error.")
	}
}
