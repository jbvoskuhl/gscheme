package gscheme

import (
	"testing"
)

func TestSpecialForm(t *testing.T) {
	s := NewSpecialForm("three", func(interpreter Scheme, args Pair, environment Environment) interface{} {
		return 3
	})
	interpreter := New()
	result := s.Apply(interpreter, nil, interpreter.Environment())
	if 3 != result {
		t.Errorf("Expected special form three to return 3 but instead got: %v.", result)
	}
}

func TestQuoteHappyPath(t *testing.T) {
	interpreter := New()
	statement := List(Symbol("quote"), Symbol("x"))
	result := interpreter.EvalGlobal(statement)
	if Symbol("x") != result {
		t.Errorf("Expected (quote x) to evaluate to x but instead was: %v.", result)
	}
}

func TestQuoteMultiple(t *testing.T) {
	interpreter := New()
	statement := List(Symbol("quote"), Symbol("x"), float64(34))
	result := interpreter.EvalGlobal(statement)
	if Symbol("x") != result {
		t.Errorf("Expected (quote x 34) to ignore 34 and evaluate to x but instead was: %v.", result)
	}
}

func TestQuoteNone(t *testing.T) {
	interpreter := New()
	statement := List(Symbol("quote"))
	result := interpreter.EvalGlobal(statement)
	if nil != result {
		t.Errorf("Expected (quote) evaluate to nil but instead was: %v.", result)
	}
}
