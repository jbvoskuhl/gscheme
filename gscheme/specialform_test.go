package gscheme

import "testing"

func TestSpecialForm(t *testing.T) {
	s := NewSpecialForm("three", func(interpreter Scheme, args interface{}, environment Environment) interface{} {
		return 3
	})
	interpreter := New()
	result := s.Apply(interpreter, nil, interpreter.Environment())
	if 3 != result {
		t.Errorf("Expected special form three to return 3 but instead got: %v.", result)
	}
}

func TestIf(t *testing.T) {
	interpreter := New()
	statement := List(Symbol("if"), true, 2, 3)
	result := interpreter.EvalGlobal(statement)
	if 2 != result {
		t.Errorf("Expected (if #t 2 3) to evaluate to 2 but instead was: %v.", result)
	}
	statement2 := List(Symbol("if"), false, 2, 3)
	result2 := interpreter.EvalGlobal(statement2)
	if 3 != result2 {
		t.Errorf("Expected (if #f 2 3) to evaluate to 3 but instead was: %v.", result2)
	}
}
