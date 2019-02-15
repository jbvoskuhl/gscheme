package gscheme

import "testing"

func TestBooleanPrimitives(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("not"), List(Symbol("boolean?"), List(Symbol("boolean=?"), false, false, true)))
	result := interpreter.EvalGlobal(expression)
	if result != false {
		t.Errorf("Expected the expression (not (boolean? (boolean=? #f #f #t))) to evaluate to #f.")
	}
}

func TestBooleanEqualPrimitives(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("boolean=?"), false, false, false)
	result := interpreter.EvalGlobal(expression)
	if result != true {
		t.Errorf("Expected the expression (boolean=? #f #f #F))) to evaluate to #t.")
	}
}
