package gscheme

import "testing"

func TestNewClosure(t *testing.T) {
	env := NewRootEnvironment()
	// Single expression body should be unwrapped
	params := List(Symbol("x"))
	body := List(Symbol("x"))
	c := NewClosure(params, body, env)
	if c == nil {
		t.Error("Expected closure to be created")
	}
}

func TestClosureVariadic(t *testing.T) {
	interpreter := New()
	// (define list-all (lambda args args))
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("list-all"),
		List(Symbol("lambda"), Symbol("args"), Symbol("args"))))
	// (list-all 1 2 3) should return (1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("list-all"), float64(1), float64(2), float64(3)))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(1) || First(Rest(pair)) != float64(2) || First(Rest(Rest(pair))) != float64(3) {
		t.Errorf("Expected (1 2 3) but got: %v", result)
	}
}

func TestClosureMultipleBody(t *testing.T) {
	interpreter := New()
	// (define foo (lambda (x) (define y 10) (+ x y)))
	// This tests that multi-expression bodies work via begin
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("foo"),
		List(Symbol("lambda"), List(Symbol("x")),
			List(Symbol("define"), Symbol("y"), float64(10)),
			List(Symbol("+"), Symbol("x"), Symbol("y")))))
	result := interpreter.EvalGlobal(List(Symbol("foo"), float64(5)))
	if result != float64(15) {
		t.Errorf("Expected 15 but got: %v", result)
	}
}
