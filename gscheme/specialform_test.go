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

func TestDefineSimple(t *testing.T) {
	interpreter := New()
	// (define x 42)
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("x"), float64(42)))
	// x should now evaluate to 42
	result := interpreter.EvalGlobal(Symbol("x"))
	if result != float64(42) {
		t.Errorf("Expected x to be 42 but was: %v", result)
	}
}

func TestDefineWithExpression(t *testing.T) {
	interpreter := New()
	// (define y (+ 1 2))
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("y"),
		List(Symbol("+"), float64(1), float64(2))))
	result := interpreter.EvalGlobal(Symbol("y"))
	if result != float64(3) {
		t.Errorf("Expected y to be 3 but was: %v", result)
	}
}

func TestDefineFunctionShorthand(t *testing.T) {
	interpreter := New()
	// (define (square x) (* x x))
	interpreter.EvalGlobal(List(Symbol("define"),
		List(Symbol("square"), Symbol("x")),
		List(Symbol("*"), Symbol("x"), Symbol("x"))))
	// (square 5) should return 25
	result := interpreter.EvalGlobal(List(Symbol("square"), float64(5)))
	if result != float64(25) {
		t.Errorf("Expected (square 5) to be 25 but was: %v", result)
	}
}

func TestLambda(t *testing.T) {
	interpreter := New()
	// ((lambda (x) (* x 2)) 5)
	lambda := List(Symbol("lambda"), List(Symbol("x")),
		List(Symbol("*"), Symbol("x"), float64(2)))
	result := interpreter.EvalGlobal(List(lambda, float64(5)))
	if result != float64(10) {
		t.Errorf("Expected ((lambda (x) (* x 2)) 5) to be 10 but was: %v", result)
	}
}

func TestLambdaMultipleParams(t *testing.T) {
	interpreter := New()
	// (define add (lambda (a b) (+ a b)))
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("add"),
		List(Symbol("lambda"), List(Symbol("a"), Symbol("b")),
			List(Symbol("+"), Symbol("a"), Symbol("b")))))
	// (add 3 4) should return 7
	result := interpreter.EvalGlobal(List(Symbol("add"), float64(3), float64(4)))
	if result != float64(7) {
		t.Errorf("Expected (add 3 4) to be 7 but was: %v", result)
	}
}

func TestBegin(t *testing.T) {
	interpreter := New()
	// (begin 1 2 3) should return 3
	result := interpreter.EvalGlobal(List(Symbol("begin"), float64(1), float64(2), float64(3)))
	if result != float64(3) {
		t.Errorf("Expected (begin 1 2 3) to be 3 but was: %v", result)
	}
}

func TestBeginWithSideEffects(t *testing.T) {
	interpreter := New()
	// (begin (define x 1) (define y 2) (+ x y))
	result := interpreter.EvalGlobal(List(Symbol("begin"),
		List(Symbol("define"), Symbol("x"), float64(1)),
		List(Symbol("define"), Symbol("y"), float64(2)),
		List(Symbol("+"), Symbol("x"), Symbol("y"))))
	if result != float64(3) {
		t.Errorf("Expected begin with defines to return 3 but was: %v", result)
	}
}

func TestClosureCapture(t *testing.T) {
	interpreter := New()
	// (define make-adder (lambda (n) (lambda (x) (+ n x))))
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("make-adder"),
		List(Symbol("lambda"), List(Symbol("n")),
			List(Symbol("lambda"), List(Symbol("x")),
				List(Symbol("+"), Symbol("n"), Symbol("x"))))))
	// (define add5 (make-adder 5))
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("add5"),
		List(Symbol("make-adder"), float64(5))))
	// (add5 10) should return 15
	result := interpreter.EvalGlobal(List(Symbol("add5"), float64(10)))
	if result != float64(15) {
		t.Errorf("Expected (add5 10) to be 15 but was: %v", result)
	}
}
