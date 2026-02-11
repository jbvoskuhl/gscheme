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

func TestIfTrue(t *testing.T) {
	interpreter := New()
	// (if #t 1 2) should return 1
	result := interpreter.EvalGlobal(List(Symbol("if"), true, float64(1), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (if #t 1 2) to be 1 but was: %v", result)
	}
}

func TestIfFalse(t *testing.T) {
	interpreter := New()
	// (if #f 1 2) should return 2
	result := interpreter.EvalGlobal(List(Symbol("if"), false, float64(1), float64(2)))
	if result != float64(2) {
		t.Errorf("Expected (if #f 1 2) to be 2 but was: %v", result)
	}
}

func TestIfNoAlternate(t *testing.T) {
	interpreter := New()
	// (if #f 1) should return nil when condition is false and no alternate
	result := interpreter.EvalGlobal(List(Symbol("if"), false, float64(1)))
	if result != nil {
		t.Errorf("Expected (if #f 1) to be nil but was: %v", result)
	}
}

func TestIfTruthyValue(t *testing.T) {
	interpreter := New()
	// (if 0 1 2) should return 1 (0 is truthy in Scheme)
	result := interpreter.EvalGlobal(List(Symbol("if"), float64(0), float64(1), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (if 0 1 2) to be 1 but was: %v", result)
	}
}

func TestIfNilIsTruthy(t *testing.T) {
	interpreter := New()
	// (if '() 1 2) should return 1 (empty list is truthy in Scheme)
	result := interpreter.EvalGlobal(List(Symbol("if"),
		List(Symbol("quote"), nil), float64(1), float64(2)))
	if result != float64(1) {
		t.Errorf("Expected (if '() 1 2) to be 1 but was: %v", result)
	}
}

func TestIfWithExpression(t *testing.T) {
	interpreter := New()
	// (if (not #f) (* 2 3) (+ 1 1)) should return 6
	result := interpreter.EvalGlobal(List(Symbol("if"),
		List(Symbol("not"), false),
		List(Symbol("*"), float64(2), float64(3)),
		List(Symbol("+"), float64(1), float64(1))))
	if result != float64(6) {
		t.Errorf("Expected (if (not #f) (* 2 3) (+ 1 1)) to be 6 but was: %v", result)
	}
}

func TestIfOnlyEvaluatesSelectedBranch(t *testing.T) {
	interpreter := New()
	// Define a counter
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("x"), float64(0)))
	// (if #t (define x 1) (define x 2)) should only set x to 1
	interpreter.EvalGlobal(List(Symbol("if"), true,
		List(Symbol("define"), Symbol("x"), float64(1)),
		List(Symbol("define"), Symbol("x"), float64(2))))
	result := interpreter.EvalGlobal(Symbol("x"))
	if result != float64(1) {
		t.Errorf("Expected x to be 1 but was: %v", result)
	}
}

func TestIfNestedConditional(t *testing.T) {
	interpreter := New()
	// (if #t (if #f 1 2) 3) should return 2
	result := interpreter.EvalGlobal(List(Symbol("if"), true,
		List(Symbol("if"), false, float64(1), float64(2)),
		float64(3)))
	if result != float64(2) {
		t.Errorf("Expected nested if to be 2 but was: %v", result)
	}
}

func TestSetBasic(t *testing.T) {
	interpreter := New()
	// (define x 1)
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("x"), float64(1)))
	// (set! x 2)
	interpreter.EvalGlobal(List(Symbol("set!"), Symbol("x"), float64(2)))
	// x should now be 2
	result := interpreter.EvalGlobal(Symbol("x"))
	if result != float64(2) {
		t.Errorf("Expected x to be 2 after set! but was: %v", result)
	}
}

func TestSetReturnsValue(t *testing.T) {
	interpreter := New()
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("x"), float64(1)))
	// set! should return the new value
	result := interpreter.EvalGlobal(List(Symbol("set!"), Symbol("x"), float64(42)))
	if result != float64(42) {
		t.Errorf("Expected set! to return 42 but was: %v", result)
	}
}

func TestSetInClosure(t *testing.T) {
	interpreter := New()
	// Create a counter using closures
	// (define counter 0)
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("counter"), float64(0)))
	// (define (increment) (set! counter (+ counter 1)))
	interpreter.EvalGlobal(List(Symbol("define"),
		List(Symbol("increment")),
		List(Symbol("set!"), Symbol("counter"),
			List(Symbol("+"), Symbol("counter"), float64(1)))))
	// (increment) three times
	interpreter.EvalGlobal(List(Symbol("increment")))
	interpreter.EvalGlobal(List(Symbol("increment")))
	interpreter.EvalGlobal(List(Symbol("increment")))
	// counter should be 3
	result := interpreter.EvalGlobal(Symbol("counter"))
	if result != float64(3) {
		t.Errorf("Expected counter to be 3 but was: %v", result)
	}
}

func TestCondBasic(t *testing.T) {
	interpreter := New()
	// (cond (#f 1) (#t 2) (else 3)) should return 2
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(false, float64(1)),
		List(true, float64(2)),
		List(Symbol("else"), float64(3))))
	if result != float64(2) {
		t.Errorf("Expected (cond (#f 1) (#t 2) (else 3)) to be 2 but was: %v", result)
	}
}

func TestCondElse(t *testing.T) {
	interpreter := New()
	// (cond (#f 1) (#f 2) (else 3)) should return 3
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(false, float64(1)),
		List(false, float64(2)),
		List(Symbol("else"), float64(3))))
	if result != float64(3) {
		t.Errorf("Expected cond with else to return 3 but was: %v", result)
	}
}

func TestCondNoMatch(t *testing.T) {
	interpreter := New()
	// (cond (#f 1) (#f 2)) should return #f when nothing matches
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(false, float64(1)),
		List(false, float64(2))))
	if result != false {
		t.Errorf("Expected cond with no match to return #f but was: %v", result)
	}
}

func TestCondMultipleExpressions(t *testing.T) {
	interpreter := New()
	// (cond (#t 1 2 3)) should return 3 (last expression)
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(true, float64(1), float64(2), float64(3))))
	if result != float64(3) {
		t.Errorf("Expected cond with multiple expressions to return 3 but was: %v", result)
	}
}

func TestCondArrow(t *testing.T) {
	interpreter := New()
	// (cond (5 => (lambda (x) (* x 2)))) should return 10
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(float64(5), Symbol("=>"),
			List(Symbol("lambda"), List(Symbol("x")),
				List(Symbol("*"), Symbol("x"), float64(2))))))
	if result != float64(10) {
		t.Errorf("Expected cond with => to return 10 but was: %v", result)
	}
}

func TestCondTestOnly(t *testing.T) {
	interpreter := New()
	// (cond (5)) should return 5 (the test value itself)
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(float64(5))))
	if result != float64(5) {
		t.Errorf("Expected (cond (5)) to return 5 but was: %v", result)
	}
}

func TestCondWithExpressions(t *testing.T) {
	interpreter := New()
	// (cond ((> 3 2) 'greater) ((< 3 2) 'less)) should return 'greater
	result := interpreter.EvalGlobal(List(Symbol("cond"),
		List(List(Symbol(">"), float64(3), float64(2)),
			List(Symbol("quote"), Symbol("greater"))),
		List(List(Symbol("<"), float64(3), float64(2)),
			List(Symbol("quote"), Symbol("less")))))
	if result != Symbol("greater") {
		t.Errorf("Expected cond to return 'greater but was: %v", result)
	}
}
