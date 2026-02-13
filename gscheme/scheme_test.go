package gscheme

import (
	"reflect"
	"testing"
)

func TestEvalConstants(t *testing.T) {
	scheme := New()
	result := scheme.EvalGlobal(42)
	if result != 42 {
		t.Errorf("Expected 42 but instead got: %v", result)
	}
}

func TestEvalSymbol(t *testing.T) {
	scheme := New()
	environment := NewRootEnvironment()
	environment.Define(Symbol("hello"), float64(3))
	result := scheme.Eval(Symbol("hello"), environment)
	if result != float64(3) {
		t.Errorf("Expected 3 but instead got: %v", result)
	}
	result = scheme.Eval(Symbol("goodbye"), environment)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected an error evaluating unbound symbol but instead got: %v", result)
	}
}

func TestEvalList(t *testing.T) {
	scheme := New()
	environment := NewRootEnvironment()
	environment.Define(Symbol("x"), float64(2))
	list := NewPair(float64(1), Cons(Symbol("x"), Cons(float64(3), nil)))
	result := scheme.EvalList(list, environment)
	expected := Cons(float64(1), Cons(float64(2), Cons(float64(3), nil)))
	if !reflect.DeepEqual(expected, result) {
		t.Errorf("EvalList of (1 x 3) did not produce (1 2 3) in an environment where x is bound to 2.")
	}
}

func TestTimes(t *testing.T) {
	scheme := New()
	six := Cons(Symbol("*"), Cons(float64(3), Cons(float64(2), nil)))
	result := scheme.EvalGlobal(six)
	if result != float64(6) {
		t.Errorf("Expected 6 but instead got: %v", result)
	}
}

func TestPlus(t *testing.T) {
	scheme := New()
	five := Cons(Symbol("+"), Cons(float64(3), Cons(float64(2), nil)))
	result := scheme.EvalGlobal(five)
	if result != float64(5) {
		t.Errorf("Expected (+ 3 2) => 5 but instead got: %v", result)
	}
}

func TestMinus(t *testing.T) {
	scheme := New()
	negativeOne := Cons(Symbol("-"), Cons(float64(2), Cons(float64(3), nil)))
	result := scheme.EvalGlobal(negativeOne)
	if result != float64(-1) {
		t.Errorf("Expected (- 2 3) => -1 but instead got: %v", result)
	}
}

func TestReciprocal(t *testing.T) {
	scheme := New()
	half := Cons(Symbol("/"), Cons(float64(2), nil))
	result := scheme.EvalGlobal(half)
	if result != float64(0.5) {
		t.Errorf("Expected (/ 2) => 0.5 but instead got: %v", result)
	}
}

func TestDivide(t *testing.T) {
	scheme := New()
	half := Cons(Symbol("/"), Cons(float64(2), Cons(float64(4), nil)))
	result := scheme.EvalGlobal(half)
	if result != float64(0.5) {
		t.Errorf("Expected (/ 2 4) => 0.5 but instead got: %v", result)
	}
}

func TestNegate(t *testing.T) {
	scheme := New()
	negativeFour := Cons(Symbol("-"), Cons(float64(4), nil))
	result := scheme.EvalGlobal(negativeFour)
	if result != float64(-4) {
		t.Errorf("Expected (- 4) => -4 but instead got: %v", result)
	}
}

func TestBasicMath(t *testing.T) {
	scheme := New()
	three := Cons(Symbol("+"), Cons(float64(2), Cons(float64(1), nil)))
	nine := Cons(Symbol("*"), Cons(three, Cons(float64(3), nil)))
	result := scheme.EvalGlobal(nine)
	if result != float64(9) {
		t.Errorf("Expected (* (+ 2 1) 3) => 9 but instead got: %v", result)
	}
}

// evalScheme is a helper that parses and evaluates a Scheme expression string.
func evalScheme(s Scheme, code string) interface{} {
	input := NewInputPortFromString(code)
	var result interface{}
	for {
		x := input.Read()
		if IsEOF(x) {
			return result
		}
		result = s.EvalGlobal(x)
	}
}

func TestTailRecursionIf(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n) (if (= n 0) 'done (loop (- n 1))))`)
	result := evalScheme(s, `(loop 1000000)`)
	if result != Symbol("done") {
		t.Errorf("Expected 'done from deep tail recursion via if, got: %v", result)
	}
}

func TestTailRecursionBegin(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n) (begin (if (= n 0) 'done (loop (- n 1)))))`)
	result := evalScheme(s, `(loop 1000000)`)
	if result != Symbol("done") {
		t.Errorf("Expected 'done from deep tail recursion via begin, got: %v", result)
	}
}

func TestTailRecursionCond(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n) (cond ((= n 0) 'done) (else (loop (- n 1)))))`)
	result := evalScheme(s, `(loop 1000000)`)
	if result != Symbol("done") {
		t.Errorf("Expected 'done from deep tail recursion via cond, got: %v", result)
	}
}

func TestMutualTailRecursion(t *testing.T) {
	s := New()
	evalScheme(s, `
		(define (my-even? n) (if (= n 0) #t (my-odd? (- n 1))))
		(define (my-odd? n) (if (= n 0) #f (my-even? (- n 1))))
	`)
	result := evalScheme(s, `(my-even? 100000)`)
	if result != true {
		t.Errorf("Expected #t from mutual tail recursion (even? 100000), got: %v", result)
	}
	result = evalScheme(s, `(my-odd? 100001)`)
	if result != true {
		t.Errorf("Expected #t from mutual tail recursion (odd? 100001), got: %v", result)
	}
}
