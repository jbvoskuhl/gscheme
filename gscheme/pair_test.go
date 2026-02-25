package gscheme

import (
	"reflect"
	"testing"
)

func TestPairString(t *testing.T) {
	list := NewPair(1, nil)
	if "(1)" != list.String() {
		t.Errorf("List of (1) does not print like \"(1)\" instead was: \"%s\".", list.String())
	}
}

func TestFirst(t *testing.T) {
	list := List(1, 2, 3)
	if First(list) != 1 {
		t.Errorf("Expected first element of the list to be 1 but was: %v.", First(list))
	}
	if First(42) != nil {
		t.Errorf("Expected first of non-pair to be nil but was %v.", First(42))
	}
}

func TestRest(t *testing.T) {
	list := List(1, 2, 3)
	rest := Rest(list)
	expected := List(2, 3)
	if !reflect.DeepEqual(expected, rest) {
		t.Errorf("Expected rest of the list to be (2 3) but was: %v.", rest)
	}
	if Rest(42) != nil {
		t.Errorf("Expected rest of a non-pair to be nil but was: %v.", Rest(42))
	}
}

func TestLast(t *testing.T) {
	list := List(1, 2, 3)
	last := Last(list)
	if 3 != last {
		t.Errorf("Expected the last element in the list to be 3 but was: %v.", last)
	}
	dotted := Cons(1, Cons(2, 3))
	last = Last(dotted)
	if last != nil {
		t.Errorf("Expected the last element in a dotted list to be 2 but was: %v.", last)
	}
	if Last(42) != nil {
		t.Errorf("Expected last of a non-pair to be nil but was: %v.", Last(42))
	}
}

func TestSet(t *testing.T) {
	list := List(1, 2, 3)
	SetRest(list, List(9))
	expected := List(1, 9)
	if !reflect.DeepEqual(expected, list) {
		t.Errorf("Expected the changed list to be (1 9) but instead it was: %v.", list)
	}
	SetFirst(list, 8)
	expected = List(8, 9)
	if !reflect.DeepEqual(expected, list) {
		t.Errorf("Expected the changed list to be (8 9) but instead it was: %v.", list)
	}
}

func TestSecondThird(t *testing.T) {
	list := List(1, 2, 3)
	if 2 != Second(list) {
		t.Errorf("Expected the second element in the list to be 2 but instead it was: %v.", Second(list))
	}
	if 3 != Third(list) {
		t.Errorf("Expected the third element in the list to be 3 but instead it was: %v.", Third(list))
	}
}

func TestCons(t *testing.T) {
	list := List(1, 2)
	consed := Cons(1, Cons(2, nil))
	if !reflect.DeepEqual(list, consed) {
		t.Errorf("The list (1 2) built via cons should also be (1 2) but instead it was: %v.", consed)
	}
}

func TestReverse(t *testing.T) {
	list := List(1, 2, 3)
	reverse := Reverse(list)
	expected := List(3, 2, 1)
	if !reflect.DeepEqual(expected, reverse) {
		t.Errorf("Expected to reverse list (1 2 3) to be (3 2 1) but instead it was: %v.", reverse)
	}
}

func TestPrimitiveCxr(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("caar"), List(Symbol("quote"), List(List(1, 2), 3)))
	result := interpreter.EvalGlobal(expression)
	if result != 1 {
		t.Errorf("Exprected the expression (caar '((1 2) 3) to evaluate to 1.")
	}
}

// TestPrimitiveListP has been moved to testdata/scheme-tests.scm.

func TestPrimitiveMakeList(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("make-list"), 4)
	result := interpreter.EvalGlobal(expression)
	if Len(result) != 4 {
		t.Errorf("Expected make-list to produce a four element list.")
	}
	if First(result) != nil {
		t.Errorf("Expected make-list to produce a list of null elements.")
	}
	expression = List(Symbol("make-list"), 1, 'a')
	result = interpreter.EvalGlobal(expression)
	if Len(result) != 1 {
		t.Errorf("Expected make-list to produce a one element list.")
	}
	if First(result) != 'a' {
		t.Errorf("Expected make-list to produce a list of 'a' elements.")
	}
}

func TestPrimitiveReverse(t *testing.T) {
	interpreter := New()
	// (reverse '(1 2 3)) => (3 2 1)
	result := interpreter.EvalGlobal(List(Symbol("reverse"),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	if First(result) != float64(3) || First(Rest(result)) != float64(2) || First(Rest(Rest(result))) != float64(1) {
		t.Errorf("Expected (3 2 1) but got: %v", result)
	}
	// (reverse '()) => ()
	result = interpreter.EvalGlobal(List(Symbol("reverse"),
		List(Symbol("quote"), nil)))
	if result != nil {
		t.Errorf("Expected () but got: %v", result)
	}
}

func TestPrimitiveListTail(t *testing.T) {
	interpreter := New()
	// (list-tail '(a b c d) 2) => (c d)
	result := interpreter.EvalGlobal(List(Symbol("list-tail"),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"))),
		float64(2)))
	if First(result) != Symbol("c") || First(Rest(result)) != Symbol("d") {
		t.Errorf("Expected (c d) but got: %v", result)
	}
	// (list-tail '(a b c) 0) => (a b c)
	result = interpreter.EvalGlobal(List(Symbol("list-tail"),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c"))),
		float64(0)))
	if First(result) != Symbol("a") {
		t.Errorf("Expected (a b c) but got: %v", result)
	}
}

func TestPrimitiveListRef(t *testing.T) {
	interpreter := New()
	// (list-ref '(a b c d) 2) => c
	result := interpreter.EvalGlobal(List(Symbol("list-ref"),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c"), Symbol("d"))),
		float64(2)))
	if result != Symbol("c") {
		t.Errorf("Expected c but got: %v", result)
	}
}

func TestPrimitiveListSet(t *testing.T) {
	interpreter := New()
	// (define x '(a b c)) (list-set! x 1 'd) x => (a d c)
	interpreter.EvalGlobal(List(Symbol("define"), Symbol("x"),
		List(Symbol("quote"), List(Symbol("a"), Symbol("b"), Symbol("c")))))
	interpreter.EvalGlobal(List(Symbol("list-set!"), Symbol("x"), float64(1), List(Symbol("quote"), Symbol("d"))))
	result := interpreter.EvalGlobal(Symbol("x"))
	if Second(result) != Symbol("d") {
		t.Errorf("Expected (a d c) but got: %v", result)
	}
}

func TestPrimitiveListCopy(t *testing.T) {
	interpreter := New()
	// (list-copy '(1 2 3)) => (1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("list-copy"),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != float64(1) || First(Rest(pair)) != float64(2) || First(Rest(Rest(pair))) != float64(3) {
		t.Errorf("Expected (1 2 3) but got: %v", result)
	}
	// (list-copy '()) => ()
	result = interpreter.EvalGlobal(List(Symbol("list-copy"),
		List(Symbol("quote"), nil)))
	if result != nil {
		t.Errorf("Expected () but got: %v", result)
	}
}

func TestPrimitiveAppend(t *testing.T) {
	s := New()
	// (append) => ()
	result := evalScheme(s, `(append)`)
	if result != nil {
		t.Errorf("Expected (append) to evaluate to (), got: %v", result)
	}
	// (append '()) => ()
	result = evalScheme(s, `(append '())`)
	if result != nil {
		t.Errorf("Expected (append '()) to evaluate to (), got: %v", result)
	}
	// (append '(1 2) '(3 4)) => (1 2 3 4)
	result = evalScheme(s, `(append '(1 2) '(3 4))`)
	if Stringify(result) != "(1 2 3 4)" {
		t.Errorf("Expected (1 2 3 4) but got: %v", Stringify(result))
	}
	// (append '(1 2) '(3 4) '(5 6)) => (1 2 3 4 5 6)
	result = evalScheme(s, `(append '(1 2) '(3 4) '(5 6))`)
	if Stringify(result) != "(1 2 3 4 5 6)" {
		t.Errorf("Expected (1 2 3 4 5 6) but got: %v", Stringify(result))
	}
	// (append '(a) '(b) 'c) => (a b . c) — improper tail
	result = evalScheme(s, `(append '(a) '(b) 'c)`)
	if Stringify(result) != "(a b . c)" {
		t.Errorf("Expected (a b . c) but got: %v", Stringify(result))
	}
	// (append '() '(1)) => (1)
	result = evalScheme(s, `(append '() '(1))`)
	if Stringify(result) != "(1)" {
		t.Errorf("Expected (1) but got: %v", Stringify(result))
	}
	// (append 'x) => x — single non-list arg returned as-is
	result = evalScheme(s, `(append 'x)`)
	if result != Symbol("x") {
		t.Errorf("Expected x but got: %v", result)
	}
}
