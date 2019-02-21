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

func makeListCycle(length uint) (result Pair) {
	result = NewPair(length, nil)
	tail := result
	for length > 0 {
		length--
		result = NewPair(length, result)
	}
	tail.SetRest(result)
	return
}

func TestPrimitiveListP(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("list?"), List(Symbol("quote"), List(1, 2, 3)))
	result := interpreter.EvalGlobal(expression)
	if result != true {
		t.Errorf("Expected the expression (list? '(1 2 3)) to evaluate to #t.")
	}
	expression = List(Symbol("list?"), Cons(1, 2))
	result = interpreter.EvalGlobal(expression)
	if result != false {
		t.Errorf("Expected the expression (list? (cons 1 2)) to evaluate to #f.")
	}
	expression = List(Symbol("list?"), nil)
	result = interpreter.EvalGlobal(expression)
	if result != true {
		t.Errorf("Expected the expression (list? ()) to evaluate to #t.")
	}
	expression = List(Symbol("list?"), 5)
	result = interpreter.EvalGlobal(expression)
	if result != false {
		t.Errorf("Expected the expression (list? 5) to evaluate to #f.")
	}
	expression = List(Symbol("list?"), List(Symbol("quote"), makeListCycle(3)))
	result = interpreter.EvalGlobal(expression)
	if result != false {
		t.Errorf("Expected the expression (list? <huge-loop>) to evaluate to #f.")
	}
}

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

func TestPrimitiveAppend(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("append"))
	result := interpreter.EvalGlobal(expression)
	if result != nil {
		t.Errorf("Expected (append) to evaluate to ().")
	}
	expression = List(Symbol("append"), nil)
	result = interpreter.EvalGlobal(expression)
	if result != nil {
		t.Errorf("Expected (append ()) to evaluate to ().")
	}
	expression = List(Symbol("append"), List(Symbol("quote"), 1, 2), List(Symbol("quote"), 3, 4))
	result = interpreter.EvalGlobal(expression)
	if reflect.DeepEqual(result, List(1, 2, 3, 4)) {
		t.Errorf("Expected (append '(1 2) '(3 4)) to evaluate to (1 2 3 4).")
	}
	expression = List(Symbol("append"), List(Symbol("quote"), 1, 2), 3)
	result = interpreter.EvalGlobal(expression)
	if reflect.DeepEqual(result, Cons(1, Cons(2, 3))) {
		t.Errorf("Expected (append '(1 2) 3) to evaluate to (1 2 . 3.")
	}
}
