package gscheme

import "testing"

func TestEmptyList(t *testing.T) {
	list := List()
	if Stringify(list) != "()" {
		t.Errorf("The empty list doesn't stringify as () instead it was: %v.", Stringify(list))
	}
}

func TestSimpleList(t *testing.T) {
	list := List(1, 2, 3)
	if Stringify(list) != "(1 2 3)" {
		t.Errorf("A simple list doesn't stringify as (1 2 3) instead it was: %v.", Stringify(list))
	}
}

func TestDottedList(t *testing.T) {
	list := Cons(1, 2)
	if Stringify(list) != "(1 . 2)" {
		t.Errorf("A dotted list doesn't stringify as (1 . 2) instead it was: %v.", Stringify(list))
	}
}

func TestNestedList(t *testing.T) {
	list := List(List(1, false), List(true, 4))
	if Stringify(list) != "((1 #f) (#t 4))" {
		t.Errorf("A nested list doesn't stringify as ((1 #f) (#t 4)) instead it was: %v.", Stringify(list))
	}
}

func TestString(t *testing.T) {
	s := "string"
	if Stringify(s) != "\"string\"" {
		t.Errorf("A string doesn't stringify as \"string\" instead it was: %v.", Stringify(s))
	}
}

func TestNumber(t *testing.T) {
	num := 42.42
	if Stringify(num) != "42.42" {
		t.Errorf("A number doesn't stringify as 42.42 instead it was: %v.", Stringify(num))
	}
}
