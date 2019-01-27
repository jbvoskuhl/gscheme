package gscheme

import "testing"

func TestNewEnvironment(t *testing.T) {
	e := NewRootEnvironment()
	e.Define("hello", 56)
	if value, _ := e.Lookup("hello"); value != 56 {
		t.Error("Environment wasn't able to lookup a defined value.")
	}
}

func TestEnvironmentBadLookup(t *testing.T) {
	e := NewRootEnvironment()
	e.Define("hello", 56)
	_, ok := e.Lookup("goodbye")
	if ok {
		t.Error("We got ok for a bad Lookup call.")
	}
}

func TestNestedEnvironment(t *testing.T) {
	root := NewRootEnvironment()
	root.Define("x", 42)
	root.Define("y", 43)
	nested := NewChildEnvironment(root)
	nested.Define("x", 0)
	if value, _ := nested.Lookup("x"); value != 0 {
		t.Error("The nested environment should hide x value of 42.")
	}
	nested.Set("x", 1)
	if value, _ := nested.Lookup("x"); value != 1 {
		t.Error("After setting x to 2 the nested environment should lookup x as 1.")
	}
	if value, _ := nested.Lookup("y"); value != 43 {
		t.Error("The nested environment should lookup y as 43.")
	}
	if value, _ := root.Lookup("x"); value != 42 {
		t.Error("The root environment should still contain x as 0.")
	}
}

func TestBadSet(t *testing.T) {
	root := NewRootEnvironment()
	ok := root.Set("x", 42)
	if ok {
		t.Error("Should not be able to set an unbound variable.")
	}
}
