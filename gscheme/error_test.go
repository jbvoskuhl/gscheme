package gscheme

import (
	"reflect"
	"testing"
)

func TestNewError(t *testing.T) {
	e := NewError("message", List(1, 2, 3))
	if e.GetMessage() != "message" {
		t.Errorf("Error message was expected to be 'message' and instead was: %v.", e.GetMessage())
	}
	if e.String() != "message 1 2 3" {
		t.Errorf("Error as string was expected to be 'message 1 2 3' and instead was: %v.", e.String())
	}
	if e.Error() != "message 1 2 3" {
		t.Errorf("Scheme error as error was expected to be 'message 1 2 3' and instead was: %v.", e.Error())
	}
	if !reflect.DeepEqual(e.GetIrritants(), List(1, 2, 3)) {
		t.Errorf("Error irritants were expected to be (1 2 3) and instead was: %v.", e.GetIrritants())
	}
}

func TestErr(t *testing.T) {
	defer func() {
		recovered := recover()
		if recovered == nil {
			t.Error("Expected to see a panic occur via Err.")
		}
		e, ok := recovered.(Error)
		if !ok {
			t.Errorf("Error recovered from panic was not of type Error: %v.", e)
		}
		if e.GetMessage() != "message" {
			t.Errorf("Error message recovered from panic was not 'message': %v.", e.GetMessage())
		}
	}()
	Err("message", nil)
}

func TestErrorExpression(t *testing.T) {
	interpreter := New()
	expression := List(45) // This is an error because 45 is not callable.
	result := interpreter.EvalGlobal(expression)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected an error object but instead was: %v", result)
	}
}

func TestReadErrorPredicate(t *testing.T) {
	s := New()

	// read-error? returns #t for a read error object
	re := NewReadError("bad input", nil)
	s.Environment().Define(Symbol("re"), re)
	result := evalScheme(s, `(read-error? re)`)
	if result != true {
		t.Errorf("Expected (read-error? re) to be #t for read error, got %v", result)
	}

	// read-error? returns #f for a regular error
	result = evalScheme(s, `(guard (e (#t (read-error? e))) (error "msg"))`)
	if result != false {
		t.Errorf("Expected (read-error? e) to be #f for regular error, got %v", result)
	}

	// read-error? returns #f for a non-error
	result = evalScheme(s, `(read-error? 42)`)
	if result != false {
		t.Errorf("Expected (read-error? 42) to be #f, got %v", result)
	}
}

func TestFileErrorPredicate(t *testing.T) {
	s := New()

	// file-error? returns #t for a file error object
	fe := NewFileError("no such file", nil)
	s.Environment().Define(Symbol("fe"), fe)
	result := evalScheme(s, `(file-error? fe)`)
	if result != true {
		t.Errorf("Expected (file-error? fe) to be #t for file error, got %v", result)
	}

	// file-error? returns #f for a regular error
	result = evalScheme(s, `(guard (e (#t (file-error? e))) (error "msg"))`)
	if result != false {
		t.Errorf("Expected (file-error? e) to be #f for regular error, got %v", result)
	}

	// file-error? returns #f for a non-error
	result = evalScheme(s, `(file-error? 42)`)
	if result != false {
		t.Errorf("Expected (file-error? 42) to be #f, got %v", result)
	}
}

func TestNewReadError(t *testing.T) {
	e := NewReadError("bad read", nil)
	if !e.IsReadError() {
		t.Error("Expected IsReadError() to be true for NewReadError")
	}
	if e.IsFileError() {
		t.Error("Expected IsFileError() to be false for NewReadError")
	}
}

func TestNewFileError(t *testing.T) {
	e := NewFileError("bad file", nil)
	if !e.IsFileError() {
		t.Error("Expected IsFileError() to be true for NewFileError")
	}
	if e.IsReadError() {
		t.Error("Expected IsReadError() to be false for NewFileError")
	}
}

func TestGeneralErrorIsNeitherReadNorFile(t *testing.T) {
	e := NewError("general", nil)
	if e.IsReadError() {
		t.Error("Expected IsReadError() to be false for general error")
	}
	if e.IsFileError() {
		t.Error("Expected IsFileError() to be false for general error")
	}
}
