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
	if e.String() != "message" {
		t.Errorf("Error as string was expected to be 'message' and instead was: %v.", e.String())
	}
	if e.Error() != "message" {
		t.Errorf("Scheme error as error was expected to be 'message' and instead was: %v.", e.Error())
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
