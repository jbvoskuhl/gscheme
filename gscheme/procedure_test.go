package gscheme

import "testing"

func TestProcedure(t *testing.T) {
	proc := NewProcedure()
	if proc.Name() != "anonymous procedure" {
		t.Errorf("New procedures should have a name 'anonymous procedure'.")
	}
	if proc.String() != "{anonymous procedure}" {
		t.Errorf("New procedures should print as '{anonymous procedure}'.")
	}
}

func TestNames(t *testing.T) {
	proc := NewProcedure()
	proc.SetName("name")
	if proc.Name() != "name" {
		t.Errorf("Unable to set the name of a procedure: %v.", proc.Name())
	}
}
