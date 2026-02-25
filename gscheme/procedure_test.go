package gscheme

import "testing"

func TestProcedure(t *testing.T) {
	proc := NewProcedure()
	if proc.Name() != "" {
		t.Errorf("New procedures should have an empty name, got %q.", proc.Name())
	}
	if proc.String() != "#<procedure>" {
		t.Errorf("Anonymous procedures should print as '#<procedure>', got %q.", proc.String())
	}
}

func TestNames(t *testing.T) {
	proc := NewProcedure()
	proc.SetName("name")
	if proc.Name() != "name" {
		t.Errorf("Unable to set the name of a procedure: %v.", proc.Name())
	}
}
