package gscheme

import "testing"

func TestSymbolString(t *testing.T) {
	s := Symbol("car")
	if Stringify(s) != "car" {
		t.Errorf("A symbol car doesn't stringify as car instead it was: %v.", Stringify(s))
	}
}
