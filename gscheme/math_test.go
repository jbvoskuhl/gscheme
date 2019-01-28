package gscheme

import "testing"

func TestArithmetic(t *testing.T) {
	scheme := New()
	expression := List(
		Symbol("+"),
		float64(2),
		List(Symbol("-"), float64(5), float64(3)),
		List(Symbol("/"), float64(4), float64(2)),
		List(Symbol("*"), float64(2), float64(1)))
	result := scheme.EvalGlobal(expression)
	if float64(8.0) != result {
		t.Errorf("Expected (+ 2 (- 5 3) (/ 4 2) (* 2 1)) to evaluate to 8 but was: %v.", result)
	}
}
