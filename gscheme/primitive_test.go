package gscheme

import "testing"

func TestPrimitiveSeven(t *testing.T) {
	scheme := New()
	primitive := NewPrimitive(
		Symbol("seven"),
		0,
		0,
		func(args interface{}) interface{} {
			return 7
		})
	result := primitive.Apply(scheme, nil, nil)
	if result != 7 {
		t.Error("Primitive seven didn't evaluate to 7.")
	}
}

func TestMathPrimitives(t *testing.T) {
	scheme := New()
	symbols := []Symbol{"*", "+", "-", "/"}
	for _, symbol := range symbols {
		primitive := scheme.EvalGlobal(symbol)
		procedure, ok := primitive.(Procedure)
		if !ok {
			t.Errorf("Symbol did not evaluate to a callable object instead was %v.", primitive)
		}
		args := List(float64(4), float64(7))
		result := procedure.Apply(scheme, args, scheme.Environment())
		if _, ok := result.(float64); !ok {
			t.Errorf("Math primitive did not result in a number instead got: %v.", result)
		}
	}
}
