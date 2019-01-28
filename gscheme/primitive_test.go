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
