package gscheme

import "testing"

func TestValuesTypes(t *testing.T) {
	s := New()

	// (values) must return an empty MultipleValues (not nil or some other type)
	result := evalScheme(s, `(values)`)
	mv, ok := result.(MultipleValues)
	if !ok {
		t.Fatalf("(values) should return MultipleValues, got %T: %v", result, result)
	}
	if len(mv) != 0 {
		t.Errorf("(values) should return empty MultipleValues, got length %d", len(mv))
	}

	// (values x) must pass through x unchanged — not wrapped in MultipleValues
	result = evalScheme(s, `(values 42)`)
	if _, ok := result.(MultipleValues); ok {
		t.Errorf("(values 42) should not return MultipleValues, got %T", result)
	}

	// (values v1 v2 ...) must return a MultipleValues, not a list
	result = evalScheme(s, `(values 1 2 3)`)
	mv, ok = result.(MultipleValues)
	if !ok {
		t.Fatalf("(values 1 2 3) should return MultipleValues, got %T: %v", result, result)
	}
	if len(mv) != 3 {
		t.Errorf("(values 1 2 3) should have length 3, got %d", len(mv))
	}
}

func TestValuesStringify(t *testing.T) {
	// Zero values stringifies to empty string (not "()")
	if got := Stringify(MultipleValues{}); got != "" {
		t.Errorf("Stringify of empty MultipleValues should be empty string, got %q", got)
	}

	// Multiple values are newline-separated
	mv := MultipleValues{int64(1), int64(2)}
	if got := Stringify(mv); got != "1\n2" {
		t.Errorf("Stringify of MultipleValues{1 2} should be \"1\\n2\", got %q", got)
	}
}
