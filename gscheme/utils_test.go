package gscheme

import "testing"

func TestTruth(t *testing.T) {
	if false != Truth(false) {
		t.Errorf("Truth of false should be false.")
	}
	if true != Truth(true) {
		t.Errorf("Truth of true should be true.")
	}
	if true != Truth(nil) {
		t.Errorf("Truth of non-#f should be true.")
	}
}

func TestNum(t *testing.T) {
	values := []interface{}{
		float64(0.0),
		float32(0.0),
		int64(0),
		int32(0),
		int16(0),
		int8(0),
		uint64(0),
		uint32(0),
		uint16(0),
		uint8(0)}
	for _, value := range values {
		if float64(0.0) != Num(value) {
			t.Errorf("Num could not coerce value to float64: %v.", value)
		}
	}
}
