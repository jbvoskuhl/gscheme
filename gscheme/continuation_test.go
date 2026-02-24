package gscheme

import "testing"

func TestContinuationBasicEscape(t *testing.T) {
	s := New()
	result := evalScheme(s, `(call/cc (lambda (k) (k 42) 99))`)
	if result != int64(42) {
		t.Errorf("Expected 42 but got: %v", result)
	}
}

func TestContinuationNoEscape(t *testing.T) {
	s := New()
	result := evalScheme(s, `(call/cc (lambda (k) 99))`)
	if result != int64(99) {
		t.Errorf("Expected 99 but got: %v", result)
	}
}

func TestContinuationEscapeFromNestedContext(t *testing.T) {
	s := New()
	result := evalScheme(s, `(+ 1 (call/cc (lambda (k) (+ 2 (k 42)))))`)
	if result != int64(43) {
		t.Errorf("Expected 43 but got: %v", result)
	}
}

func TestContinuationFullName(t *testing.T) {
	s := New()
	result := evalScheme(s, `(call-with-current-continuation (lambda (k) (k 42) 99))`)
	if result != int64(42) {
		t.Errorf("Expected 42 but got: %v", result)
	}
}

func TestContinuationIsProcedure(t *testing.T) {
	s := New()
	result := evalScheme(s, `(procedure? (call/cc (lambda (k) k)))`)
	if result != true {
		t.Errorf("Expected #t but got: %v", result)
	}
}

func TestContinuationEscapeFromMap(t *testing.T) {
	s := New()
	result := evalScheme(s, `(call/cc (lambda (k) (map (lambda (x) (if (= x 3) (k 'found) x)) '(1 2 3 4))))`)
	if result != Symbol("found") {
		t.Errorf("Expected found but got: %v", result)
	}
}

func TestContinuationNonProcedureError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(call/cc 42)`)
	if _, ok := result.(Error); !ok {
		t.Errorf("Expected error for non-procedure argument but got: %v", result)
	}
}
