package gscheme

import "testing"

// Tests that require Go type introspection and cannot be expressed in Scheme.
// All other tailcall tests have been moved to testdata/scheme-tests.scm.

func TestErrorObjectMessageNonError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(error-object-message 42)`)
	if _, ok := result.(Error); !ok {
		t.Errorf("(error-object-message 42) should raise error, got %T: %v", result, result)
	}
}

func TestErrorObjectIrritantsNonError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(error-object-irritants 42)`)
	if _, ok := result.(Error); !ok {
		t.Errorf("(error-object-irritants 42) should raise error, got %T: %v", result, result)
	}
}

func TestRaiseInteger(t *testing.T) {
	s := New()
	result := evalScheme(s, `(raise 42)`)
	if raised, ok := result.(*raisedError); ok {
		if raised.value != int64(42) {
			t.Errorf("raise 42 should wrap 42, got %v", raised.value)
		}
	} else {
		t.Errorf("raise 42 should return raisedError, got %T: %v", result, result)
	}
}

func TestRaiseError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(raise (error "test"))`)
	err, ok := result.(Error)
	if !ok {
		t.Errorf("raise of error should return Error, got %T: %v", result, result)
	} else if err.GetMessage() != `"test"` {
		t.Errorf("raise of error message should be '\"test\"', got %v", err.GetMessage())
	}
}

func TestGuardCatchError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e (#t e)) (error "test"))`)
	err, ok := result.(Error)
	if !ok {
		t.Errorf("guard should catch error, got %T: %v", result, result)
	} else if err.GetMessage() != `"test"` {
		t.Errorf("guard caught error message should be '\"test\"', got %v", err.GetMessage())
	}
}

func TestGuardCondClauseBody(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e (#t (list 'caught e))) (raise 42))`)
	resultPair, ok := result.(Pair)
	if !ok {
		t.Errorf("guard cond clause body should return a pair, got %T: %v", result, result)
	} else if First(resultPair) != Symbol("caught") || Second(resultPair) != int64(42) {
		t.Errorf("guard cond clause body should return (caught 42), got %v", result)
	}
}

func TestGuardReRaise(t *testing.T) {
	s := New()
	// If no clause matches, the error is re-raised. The outer Eval catches it.
	result := evalScheme(s, `(guard (e (#f 'nope)) (error "test"))`)
	err, ok := result.(Error)
	if !ok {
		t.Errorf("guard re-raise should return Error, got %T: %v", result, result)
	} else if err.GetMessage() != `"test"` {
		t.Errorf("guard re-raised error message should be '\"test\"', got %v", err.GetMessage())
	}
}
