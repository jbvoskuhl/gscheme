package gscheme

import "testing"

// --- or tests ---

func TestOrNoArgs(t *testing.T) {
	s := New()
	result := evalScheme(s, `(or)`)
	if result != false {
		t.Errorf("(or) should be #f, got %v", result)
	}
}

func TestOrSingleArg(t *testing.T) {
	s := New()
	result := evalScheme(s, `(or 42)`)
	if result != int64(42) {
		t.Errorf("(or 42) should be 42, got %v", result)
	}
}

func TestOrReturnFirstTruthy(t *testing.T) {
	s := New()
	result := evalScheme(s, `(or #f 1 2)`)
	if result != int64(1) {
		t.Errorf("(or #f 1 2) should be 1, got %v", result)
	}
}

func TestOrAllFalse(t *testing.T) {
	s := New()
	result := evalScheme(s, `(or #f #f #f)`)
	if result != false {
		t.Errorf("(or #f #f #f) should be #f, got %v", result)
	}
}

func TestOrLastArgTailPosition(t *testing.T) {
	s := New()
	result := evalScheme(s, `(or #f #f (begin 42))`)
	if result != int64(42) {
		t.Errorf("(or #f #f (begin 42)) should be 42, got %v", result)
	}
}

func TestOrTailCallOptimization(t *testing.T) {
	// This would stack overflow without proper TCO in the last argument position
	s := New()
	evalScheme(s, `(define (loop n) (or (= n 0) (loop (- n 1))))`)
	result := evalScheme(s, `(loop 100000)`)
	if result != true {
		t.Errorf("or TCO loop should return #t, got %v", result)
	}
}

// --- raise tests ---

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

// --- guard tests ---

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

func TestGuardCatchRaise(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e (#t e)) (raise 42))`)
	if result != int64(42) {
		t.Errorf("guard should catch raised 42, got %v (%T)", result, result)
	}
}

func TestGuardNoError(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e (#t 'caught)) 99)`)
	if result != int64(99) {
		t.Errorf("guard with no error should return body result 99, got %v", result)
	}
}

func TestGuardElseClause(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e (else 'caught)) (error "test"))`)
	if result != Symbol("caught") {
		t.Errorf("guard with else clause should return 'caught, got %v", result)
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

func TestGuardMultipleClauses(t *testing.T) {
	s := New()
	result := evalScheme(s, `(guard (e
                                 ((number? e) 'number)
                                 ((string? e) 'string)
                                 (else 'other))
                               (raise "hello"))`)
	if result != Symbol("string") {
		t.Errorf("guard with multiple clauses should match string, got %v", result)
	}
}

func TestGuardTailPosition(t *testing.T) {
	// The cond clause body should be in tail position
	s := New()
	evalScheme(s, `(define (loop n)
                     (guard (e (#t e))
                       (if (= n 0) (raise 'done) (loop (- n 1)))))`)
	result := evalScheme(s, `(loop 100000)`)
	if result != Symbol("done") {
		t.Errorf("guard tail position test should return 'done, got %v", result)
	}
}
