package gscheme

import "testing"

// --- when tests ---

func TestWhenTrue(t *testing.T) {
	s := New()
	result := evalScheme(s, `(when #t 1 2 3)`)
	if result != int64(3) {
		t.Errorf("(when #t 1 2 3) should be 3, got %v", result)
	}
}

func TestWhenFalse(t *testing.T) {
	s := New()
	result := evalScheme(s, `(when #f 1 2 3)`)
	if result != nil {
		t.Errorf("(when #f ...) should be nil, got %v", result)
	}
}

func TestWhenTailCallOptimization(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n) (when (> n 0) (loop (- n 1))))`)
	result := evalScheme(s, `(loop 100000)`)
	// when test is false returns nil; last iteration has n=0 so (> 0 0) is #f
	if result != nil {
		t.Errorf("when TCO loop should return nil, got %v", result)
	}
}

// --- unless tests ---

func TestUnlessTrue(t *testing.T) {
	s := New()
	result := evalScheme(s, `(unless #t 1 2 3)`)
	if result != false {
		t.Errorf("(unless #t ...) should be #f, got %v", result)
	}
}

func TestUnlessFalse(t *testing.T) {
	s := New()
	result := evalScheme(s, `(unless #f 1 2 3)`)
	if result != int64(3) {
		t.Errorf("(unless #f 1 2 3) should be 3, got %v", result)
	}
}

func TestUnlessTailCallOptimization(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n) (unless (= n 0) (loop (- n 1))))`)
	result := evalScheme(s, `(loop 100000)`)
	// when n=0, test is true so unless returns #f
	if result != false {
		t.Errorf("unless TCO loop should return #f, got %v", result)
	}
}

// --- case tests ---

func TestCaseMatchFirst(t *testing.T) {
	s := New()
	result := evalScheme(s, `(case 1 ((1) 'one) ((2) 'two) (else 'other))`)
	if result != Symbol("one") {
		t.Errorf("(case 1 ...) should be 'one, got %v", result)
	}
}

func TestCaseMatchSecond(t *testing.T) {
	s := New()
	result := evalScheme(s, `(case 2 ((1) 'one) ((2) 'two) (else 'other))`)
	if result != Symbol("two") {
		t.Errorf("(case 2 ...) should be 'two, got %v", result)
	}
}

func TestCaseElse(t *testing.T) {
	s := New()
	result := evalScheme(s, `(case 3 ((1) 'one) ((2) 'two) (else 'other))`)
	if result != Symbol("other") {
		t.Errorf("(case 3 ...) should be 'other, got %v", result)
	}
}

func TestCaseMultipleValues(t *testing.T) {
	s := New()
	result := evalScheme(s, `(case 2 ((1 2 3) 'low) ((4 5 6) 'high))`)
	if result != Symbol("low") {
		t.Errorf("(case 2 ((1 2 3) ...)) should be 'low, got %v", result)
	}
}

func TestCaseTailCallOptimization(t *testing.T) {
	s := New()
	evalScheme(s, `(define (loop n)
                     (case (= n 0)
                       ((#t) 'done)
                       (else (loop (- n 1)))))`)
	result := evalScheme(s, `(loop 100000)`)
	if result != Symbol("done") {
		t.Errorf("case TCO loop should return 'done, got %v", result)
	}
}

// --- do tests ---

func TestDoBasic(t *testing.T) {
	s := New()
	result := evalScheme(s, `(do ((i 0 (+ i 1)))
                               ((= i 5) i))`)
	if result != int64(5) {
		t.Errorf("(do ...) should return 5, got %v", result)
	}
}

func TestDoMultipleBindings(t *testing.T) {
	s := New()
	result := evalScheme(s, `(do ((i 0 (+ i 1))
                                 (sum 0 (+ sum i)))
                               ((= i 5) sum))`)
	if result != int64(10) {
		t.Errorf("(do ...) should return 10 (0+1+2+3+4), got %v", result)
	}
}

func TestDoWithBody(t *testing.T) {
	s := New()
	result := evalScheme(s, `(let ((x 0))
                               (do ((i 0 (+ i 1)))
                                   ((= i 3) x)
                                 (set! x (+ x 10))))`)
	if result != int64(30) {
		t.Errorf("(do ...) with body should return 30, got %v", result)
	}
}

func TestDoTailCallOptimization(t *testing.T) {
	s := New()
	result := evalScheme(s, `(do ((i 0 (+ i 1)))
                               ((= i 100000) 'done))`)
	if result != Symbol("done") {
		t.Errorf("do TCO loop should return 'done, got %v", result)
	}
}

// --- and tests ---

func TestAndNoArgs(t *testing.T) {
	s := New()
	result := evalScheme(s, `(and)`)
	if result != true {
		t.Errorf("(and) should be #t, got %v", result)
	}
}

func TestAndSingleArg(t *testing.T) {
	s := New()
	result := evalScheme(s, `(and 42)`)
	if result != int64(42) {
		t.Errorf("(and 42) should be 42, got %v", result)
	}
}

func TestAndReturnsLastTruthy(t *testing.T) {
	s := New()
	result := evalScheme(s, `(and 1 2 3)`)
	if result != int64(3) {
		t.Errorf("(and 1 2 3) should be 3, got %v", result)
	}
}

func TestAndShortCircuits(t *testing.T) {
	s := New()
	result := evalScheme(s, `(and 1 #f 3)`)
	if result != false {
		t.Errorf("(and 1 #f 3) should be #f, got %v", result)
	}
}

func TestAndLastArgTailPosition(t *testing.T) {
	s := New()
	result := evalScheme(s, `(and #t #t (begin 42))`)
	if result != int64(42) {
		t.Errorf("(and #t #t (begin 42)) should be 42, got %v", result)
	}
}

func TestAndTailCallOptimization(t *testing.T) {
	// This would stack overflow without proper TCO in the last argument position
	s := New()
	evalScheme(s, `(define (loop n) (and (> n 0) (loop (- n 1))))`)
	result := evalScheme(s, `(loop 100000)`)
	if result != false {
		t.Errorf("and TCO loop should return #f, got %v", result)
	}
}

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
