package gscheme

import "testing"

func TestCommandLine(t *testing.T) {
	s := New()
	result := evalScheme(s, `(command-line)`)
	// Should return a list (at minimum containing the test binary)
	if result == nil {
		t.Errorf("Expected non-nil result from (command-line)")
	}
	_, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected list from (command-line), got: %v (%T)", result, result)
	}
}

func TestGetEnvironmentVariable(t *testing.T) {
	s := New()
	// PATH should exist on any Unix system
	result := evalScheme(s, `(get-environment-variable "PATH")`)
	if result == false {
		t.Errorf("Expected PATH to be set")
	}
	if _, ok := result.(string); !ok {
		t.Errorf("Expected string, got: %v (%T)", result, result)
	}
	// Non-existent variable should return #f
	result = evalScheme(s, `(get-environment-variable "GSCHEME_NONEXISTENT_VAR_12345")`)
	if result != false {
		t.Errorf("Expected #f for non-existent variable, got: %v", result)
	}
}

func TestGetEnvironmentVariables(t *testing.T) {
	s := New()
	result := evalScheme(s, `(get-environment-variables)`)
	if result == nil {
		t.Errorf("Expected non-nil result from (get-environment-variables)")
		return
	}
	// Should be a list of pairs
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected list from (get-environment-variables), got: %v (%T)", result, result)
		return
	}
	// First element should be a pair (name . value)
	entry := First(pair)
	entryPair, ok := entry.(Pair)
	if !ok {
		t.Errorf("Expected pair entry, got: %v (%T)", entry, entry)
		return
	}
	if _, ok := First(entryPair).(string); !ok {
		t.Errorf("Expected string key, got: %v (%T)", First(entryPair), First(entryPair))
	}
}

func TestCurrentSecond(t *testing.T) {
	s := New()
	result := evalScheme(s, `(current-second)`)
	f, ok := result.(float64)
	if !ok {
		t.Fatalf("Expected float64, got: %v (%T)", result, result)
	}
	if f < 1700000000 {
		t.Errorf("current-second returned suspiciously small value: %v", f)
	}
}

func TestCurrentJiffy(t *testing.T) {
	s := New()
	result := evalScheme(s, `(current-jiffy)`)
	j, ok := result.(int64)
	if !ok {
		t.Fatalf("Expected int64, got: %v (%T)", result, result)
	}
	if j <= 0 {
		t.Errorf("current-jiffy returned non-positive value: %v", j)
	}
}

func TestJiffiesPerSecond(t *testing.T) {
	s := New()
	result := evalScheme(s, `(jiffies-per-second)`)
	if result != int64(1_000_000_000) {
		t.Errorf("Expected 1000000000, got: %v", result)
	}
}

func TestFeatures(t *testing.T) {
	s := New()
	result := evalScheme(s, `(features)`)
	if Stringify(result) != "(r7rs gscheme)" {
		t.Errorf("Expected (r7rs gscheme), got: %v", Stringify(result))
	}
}

func TestExitIsProcedure(t *testing.T) {
	s := New()
	result := evalScheme(s, `(procedure? exit)`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}

func TestLoadIsProcedure(t *testing.T) {
	s := New()
	result := evalScheme(s, `(procedure? load)`)
	if result != true {
		t.Errorf("Expected #t, got: %v", result)
	}
}
