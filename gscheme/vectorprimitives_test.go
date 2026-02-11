package gscheme

import "testing"

func TestInstallVectorPrimitives(t *testing.T) {
	environment := NewRootEnvironment()
	installVectorPrimitives(environment)
	symbols := []Symbol{
		"make-vector", "vector", "vector-length", "vector-ref", "vector-set!",
		"vector-fill!", "vector->list", "list->vector", "vector-copy",
		"vector-append", "vector->string", "string->vector",
		"vector-map", "vector-for-each",
		"make-bytevector", "bytevector", "bytevector-length",
		"bytevector-u8-ref", "bytevector-u8-set!", "bytevector-copy",
		"bytevector-append", "bytevector-copy!",
		"utf8->string", "string->utf8",
	}
	for _, symbol := range symbols {
		_, ok := environment.Lookup(symbol)
		if !ok {
			t.Errorf("Expected to find symbol %s but it was not found.", symbol)
		}
	}
}

func TestMakeVector(t *testing.T) {
	interpreter := New()

	// (make-vector 3 0) => #(0 0 0)
	result := interpreter.EvalGlobal(List(Symbol("make-vector"), float64(3), float64(0)))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 {
		t.Errorf("Expected length 3 but got: %d", len(vec))
	}
	for i, v := range vec {
		if v != float64(0) {
			t.Errorf("Expected 0 at index %d but got: %v", i, v)
		}
	}

	// (make-vector 2) => #(nil nil)
	result = interpreter.EvalGlobal(List(Symbol("make-vector"), float64(2)))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 2 {
		t.Errorf("Expected length 2 but got: %d", len(vec))
	}

	// (make-vector 0) => #()
	result = interpreter.EvalGlobal(List(Symbol("make-vector"), float64(0)))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 0 {
		t.Errorf("Expected length 0 but got: %d", len(vec))
	}
}

func TestVectorConstructor(t *testing.T) {
	interpreter := New()

	// (vector 1 2 3) => #(1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("vector"), float64(1), float64(2), float64(3)))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != float64(1) || vec[1] != float64(2) || vec[2] != float64(3) {
		t.Errorf("Expected #(1 2 3) but got: %v", result)
	}

	// (vector) => #()
	result = interpreter.EvalGlobal(List(Symbol("vector")))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 0 {
		t.Errorf("Expected empty vector but got length: %d", len(vec))
	}
}

func TestVectorLength(t *testing.T) {
	interpreter := New()

	// (vector-length (vector 1 2 3)) => 3
	result := interpreter.EvalGlobal(List(Symbol("vector-length"),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}

	// (vector-length (vector)) => 0
	result = interpreter.EvalGlobal(List(Symbol("vector-length"),
		List(Symbol("vector"))))
	if result != float64(0) {
		t.Errorf("Expected 0 but got: %v", result)
	}
}

func TestVectorRef(t *testing.T) {
	interpreter := New()

	// (vector-ref (vector 10 20 30) 1) => 20
	result := interpreter.EvalGlobal(List(Symbol("vector-ref"),
		List(Symbol("vector"), float64(10), float64(20), float64(30)),
		float64(1)))
	if result != float64(20) {
		t.Errorf("Expected 20 but got: %v", result)
	}
}

func TestVectorSet(t *testing.T) {
	interpreter := New()

	// (let ((v (vector 1 2 3))) (vector-set! v 1 99) (vector-ref v 1)) => 99
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("v"), List(Symbol("vector"), float64(1), float64(2), float64(3)))),
		List(Symbol("vector-set!"), Symbol("v"), float64(1), float64(99)),
		List(Symbol("vector-ref"), Symbol("v"), float64(1))))
	if result != float64(99) {
		t.Errorf("Expected 99 but got: %v", result)
	}
}

func TestVectorFill(t *testing.T) {
	interpreter := New()

	// (let ((v (make-vector 3))) (vector-fill! v 42) (vector-ref v 0)) => 42
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("v"), List(Symbol("make-vector"), float64(3)))),
		List(Symbol("vector-fill!"), Symbol("v"), float64(42)),
		List(Symbol("vector-ref"), Symbol("v"), float64(0))))
	if result != float64(42) {
		t.Errorf("Expected 42 but got: %v", result)
	}
}

func TestVectorToList(t *testing.T) {
	interpreter := New()

	// (vector->list (vector 1 2 3)) => (1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("vector->list"),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected pair but got: %T", result)
	}
	if First(pair) != float64(1) || First(Rest(pair)) != float64(2) || First(Rest(Rest(pair))) != float64(3) {
		t.Errorf("Expected (1 2 3) but got: %v", result)
	}

	// (vector->list (vector)) => ()
	result = interpreter.EvalGlobal(List(Symbol("vector->list"),
		List(Symbol("vector"))))
	if result != nil {
		t.Errorf("Expected () but got: %v", result)
	}
}

func TestListToVector(t *testing.T) {
	interpreter := New()

	// (list->vector '(1 2 3)) => #(1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("list->vector"),
		List(Symbol("quote"), List(float64(1), float64(2), float64(3)))))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != float64(1) || vec[1] != float64(2) || vec[2] != float64(3) {
		t.Errorf("Expected #(1 2 3) but got: %v", result)
	}

	// (list->vector '()) => #()
	result = interpreter.EvalGlobal(List(Symbol("list->vector"),
		List(Symbol("quote"), nil)))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 0 {
		t.Errorf("Expected empty vector but got length: %d", len(vec))
	}
}

func TestVectorCopy(t *testing.T) {
	interpreter := New()

	// (vector-copy (vector 1 2 3)) => #(1 2 3) (independent copy)
	result := interpreter.EvalGlobal(List(Symbol("vector-copy"),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != float64(1) || vec[1] != float64(2) || vec[2] != float64(3) {
		t.Errorf("Expected #(1 2 3) but got: %v", result)
	}

	// (vector-copy (vector 1 2 3 4 5) 1 4) => #(2 3 4)
	result = interpreter.EvalGlobal(List(Symbol("vector-copy"),
		List(Symbol("vector"), float64(1), float64(2), float64(3), float64(4), float64(5)),
		float64(1), float64(4)))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != float64(2) || vec[1] != float64(3) || vec[2] != float64(4) {
		t.Errorf("Expected #(2 3 4) but got: %v", result)
	}
}

func TestVectorAppend(t *testing.T) {
	interpreter := New()

	// (vector-append (vector 1 2) (vector 3 4)) => #(1 2 3 4)
	result := interpreter.EvalGlobal(List(Symbol("vector-append"),
		List(Symbol("vector"), float64(1), float64(2)),
		List(Symbol("vector"), float64(3), float64(4))))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 4 || vec[0] != float64(1) || vec[3] != float64(4) {
		t.Errorf("Expected #(1 2 3 4) but got: %v", result)
	}

	// (vector-append) => #()
	result = interpreter.EvalGlobal(List(Symbol("vector-append")))
	vec, ok = result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 0 {
		t.Errorf("Expected empty vector but got length: %d", len(vec))
	}
}

func TestVectorToString(t *testing.T) {
	interpreter := New()

	// (vector->string (vector #\a #\b #\c)) => "abc"
	result := interpreter.EvalGlobal(List(Symbol("vector->string"),
		List(Symbol("vector"), 'a', 'b', 'c')))
	if result != "abc" {
		t.Errorf("Expected \"abc\" but got: %v", result)
	}
}

func TestStringToVector(t *testing.T) {
	interpreter := New()

	// (string->vector "abc") => #(#\a #\b #\c)
	result := interpreter.EvalGlobal(List(Symbol("string->vector"), "abc"))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != 'a' || vec[1] != 'b' || vec[2] != 'c' {
		t.Errorf("Expected #(#\\a #\\b #\\c) but got: %v", result)
	}
}

func TestVectorMap(t *testing.T) {
	interpreter := New()

	// (vector-map (lambda (x) (+ x 1)) (vector 1 2 3)) => #(2 3 4)
	result := interpreter.EvalGlobal(List(Symbol("vector-map"),
		List(Symbol("lambda"), List(Symbol("x")),
			List(Symbol("+"), Symbol("x"), float64(1))),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	vec, ok := result.([]interface{})
	if !ok {
		t.Fatalf("Expected vector but got: %T", result)
	}
	if len(vec) != 3 || vec[0] != float64(2) || vec[1] != float64(3) || vec[2] != float64(4) {
		t.Errorf("Expected #(2 3 4) but got: %v", result)
	}
}

func TestVectorForEach(t *testing.T) {
	interpreter := New()

	// Use vector-for-each to accumulate into a list via side effect
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("result"), List(Symbol("quote"), nil))),
		List(Symbol("vector-for-each"),
			List(Symbol("lambda"), List(Symbol("x")),
				List(Symbol("set!"), Symbol("result"),
					List(Symbol("cons"), Symbol("x"), Symbol("result")))),
			List(Symbol("vector"), float64(1), float64(2), float64(3))),
		Symbol("result")))
	pair, ok := result.(Pair)
	if !ok {
		t.Fatalf("Expected pair but got: %T", result)
	}
	if First(pair) != float64(3) || First(Rest(pair)) != float64(2) || First(Rest(Rest(pair))) != float64(1) {
		t.Errorf("Expected (3 2 1) but got: %v", result)
	}
}

// --- Bytevector Tests ---

func TestMakeBytevector(t *testing.T) {
	interpreter := New()

	// (make-bytevector 3 0) => #u8(0 0 0)
	result := interpreter.EvalGlobal(List(Symbol("make-bytevector"), float64(3), float64(0)))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 0 || bv[1] != 0 || bv[2] != 0 {
		t.Errorf("Expected #u8(0 0 0) but got: %v", result)
	}

	// (make-bytevector 2 255)
	result = interpreter.EvalGlobal(List(Symbol("make-bytevector"), float64(2), float64(255)))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 2 || bv[0] != 255 || bv[1] != 255 {
		t.Errorf("Expected #u8(255 255) but got: %v", result)
	}

	// (make-bytevector 0) => #u8()
	result = interpreter.EvalGlobal(List(Symbol("make-bytevector"), float64(0)))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 0 {
		t.Errorf("Expected empty bytevector but got length: %d", len(bv))
	}
}

func TestBytevectorConstructor(t *testing.T) {
	interpreter := New()

	// (bytevector 1 2 3) => #u8(1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("bytevector"), float64(1), float64(2), float64(3)))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 1 || bv[1] != 2 || bv[2] != 3 {
		t.Errorf("Expected #u8(1 2 3) but got: %v", result)
	}

	// (bytevector) => #u8()
	result = interpreter.EvalGlobal(List(Symbol("bytevector")))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 0 {
		t.Errorf("Expected empty bytevector but got length: %d", len(bv))
	}
}

func TestBytevectorLength(t *testing.T) {
	interpreter := New()

	// (bytevector-length (bytevector 1 2 3)) => 3
	result := interpreter.EvalGlobal(List(Symbol("bytevector-length"),
		List(Symbol("bytevector"), float64(1), float64(2), float64(3))))
	if result != float64(3) {
		t.Errorf("Expected 3 but got: %v", result)
	}
}

func TestBytevectorU8Ref(t *testing.T) {
	interpreter := New()

	// (bytevector-u8-ref (bytevector 10 20 30) 1) => 20
	result := interpreter.EvalGlobal(List(Symbol("bytevector-u8-ref"),
		List(Symbol("bytevector"), float64(10), float64(20), float64(30)),
		float64(1)))
	if result != float64(20) {
		t.Errorf("Expected 20 but got: %v", result)
	}
}

func TestBytevectorU8Set(t *testing.T) {
	interpreter := New()

	// (let ((bv (bytevector 1 2 3))) (bytevector-u8-set! bv 1 99) (bytevector-u8-ref bv 1)) => 99
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(List(Symbol("bv"), List(Symbol("bytevector"), float64(1), float64(2), float64(3)))),
		List(Symbol("bytevector-u8-set!"), Symbol("bv"), float64(1), float64(99)),
		List(Symbol("bytevector-u8-ref"), Symbol("bv"), float64(1))))
	if result != float64(99) {
		t.Errorf("Expected 99 but got: %v", result)
	}
}

func TestBytevectorCopy(t *testing.T) {
	interpreter := New()

	// (bytevector-copy (bytevector 1 2 3)) => #u8(1 2 3)
	result := interpreter.EvalGlobal(List(Symbol("bytevector-copy"),
		List(Symbol("bytevector"), float64(1), float64(2), float64(3))))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 1 || bv[1] != 2 || bv[2] != 3 {
		t.Errorf("Expected #u8(1 2 3) but got: %v", result)
	}

	// (bytevector-copy (bytevector 1 2 3 4 5) 1 4) => #u8(2 3 4)
	result = interpreter.EvalGlobal(List(Symbol("bytevector-copy"),
		List(Symbol("bytevector"), float64(1), float64(2), float64(3), float64(4), float64(5)),
		float64(1), float64(4)))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 3 || bv[0] != 2 || bv[1] != 3 || bv[2] != 4 {
		t.Errorf("Expected #u8(2 3 4) but got: %v", result)
	}
}

func TestBytevectorAppend(t *testing.T) {
	interpreter := New()

	// (bytevector-append (bytevector 1 2) (bytevector 3 4)) => #u8(1 2 3 4)
	result := interpreter.EvalGlobal(List(Symbol("bytevector-append"),
		List(Symbol("bytevector"), float64(1), float64(2)),
		List(Symbol("bytevector"), float64(3), float64(4))))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 4 || bv[0] != 1 || bv[1] != 2 || bv[2] != 3 || bv[3] != 4 {
		t.Errorf("Expected #u8(1 2 3 4) but got: %v", result)
	}

	// (bytevector-append) => #u8()
	result = interpreter.EvalGlobal(List(Symbol("bytevector-append")))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if len(bv) != 0 {
		t.Errorf("Expected empty bytevector but got length: %d", len(bv))
	}
}

func TestBytevectorCopyTo(t *testing.T) {
	interpreter := New()

	// (let ((to (bytevector 0 0 0 0 0))
	//       (from (bytevector 1 2 3)))
	//   (bytevector-copy! to 1 from)
	//   to) => #u8(0 1 2 3 0)
	result := interpreter.EvalGlobal(List(Symbol("let"),
		List(
			List(Symbol("to"), List(Symbol("bytevector"), float64(0), float64(0), float64(0), float64(0), float64(0))),
			List(Symbol("from"), List(Symbol("bytevector"), float64(1), float64(2), float64(3)))),
		List(Symbol("bytevector-copy!"), Symbol("to"), float64(1), Symbol("from")),
		Symbol("to")))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	expected := []uint8{0, 1, 2, 3, 0}
	for i, b := range expected {
		if bv[i] != b {
			t.Errorf("Expected %d at index %d but got: %d", b, i, bv[i])
		}
	}

	// With start and end: (bytevector-copy! to 0 from 1 3) copies from[1:3] to to[0:]
	result = interpreter.EvalGlobal(List(Symbol("let"),
		List(
			List(Symbol("to"), List(Symbol("bytevector"), float64(0), float64(0), float64(0))),
			List(Symbol("from"), List(Symbol("bytevector"), float64(10), float64(20), float64(30)))),
		List(Symbol("bytevector-copy!"), Symbol("to"), float64(0), Symbol("from"), float64(1), float64(3)),
		Symbol("to")))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	if bv[0] != 20 || bv[1] != 30 {
		t.Errorf("Expected #u8(20 30 0) but got: %v", bv)
	}
}

func TestUtf8ToString(t *testing.T) {
	interpreter := New()

	// (utf8->string (bytevector 104 101 108 108 111)) => "hello"
	result := interpreter.EvalGlobal(List(Symbol("utf8->string"),
		List(Symbol("bytevector"), float64(104), float64(101), float64(108), float64(108), float64(111))))
	if result != "hello" {
		t.Errorf("Expected \"hello\" but got: %v", result)
	}

	// With start/end: (utf8->string (bytevector 104 101 108 108 111) 1 4) => "ell"
	result = interpreter.EvalGlobal(List(Symbol("utf8->string"),
		List(Symbol("bytevector"), float64(104), float64(101), float64(108), float64(108), float64(111)),
		float64(1), float64(4)))
	if result != "ell" {
		t.Errorf("Expected \"ell\" but got: %v", result)
	}
}

func TestStringToUtf8(t *testing.T) {
	interpreter := New()

	// (string->utf8 "hello") => #u8(104 101 108 108 111)
	result := interpreter.EvalGlobal(List(Symbol("string->utf8"), "hello"))
	bv, ok := result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	expected := []uint8{104, 101, 108, 108, 111}
	if len(bv) != len(expected) {
		t.Fatalf("Expected length %d but got: %d", len(expected), len(bv))
	}
	for i, b := range expected {
		if bv[i] != b {
			t.Errorf("Expected %d at index %d but got: %d", b, i, bv[i])
		}
	}

	// With start/end: (string->utf8 "hello" 1 4) => #u8(101 108 108)
	result = interpreter.EvalGlobal(List(Symbol("string->utf8"), "hello", float64(1), float64(4)))
	bv, ok = result.([]uint8)
	if !ok {
		t.Fatalf("Expected bytevector but got: %T", result)
	}
	expected = []uint8{101, 108, 108}
	if len(bv) != len(expected) {
		t.Fatalf("Expected length %d but got: %d", len(expected), len(bv))
	}
	for i, b := range expected {
		if bv[i] != b {
			t.Errorf("Expected %d at index %d but got: %d", b, i, bv[i])
		}
	}
}

// --- Equality Tests ---

func TestVectorEquality(t *testing.T) {
	interpreter := New()

	// (equal? (vector 1 2 3) (vector 1 2 3)) => #t
	result := interpreter.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("vector"), float64(1), float64(2), float64(3)),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	if result != true {
		t.Errorf("Expected #t for equal vectors but got: %v", result)
	}

	// (equal? (vector 1 2) (vector 1 3)) => #f
	result = interpreter.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("vector"), float64(1), float64(2)),
		List(Symbol("vector"), float64(1), float64(3))))
	if result != false {
		t.Errorf("Expected #f for unequal vectors but got: %v", result)
	}

	// (equal? (vector 1 2) (vector 1 2 3)) => #f (different lengths)
	result = interpreter.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("vector"), float64(1), float64(2)),
		List(Symbol("vector"), float64(1), float64(2), float64(3))))
	if result != false {
		t.Errorf("Expected #f for different length vectors but got: %v", result)
	}
}

func TestBytevectorEquality(t *testing.T) {
	interpreter := New()

	// (equal? (bytevector 1 2 3) (bytevector 1 2 3)) => #t
	result := interpreter.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("bytevector"), float64(1), float64(2), float64(3)),
		List(Symbol("bytevector"), float64(1), float64(2), float64(3))))
	if result != true {
		t.Errorf("Expected #t for equal bytevectors but got: %v", result)
	}

	// (equal? (bytevector 1 2) (bytevector 1 3)) => #f
	result = interpreter.EvalGlobal(List(Symbol("equal?"),
		List(Symbol("bytevector"), float64(1), float64(2)),
		List(Symbol("bytevector"), float64(1), float64(3))))
	if result != false {
		t.Errorf("Expected #f for unequal bytevectors but got: %v", result)
	}
}
