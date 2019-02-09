package gscheme

import "testing"

// testInstallCharacterPrimitives verifies the Scheme RS7R character primitives are in the environment.
func TestInstallCharacterPrimitives(t *testing.T) {
	environment := NewRootEnvironment()
	installCharacterPrimitives(environment)
	symbols := []Symbol{
		"char?",
		"char=?",
		"char<?",
		"char>?",
		"char<=?",
		"char>=?",
		"char-ci=?",
		"char-ci<?",
		"char-ci>?",
		"char-ci<=?",
		"char-ci>=?",
		"char-alphabetic?",
		"char-numeric?",
		"char-whitespace?",
		"char-upper-case?",
		"char-lower-case?",
		"digit-value",
		"char->integer",
		"integer->char",
		"char-upcase",
		"char-downcase",
		"char-foldcase",
	}
	for _, symbol := range symbols {
		_, ok := environment.Lookup(symbol)
		if !ok {
			t.Errorf("Expect to be able to lookup symbol %s but it was not found.", symbol)
		}
	}
}

func TestPrimitiveChar(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char?"), 'x')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char? #\\x) did not evaluate to #t.")
	}
}

func TestPrimitiveCharEqual(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char=?"), 'x', 'x')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char=? #\\x #\\x) did not evaluate to #t.")
	}
	expression = List(Symbol("char=?"), 'x', 'y')
	value = interpreter.EvalGlobal(expression)
	if value != false {
		t.Errorf("The expression (char=? #\\x #\\y) did not evaluate to #f.")
	}
	expression = List(Symbol("char-ci=?"), 'x', 'X')
	value = interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char=? #\\x #\\X) did not evaluate to #t.")
	}
}

func TestPrimitiveCharLessThan(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char<?"), 'a', 'b', 'c')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char<? #\\a #\\b #\\c) did not evaluate to #t.")
	}
	expression = List(Symbol("char<?"), 'z', 'y')
	value = interpreter.EvalGlobal(expression)
	if value != false {
		t.Errorf("The expression (char<? #\\z #\\y) did not evaluate to #f.")
	}
	expression = List(Symbol("char-ci<?"), 'a', 'B', 'c')
	value = interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char-ci<? #\\a #\\B #\\c) did not evaluate to #t.")
	}
}

func TestPrimitiveCharLessThanEqual(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char<=?"), 'a', 'a', 'b')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char<=? #\\a #\\a #\\b) did not evaluate to #t.")
	}
	expression = List(Symbol("char-ci<=?"), 'a', 'A', 'b')
	value = interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char-ci=? #\\a #\\A #\\b) did not evaluate to #t.")
	}
}

func TestPrimitiveCharGreaterThan(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char>?"), 'z', 'y', 'x')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char>? #\\z #\\y #\\x) did not evaluate to #t.")
	}
	expression = List(Symbol("char-ci>?"), 'z', 'Y', 'x')
	value = interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char-ci>? #\\z #\\Y #\\x) did not evaluate to #t.")
	}
}

func TestPrimitiveCharGreaterThanEqual(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char>=?"), 'z', 'y', 'y')
	value := interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char>=? #\\z #\\y #\\y) did not evaluate to #t.")
	}
	expression = List(Symbol("char-ci>=?"), 'z', 'Y', 'y')
	value = interpreter.EvalGlobal(expression)
	if value != true {
		t.Errorf("The expression (char-ci>=? #\\z #\\Y #\\y) did not evaluate to #t.")
	}
}

func TestPrimitiveDigitValue(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("digit-value"), '0')
	value := interpreter.EvalGlobal(expression)
	if int32(0) != value {
		t.Errorf("The expression (digit-value \\#0) did not evaluate to 0, instead was: %v.", value)
	}
	expression = List(Symbol("digit-value"), 'k')
	value = interpreter.EvalGlobal(expression)
	if false != value {
		t.Errorf("The expression (digit-value \\#k) did not evaluate to false, instead was: %v.", value)
	}
}

func TestPrimitiveCharToInteger(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char->integer"), 'A')
	value := interpreter.EvalGlobal(expression)
	if int32(65) != value {
		t.Errorf("The expression (char->integer #\\a) did not evaluate to 65, instead we got: %T.", value)
	}
}

func TestPrimitiveIntegerToChar(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("integer->char"), 65)
	value := interpreter.EvalGlobal(expression)
	if 'A' != value {
		t.Errorf("The expression (integer->char 65) did not evaluate to 'A', instead we got: %v.", value)
	}
}

func TestPrimitiveCharUpcase(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char-upcase"), 'a')
	value := interpreter.EvalGlobal(expression)
	if 'A' != value {
		t.Errorf("The expression (char-upcase #\\a) did not evaluate to 'A', instead we got: %v.", value)
	}
}

func TestPrimitiveCharDowncase(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char-downcase"), 'A')
	value := interpreter.EvalGlobal(expression)
	if 'a' != value {
		t.Errorf("The expression (char-downcase #\\A) did not evaluate to 'a', instead we got: %v.", value)
	}
}

func TestPrimitiveCharFoldcase(t *testing.T) {
	interpreter := New()
	expression := List(Symbol("char-foldcase"), 'a')
	value := interpreter.EvalGlobal(expression)
	if 'A' != value {
		t.Errorf("The expression (char-foldcase #\\a) did not evaluate to 'A', instead we got: %v.", value)
	}
}
