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
	if int64(0) != value {
		t.Errorf("The expression (digit-value \\#0) did not evaluate to 0, instead was: %v.", value)
	}
	expression = List(Symbol("digit-value"), '5')
	value = interpreter.EvalGlobal(expression)
	if int64(5) != value {
		t.Errorf("The expression (digit-value \\#5) did not evaluate to 5, instead was: %v.", value)
	}
	expression = List(Symbol("digit-value"), '9')
	value = interpreter.EvalGlobal(expression)
	if int64(9) != value {
		t.Errorf("The expression (digit-value \\#9) did not evaluate to 9, instead was: %v.", value)
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
	if int64(65) != value {
		t.Errorf("The expression (char->integer #\\A) did not evaluate to 65, instead we got: %v (%T).", value, value)
	}
	// Unicode character
	expression = List(Symbol("char->integer"), '\u03BB') // lambda
	value = interpreter.EvalGlobal(expression)
	if int64(955) != value {
		t.Errorf("The expression (char->integer #\\λ) did not evaluate to 955, instead we got: %v.", value)
	}
}

func TestPrimitiveIntegerToChar(t *testing.T) {
	interpreter := New()
	// float64 input (as the reader produces)
	expression := List(Symbol("integer->char"), float64(65))
	value := interpreter.EvalGlobal(expression)
	if 'A' != value {
		t.Errorf("The expression (integer->char 65) did not evaluate to 'A', instead we got: %v.", value)
	}
	// int input
	expression = List(Symbol("integer->char"), 65)
	value = interpreter.EvalGlobal(expression)
	if 'A' != value {
		t.Errorf("The expression (integer->char 65) with int did not evaluate to 'A', instead we got: %v.", value)
	}
	// Unicode code point
	expression = List(Symbol("integer->char"), float64(955))
	value = interpreter.EvalGlobal(expression)
	if '\u03BB' != value {
		t.Errorf("The expression (integer->char 955) did not evaluate to λ, instead we got: %v.", value)
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
	// Lowercase stays lowercase
	expression := List(Symbol("char-foldcase"), 'a')
	value := interpreter.EvalGlobal(expression)
	if 'a' != value {
		t.Errorf("The expression (char-foldcase #\\a) did not evaluate to 'a', instead we got: %v.", value)
	}
	// Uppercase folds to lowercase
	expression = List(Symbol("char-foldcase"), 'A')
	value = interpreter.EvalGlobal(expression)
	if 'a' != value {
		t.Errorf("The expression (char-foldcase #\\A) did not evaluate to 'a', instead we got: %v.", value)
	}
}

// Unicode-aware tests

func TestCharNumericUnicode(t *testing.T) {
	interpreter := New()

	// Arabic-Indic digit ٣ (U+0663) is a decimal digit
	result := interpreter.EvalGlobal(List(Symbol("char-numeric?"), '\u0663'))
	if result != true {
		t.Errorf("Expected #t for (char-numeric? #\\x663) (Arabic-Indic 3), got %v", result)
	}

	// Devanagari digit ५ (U+096B) is a decimal digit
	result = interpreter.EvalGlobal(List(Symbol("char-numeric?"), '\u096B'))
	if result != true {
		t.Errorf("Expected #t for (char-numeric? #\\x96B) (Devanagari 5), got %v", result)
	}

	// Roman numeral Ⅳ (U+2163) is NOT a decimal digit (category Nl, not Nd)
	result = interpreter.EvalGlobal(List(Symbol("char-numeric?"), '\u2163'))
	if result != false {
		t.Errorf("Expected #f for (char-numeric? #\\x2163) (Roman numeral IV), got %v", result)
	}

	// Fraction ½ (U+00BD) is NOT a decimal digit (category No, not Nd)
	result = interpreter.EvalGlobal(List(Symbol("char-numeric?"), '\u00BD'))
	if result != false {
		t.Errorf("Expected #f for (char-numeric? #\\xBD) (fraction 1/2), got %v", result)
	}
}

func TestDigitValueUnicode(t *testing.T) {
	interpreter := New()

	// Arabic-Indic digits ٠-٩ (U+0660-U+0669)
	for i := 0; i <= 9; i++ {
		ch := rune(0x0660 + i)
		result := interpreter.EvalGlobal(List(Symbol("digit-value"), ch))
		if result != int64(i) {
			t.Errorf("Expected %d for (digit-value #\\x%X), got %v", i, ch, result)
		}
	}

	// Devanagari digits ०-९ (U+0966-U+096F)
	for i := 0; i <= 9; i++ {
		ch := rune(0x0966 + i)
		result := interpreter.EvalGlobal(List(Symbol("digit-value"), ch))
		if result != int64(i) {
			t.Errorf("Expected %d for (digit-value #\\x%X), got %v", i, ch, result)
		}
	}

	// Non-digit returns #f
	result := interpreter.EvalGlobal(List(Symbol("digit-value"), '\u03BB'))
	if result != false {
		t.Errorf("Expected #f for (digit-value #\\λ), got %v", result)
	}
}

func TestCharCaseUnicode(t *testing.T) {
	interpreter := New()

	// Greek Σ (uppercase) → σ (lowercase)
	result := interpreter.EvalGlobal(List(Symbol("char-downcase"), '\u03A3'))
	if result != '\u03C3' {
		t.Errorf("Expected σ (U+03C3) for (char-downcase #\\Σ), got %v", result)
	}

	// Greek σ (lowercase) → Σ (uppercase)
	result = interpreter.EvalGlobal(List(Symbol("char-upcase"), '\u03C3'))
	if result != '\u03A3' {
		t.Errorf("Expected Σ (U+03A3) for (char-upcase #\\σ), got %v", result)
	}

	// char-foldcase on Greek Σ → σ
	result = interpreter.EvalGlobal(List(Symbol("char-foldcase"), '\u03A3'))
	if result != '\u03C3' {
		t.Errorf("Expected σ (U+03C3) for (char-foldcase #\\Σ), got %v", result)
	}

	// char-upper-case? on Σ
	result = interpreter.EvalGlobal(List(Symbol("char-upper-case?"), '\u03A3'))
	if result != true {
		t.Errorf("Expected #t for (char-upper-case? #\\Σ), got %v", result)
	}

	// char-lower-case? on σ
	result = interpreter.EvalGlobal(List(Symbol("char-lower-case?"), '\u03C3'))
	if result != true {
		t.Errorf("Expected #t for (char-lower-case? #\\σ), got %v", result)
	}

	// char-alphabetic? on λ
	result = interpreter.EvalGlobal(List(Symbol("char-alphabetic?"), '\u03BB'))
	if result != true {
		t.Errorf("Expected #t for (char-alphabetic? #\\λ), got %v", result)
	}

	// char-alphabetic? on CJK character 漢 (U+6F22)
	result = interpreter.EvalGlobal(List(Symbol("char-alphabetic?"), '\u6F22'))
	if result != true {
		t.Errorf("Expected #t for (char-alphabetic? #\\漢), got %v", result)
	}
}

func TestStringifyCharacter(t *testing.T) {
	tests := []struct {
		input    rune
		expected string
	}{
		{'a', `#\a`},
		{' ', `#\space`},
		{'\n', `#\newline`},
		{'\t', `#\tab`},
		{'\x07', `#\alarm`},
		{'\x08', `#\backspace`},
		{'\x7F', `#\delete`},
		{'\x1B', `#\escape`},
		{'\x00', `#\null`},
		{'\r', `#\return`},
		{'\u03BB', `#\x3BB`},       // λ — non-ASCII uses hex
		{'\u6F22', `#\x6F22`},      // 漢 — CJK uses hex
		{'!', `#\!`},               // printable ASCII
		{'~', `#\~`},               // printable ASCII
	}
	for _, tt := range tests {
		result := Stringify(tt.input)
		if result != tt.expected {
			t.Errorf("Stringify(%U): expected %q, got %q", tt.input, tt.expected, result)
		}
	}
}

func TestCharCIUnicode(t *testing.T) {
	interpreter := New()

	// Greek uppercase and lowercase should be equal under case-insensitive comparison
	result := interpreter.EvalGlobal(List(Symbol("char-ci=?"), '\u03A3', '\u03C3'))
	if result != true {
		t.Errorf("Expected #t for (char-ci=? #\\Σ #\\σ), got %v", result)
	}

	// Cyrillic А (U+0410) and а (U+0430)
	result = interpreter.EvalGlobal(List(Symbol("char-ci=?"), '\u0410', '\u0430'))
	if result != true {
		t.Errorf("Expected #t for (char-ci=? #\\А #\\а) (Cyrillic), got %v", result)
	}
}
