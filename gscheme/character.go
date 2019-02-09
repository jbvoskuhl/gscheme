package gscheme

import "unicode"

func installCharacterPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("char?", 1, 1, primitiveChar))
	environment.DefineName(NewPrimitive("char=?", 2, maxArgs, primitiveCharEqual))
	environment.DefineName(NewPrimitive("char<?", 2, maxArgs, primitiveCharLessThan))
	environment.DefineName(NewPrimitive("char>?", 2, maxArgs, primitiveCharGreaterThan))
	environment.DefineName(NewPrimitive("char<=?", 2, maxArgs, primitiveCharLessThanOrEquals))
	environment.DefineName(NewPrimitive("char>=?", 2, maxArgs, primitiveCharGreaterThanOrEquals))
	environment.DefineName(NewPrimitive("char-ci=?", 2, maxArgs, primitiveCharCaseInsensitiveEqual))
	environment.DefineName(NewPrimitive("char-ci<?", 2, maxArgs, primitiveCharCaseInsensitiveLessThan))
	environment.DefineName(NewPrimitive("char-ci>?", 2, maxArgs, primitiveCharCaseInsensitiveGreaterThan))
	environment.DefineName(NewPrimitive("char-ci<=?", 2, maxArgs, primitiveCharCaseInsensitiveLessThanOrEquals))
	environment.DefineName(NewPrimitive("char-ci>=?", 2, maxArgs, primitiveCharCaseInsensitiveGreaterThanOrEquals))
	environment.DefineName(NewPrimitive("char-alphabetic?", 1, 1, primitiveCharAlphabetic))
	environment.DefineName(NewPrimitive("char-numeric?", 1, 1, primitiveCharNumeric))
	environment.DefineName(NewPrimitive("char-whitespace?", 1, 1, primitiveCharWhitespace))
	environment.DefineName(NewPrimitive("char-upper-case?", 1, 1, primitiveCharUpperCase))
	environment.DefineName(NewPrimitive("char-lower-case?", 1, 1, primitiveCharLowerCase))
	environment.DefineName(NewPrimitive("digit-value", 1, 1, primitiveCharDigitValue))
	environment.DefineName(NewPrimitive("char->integer", 1, 1, primitiveCharToInteger))
	environment.DefineName(NewPrimitive("integer->char", 1, 1, primitiveIntegerToChar))
	environment.DefineName(NewPrimitive("char-upcase", 1, 1, primitiveCharUpcase))
	environment.DefineName(NewPrimitive("char-downcase", 1, 1, primitiveCharDowncase))
	environment.DefineName(NewPrimitive("char-foldcase", 1, 1, primitiveCharFoldcase))
}

var primitiveChar = charPredicate(func(rune) bool { return true })

var primitiveCharEqual = charsPredicate(func(x rune, y rune) bool { return x == y })

var primitiveCharLessThan = charsPredicate(func(x rune, y rune) bool { return x < y })

var primitiveCharGreaterThan = charsPredicate(func(x rune, y rune) bool { return x > y })

var primitiveCharLessThanOrEquals = charsPredicate(func(x rune, y rune) bool { return x <= y })

var primitiveCharGreaterThanOrEquals = charsPredicate(func(x rune, y rune) bool { return x >= y })

var primitiveCharCaseInsensitiveEqual = charsPredicateCaseInsensitive(func(x rune, y rune) bool { return x == y })

var primitiveCharCaseInsensitiveLessThan = charsPredicateCaseInsensitive(func(x rune, y rune) bool { return x < y })

var primitiveCharCaseInsensitiveGreaterThan = charsPredicateCaseInsensitive(func(x rune, y rune) bool { return x > y })

var primitiveCharCaseInsensitiveLessThanOrEquals = charsPredicateCaseInsensitive(
	func(x rune, y rune) bool { return x <= y })

var primitiveCharCaseInsensitiveGreaterThanOrEquals = charsPredicateCaseInsensitive(
	func(x rune, y rune) bool { return x >= y })

var primitiveCharAlphabetic = charPredicate(unicode.IsLetter)

var primitiveCharNumeric = charPredicate(unicode.IsNumber)

var primitiveCharWhitespace = charPredicate(unicode.IsSpace)

var primitiveCharUpperCase = charPredicate(unicode.IsUpper)

var primitiveCharLowerCase = charPredicate(unicode.IsLower)

func primitiveCharDigitValue(args Pair) interface{} {
	// TODO(jbvoskuhl): Make this more Unicode friendly.
	arg := characterConstraint(First(args))
	if arg < '0' || arg > '9' {
		return false
	} else {
		return int32(arg - '0')
	}
}

func primitiveCharToInteger(args Pair) interface{} {
	arg := characterConstraint(First(args))
	return int32(arg)
}

func primitiveIntegerToChar(args Pair) interface{} {
	arg := integerConstraint(First(args))
	switch value := arg.(type) {
	case uint:
		return rune(value)
	case uint8:
		return rune(value)
	case uint16:
		return rune(value)
	case uint32:
		return rune(value)
	case uint64:
		return rune(value)
	case int:
		return rune(value)
	case int8:
		return rune(value)
	case int16:
		return rune(value)
	case int32:
		return rune(value)
	case int64:
		return rune(value)
	default:
		panic("Internal Bug: Missing integer type in integer->char input validation.")
	}
}

func primitiveCharUpcase(args Pair) interface{} {
	arg := characterConstraint(First(args))
	return unicode.ToUpper(arg)
}

func primitiveCharDowncase(args Pair) interface{} {
	arg := characterConstraint(First(args))
	return unicode.ToLower(arg)
}

func primitiveCharFoldcase(args Pair) interface{} {
	arg := characterConstraint(First(args))
	return unicode.SimpleFold(arg)
}

func charPredicate(predicate func(rune) bool) func(Pair) interface{} {
	return func(args Pair) interface{} {
		arg := First(args)
		value, ok := arg.(rune)
		return ok && predicate(value)
	}
}

func charsPredicate(predicate func(rune, rune) bool) func(Pair) interface{} {
	return func(args Pair) interface{} {
		current := characterConstraint(First(args))
		args = RestPair(args)
		for args != nil {
			next := characterConstraint(First(args))
			if !predicate(current, next) {
				return false
			}
			current = next
			args = RestPair(args)
		}
		return true
	}
}

func charsPredicateCaseInsensitive(predicate func(rune, rune) bool) func(Pair) interface{} {
	return charsPredicate(caseInsensitivePredicate(predicate))
}

func caseInsensitivePredicate(predicate func(rune, rune) bool) func(rune, rune) bool {
	return func(x, y rune) bool {
		return predicate(unicode.ToLower(x), unicode.ToLower(y))
	}
}
