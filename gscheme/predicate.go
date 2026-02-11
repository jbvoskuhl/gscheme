package gscheme

import "math"

// installPredicatePrimitives adds predicate functions to a given environment.
func installPredicatePrimitives(environment Environment) {
	// Equivalence predicates
	environment.DefineName(NewPrimitive("eq?", 2, 2, primitiveEq))
	environment.DefineName(NewPrimitive("eqv?", 2, 2, primitiveEqv))
	environment.DefineName(NewPrimitive("equal?", 2, 2, primitiveEqual))

	// Type predicates
	environment.DefineName(NewPrimitive("number?", 1, 1, primitiveNumberP))
	environment.DefineName(NewPrimitive("integer?", 1, 1, primitiveIntegerP))
	environment.DefineName(NewPrimitive("string?", 1, 1, primitiveStringP))
	environment.DefineName(NewPrimitive("procedure?", 1, 1, primitiveProcedureP))

	// Number predicates
	environment.DefineName(NewPrimitive("zero?", 1, 1, primitiveZeroP))
	environment.DefineName(NewPrimitive("positive?", 1, 1, primitivePositiveP))
	environment.DefineName(NewPrimitive("negative?", 1, 1, primitiveNegativeP))
	environment.DefineName(NewPrimitive("odd?", 1, 1, primitiveOddP))
	environment.DefineName(NewPrimitive("even?", 1, 1, primitiveEvenP))
}

// primitiveEq implements eq? which tests for identity (pointer equality).
func primitiveEq(args Pair) interface{} {
	x, y := First(args), Second(args)
	return x == y
}

// primitiveEqv implements eqv? which is like eq? but also compares
// numbers and characters by value.
func primitiveEqv(args Pair) interface{} {
	x, y := First(args), Second(args)
	if x == y {
		return true
	}
	// Compare numbers by value
	if xNum, ok := x.(float64); ok {
		if yNum, ok := y.(float64); ok {
			return xNum == yNum
		}
	}
	// Compare characters by value
	if xChar, ok := x.(rune); ok {
		if yChar, ok := y.(rune); ok {
			return xChar == yChar
		}
	}
	return false
}

// primitiveEqual implements equal? which tests for structural equality.
// It recursively compares pairs, strings, and other compound objects.
func primitiveEqual(args Pair) interface{} {
	return equal(First(args), Second(args))
}

// equal checks for deep structural equality between two objects.
func equal(x, y interface{}) bool {
	if x == y {
		return true
	}
	if x == nil || y == nil {
		return x == y
	}
	// Compare pairs recursively
	if xPair, ok := x.(Pair); ok {
		if yPair, ok := y.(Pair); ok {
			return equal(xPair.First(), yPair.First()) &&
				equal(xPair.Rest(), yPair.Rest())
		}
		return false
	}
	// Compare strings
	if xStr, ok := x.(string); ok {
		if yStr, ok := y.(string); ok {
			return xStr == yStr
		}
		return false
	}
	// Compare numbers by value
	if xNum, ok := x.(float64); ok {
		if yNum, ok := y.(float64); ok {
			return xNum == yNum
		}
		return false
	}
	// Compare characters by value
	if xChar, ok := x.(rune); ok {
		if yChar, ok := y.(rune); ok {
			return xChar == yChar
		}
		return false
	}
	// Compare booleans
	if xBool, ok := x.(bool); ok {
		if yBool, ok := y.(bool); ok {
			return xBool == yBool
		}
		return false
	}
	// Default: not equal
	return false
}

// primitiveNumberP implements number? which tests if the argument is a number.
func primitiveNumberP(args Pair) interface{} {
	_, ok := First(args).(float64)
	return ok
}

// primitiveIntegerP implements integer? which tests if the argument is an integer.
func primitiveIntegerP(args Pair) interface{} {
	x, ok := First(args).(float64)
	if !ok {
		return false
	}
	return x == math.Trunc(x)
}

// primitiveStringP implements string? which tests if the argument is a string.
func primitiveStringP(args Pair) interface{} {
	_, ok := First(args).(string)
	return ok
}

// primitiveProcedureP implements procedure? which tests if the argument is callable.
func primitiveProcedureP(args Pair) interface{} {
	_, ok := First(args).(Applyer)
	return ok
}

// primitiveZeroP implements zero? which tests if the argument is zero.
func primitiveZeroP(args Pair) interface{} {
	x, ok := First(args).(float64)
	return ok && x == 0
}

// primitivePositiveP implements positive? which tests if the argument is positive.
func primitivePositiveP(args Pair) interface{} {
	x, ok := First(args).(float64)
	return ok && x > 0
}

// primitiveNegativeP implements negative? which tests if the argument is negative.
func primitiveNegativeP(args Pair) interface{} {
	x, ok := First(args).(float64)
	return ok && x < 0
}

// primitiveOddP implements odd? which tests if the argument is odd.
func primitiveOddP(args Pair) interface{} {
	x, ok := First(args).(float64)
	if !ok {
		return Err("odd?: expected number", args)
	}
	return int64(math.Abs(x))%2 != 0
}

// primitiveEvenP implements even? which tests if the argument is even.
func primitiveEvenP(args Pair) interface{} {
	x, ok := First(args).(float64)
	if !ok {
		return Err("even?: expected number", args)
	}
	return int64(math.Abs(x))%2 == 0
}
