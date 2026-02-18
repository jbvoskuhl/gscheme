package gscheme

import (
	"math"
	"math/big"
)

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
	environment.DefineName(NewPrimitive("vector?", 1, 1, primitiveVectorP))
	environment.DefineName(NewPrimitive("bytevector?", 1, 1, primitiveBytevectorP))
	environment.DefineName(NewPrimitive("eof-object?", 1, 1, primitiveEofObjectP))
	environment.DefineName(NewPrimitive("input-port?", 1, 1, primitiveInputPortP))
	environment.DefineName(NewPrimitive("port?", 1, 1, primitivePortP))

	// Number predicates
	environment.DefineName(NewPrimitive("zero?", 1, 1, primitiveZeroP))
	environment.DefineName(NewPrimitive("positive?", 1, 1, primitivePositiveP))
	environment.DefineName(NewPrimitive("negative?", 1, 1, primitiveNegativeP))
	environment.DefineName(NewPrimitive("odd?", 1, 1, primitiveOddP))
	environment.DefineName(NewPrimitive("even?", 1, 1, primitiveEvenP))
	environment.DefineName(NewPrimitive("rational?", 1, 1, primitiveRationalP))
	environment.DefineName(NewPrimitive("exact?", 1, 1, primitiveExactP))
	environment.DefineName(NewPrimitive("inexact?", 1, 1, primitiveInexactP))
	environment.DefineName(NewPrimitive("exact-integer?", 1, 1, primitiveExactIntegerP))
	environment.DefineName(NewPrimitive("finite?", 1, 1, primitiveFiniteP))
	environment.DefineName(NewPrimitive("infinite?", 1, 1, primitiveInfiniteP))
	environment.DefineName(NewPrimitive("nan?", 1, 1, primitiveNanP))
}

// primitiveEq implements eq? which tests for identity (pointer equality).
func primitiveEq(args Pair) interface{} {
	x, y := First(args), Second(args)
	return x == y
}

// primitiveEqv implements eqv? which is like eq? but also compares
// numbers and characters by value. Per R7RS, (eqv? 1 1.0) is #f.
func primitiveEqv(args Pair) interface{} {
	x, y := First(args), Second(args)
	if x == y {
		return true
	}
	// Compare int64 by value
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt == yInt
		}
		return false // int64 vs float64 → different exactness → #f
	}
	// Compare *big.Rat by value
	if xRat, ok := x.(*big.Rat); ok {
		if yRat, ok := y.(*big.Rat); ok {
			return xRat.Cmp(yRat) == 0
		}
		return false
	}
	// Compare float64 by value
	if xNum, ok := x.(float64); ok {
		if yNum, ok := y.(float64); ok {
			return xNum == yNum
		}
		return false // float64 vs int64 → different exactness → #f
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
	if x == nil || y == nil {
		return x == y
	}
	// Compare vectors element-by-element (before identity check since slices are not comparable)
	if xVec, ok := x.([]interface{}); ok {
		if yVec, ok := y.([]interface{}); ok {
			if len(xVec) != len(yVec) {
				return false
			}
			for i := range xVec {
				if !equal(xVec[i], yVec[i]) {
					return false
				}
			}
			return true
		}
		return false
	}
	// Compare bytevectors element-by-element
	if xBv, ok := x.([]uint8); ok {
		if yBv, ok := y.([]uint8); ok {
			if len(xBv) != len(yBv) {
				return false
			}
			for i := range xBv {
				if xBv[i] != yBv[i] {
					return false
				}
			}
			return true
		}
		return false
	}
	// Identity check (safe after ruling out uncomparable slice types)
	if x == y {
		return true
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
	// Compare int64 numbers by value
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt == yInt
		}
		return false
	}
	// Compare *big.Rat numbers by value
	if xRat, ok := x.(*big.Rat); ok {
		if yRat, ok := y.(*big.Rat); ok {
			return xRat.Cmp(yRat) == 0
		}
		return false
	}
	// Compare float64 numbers by value
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
	x := First(args)
	switch x.(type) {
	case int64, *big.Rat, float64, complex128:
		return true
	default:
		return false
	}
}

// primitiveIntegerP implements integer? which tests if the argument is an integer.
// Per R7RS, both exact and inexact integers return #t.
func primitiveIntegerP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return true
	case *big.Rat:
		return v.IsInt()
	case float64:
		return v == math.Trunc(v) && !math.IsInf(v, 0) && !math.IsNaN(v)
	default:
		return false
	}
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
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v == 0
	case *big.Rat:
		return v.Sign() == 0
	case float64:
		return v == 0
	default:
		return false
	}
}

// primitivePositiveP implements positive? which tests if the argument is positive.
func primitivePositiveP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v > 0
	case *big.Rat:
		return v.Sign() > 0
	case float64:
		return v > 0
	default:
		return false
	}
}

// primitiveNegativeP implements negative? which tests if the argument is negative.
func primitiveNegativeP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v < 0
	case *big.Rat:
		return v.Sign() < 0
	case float64:
		return v < 0
	default:
		return false
	}
}

// primitiveOddP implements odd? which tests if the argument is odd.
func primitiveOddP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v%2 != 0
	case *big.Rat:
		if !v.IsInt() {
			return Err("odd?: expected integer", args)
		}
		return v.Num().Int64()%2 != 0
	case float64:
		return int64(math.Abs(v))%2 != 0
	default:
		return Err("odd?: expected number", args)
	}
}

// primitiveEvenP implements even? which tests if the argument is even.
func primitiveEvenP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v%2 == 0
	case *big.Rat:
		if !v.IsInt() {
			return Err("even?: expected integer", args)
		}
		return v.Num().Int64()%2 == 0
	case float64:
		return int64(math.Abs(v))%2 == 0
	default:
		return Err("even?: expected number", args)
	}
}

// primitiveVectorP implements vector? which tests if the argument is a vector.
func primitiveVectorP(args Pair) interface{} {
	_, ok := First(args).([]interface{})
	return ok
}

// primitiveBytevectorP implements bytevector? which tests if the argument is a bytevector.
func primitiveBytevectorP(args Pair) interface{} {
	_, ok := First(args).([]uint8)
	return ok
}

// primitiveEofObjectP implements eof-object? which tests if the argument is an EOF object.
func primitiveEofObjectP(args Pair) interface{} {
	return IsEOF(First(args))
}

// primitiveInputPortP implements input-port? which tests if the argument is an input port.
func primitiveInputPortP(args Pair) interface{} {
	_, ok := First(args).(*InputPort)
	return ok
}

// primitivePortP implements port? which tests if the argument is any kind of port.
func primitivePortP(args Pair) interface{} {
	_, ok := First(args).(*InputPort)
	return ok
}

// primitiveRationalP implements rational? which tests if the argument is a rational number.
func primitiveRationalP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return true
	case *big.Rat:
		_ = v
		return true
	case float64:
		return !math.IsInf(v, 0) && !math.IsNaN(v)
	case complex128:
		return imag(v) == 0
	default:
		return false
	}
}

// primitiveExactP implements exact? which tests if the argument is an exact number.
// int64 values are exact; float64 and complex128 are inexact.
func primitiveExactP(args Pair) interface{} {
	return isExact(First(args))
}

// primitiveInexactP implements inexact? which tests if the argument is an inexact number.
func primitiveInexactP(args Pair) interface{} {
	x := First(args)
	switch x.(type) {
	case float64, complex128:
		return true
	default:
		return false
	}
}

// primitiveExactIntegerP implements exact-integer? which tests if the argument is an exact integer.
func primitiveExactIntegerP(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return true
	}
	if r, ok := x.(*big.Rat); ok {
		return r.IsInt()
	}
	return false
}

// primitiveFiniteP implements finite? which tests if the argument is a finite number.
func primitiveFiniteP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return true
	case *big.Rat:
		_ = v
		return true
	case float64:
		return !math.IsInf(v, 0) && !math.IsNaN(v)
	case complex128:
		r, i := real(v), imag(v)
		return !math.IsInf(r, 0) && !math.IsNaN(r) && !math.IsInf(i, 0) && !math.IsNaN(i)
	default:
		return false
	}
}

// primitiveInfiniteP implements infinite? which tests if the argument is infinite.
func primitiveInfiniteP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64, *big.Rat:
		_ = v
		return false
	case float64:
		return math.IsInf(v, 0)
	case complex128:
		return math.IsInf(real(v), 0) || math.IsInf(imag(v), 0)
	default:
		return false
	}
}

// primitiveNanP implements nan? which tests if the argument is NaN (not a number).
func primitiveNanP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64, *big.Rat:
		_ = v
		return false
	case float64:
		return math.IsNaN(v)
	case complex128:
		return math.IsNaN(real(v)) || math.IsNaN(imag(v))
	default:
		return false
	}
}
