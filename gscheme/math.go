package gscheme

import "math"

// installMathPrimitives adds mathematical primitives to a given environment.
func installMathPrimitives(environment Environment) {
	// Arithmetic
	environment.DefineName(NewPrimitive("*", minArgs, maxArgs, times))
	environment.DefineName(NewPrimitive("+", minArgs, maxArgs, plus))
	environment.DefineName(NewPrimitive("-", 1, maxArgs, minus))
	environment.DefineName(NewPrimitive("/", 1, maxArgs, divide))

	// Comparison
	environment.DefineName(NewPrimitive("=", 2, maxArgs, numEqual))
	environment.DefineName(NewPrimitive("<", 2, maxArgs, numLessThan))
	environment.DefineName(NewPrimitive(">", 2, maxArgs, numGreaterThan))
	environment.DefineName(NewPrimitive("<=", 2, maxArgs, numLessThanOrEqual))
	environment.DefineName(NewPrimitive(">=", 2, maxArgs, numGreaterThanOrEqual))

	// R7RS-small math
	environment.DefineName(NewPrimitive("abs", 1, 1, primitiveAbs))
	environment.DefineName(NewPrimitive("max", 1, maxArgs, primitiveMax))
	environment.DefineName(NewPrimitive("min", 1, maxArgs, primitiveMin))
	environment.DefineName(NewPrimitive("quotient", 2, 2, primitiveQuotient))
	environment.DefineName(NewPrimitive("remainder", 2, 2, primitiveRemainder))
	environment.DefineName(NewPrimitive("modulo", 2, 2, primitiveModulo))
	environment.DefineName(NewPrimitive("gcd", minArgs, maxArgs, primitiveGcd))
	environment.DefineName(NewPrimitive("lcm", minArgs, maxArgs, primitiveLcm))
	environment.DefineName(NewPrimitive("expt", 2, 2, primitiveExpt))
	environment.DefineName(NewPrimitive("square", 1, 1, primitiveSquare))
	environment.DefineName(NewPrimitive("floor", 1, 1, primitiveFloor))
	environment.DefineName(NewPrimitive("ceiling", 1, 1, primitiveCeiling))
	environment.DefineName(NewPrimitive("round", 1, 1, primitiveRound))
	environment.DefineName(NewPrimitive("truncate", 1, 1, primitiveTruncate))
	environment.DefineName(NewPrimitive("exact->inexact", 1, 1, primitiveExactToInexact))
	environment.DefineName(NewPrimitive("inexact->exact", 1, 1, primitiveInexactToExact))
	environment.DefineName(NewPrimitive("floor-quotient", 2, 2, primitiveFloorQuotient))
	environment.DefineName(NewPrimitive("floor-remainder", 2, 2, primitiveFloorRemainder))
	environment.DefineName(NewPrimitive("truncate-quotient", 2, 2, primitiveQuotient))
	environment.DefineName(NewPrimitive("truncate-remainder", 2, 2, primitiveRemainder))
}

// isComplex returns true if x is a complex128
func isComplex(x interface{}) bool {
	_, ok := x.(complex128)
	return ok
}

// reduce applies a binary operation across a list of arguments
func reduce(binary func(x, y interface{}) interface{}, unary interface{}) func(Pair) interface{} {
	return func(args Pair) interface{} {
		result := unary
		for args != nil {
			result = binary(result, First(args))
			args = RestPair(args)
		}
		return result
	}
}

func binaryTimes(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) * ToComplex(y))
	}
	return Num(x) * Num(y)
}

func binaryPlus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) + ToComplex(y))
	}
	return Num(x) + Num(y)
}

func binaryMinus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) - ToComplex(y))
	}
	return Num(x) - Num(y)
}

func binaryDivide(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) / ToComplex(y))
	}
	return Num(x) / Num(y)
}

var times = reduce(binaryTimes, float64(1))

var plus = reduce(binaryPlus, float64(0))

func minus(args Pair) interface{} {
	first := First(args)
	if Rest(args) == nil {
		return binaryMinus(float64(0), first)
	} else {
		return reduce(binaryMinus, first)(RestPair(args))
	}
}

func divide(args Pair) interface{} {
	first := First(args)
	if Rest(args) == nil {
		return binaryDivide(float64(1), first)
	} else {
		return reduce(binaryDivide, first)(RestPair(args))
	}
}

// numCompare compares adjacent pairs of numbers using the given comparison function.
// Returns true if all adjacent pairs satisfy the comparison, false otherwise.
// For example, (< 1 2 3) checks 1 < 2 and 2 < 3.
func numCompare(compare func(x, y float64) bool) func(Pair) interface{} {
	return func(args Pair) interface{} {
		x := Num(First(args))
		args = RestPair(args)
		for args != nil {
			y := Num(First(args))
			if !compare(x, y) {
				return false
			}
			x = y
			args = RestPair(args)
		}
		return true
	}
}

var numEqual = numCompare(func(x, y float64) bool { return x == y })
var numLessThan = numCompare(func(x, y float64) bool { return x < y })
var numGreaterThan = numCompare(func(x, y float64) bool { return x > y })
var numLessThanOrEqual = numCompare(func(x, y float64) bool { return x <= y })
var numGreaterThanOrEqual = numCompare(func(x, y float64) bool { return x >= y })

func primitiveAbs(args Pair) interface{} {
	return math.Abs(Num(First(args)))
}

func primitiveMax(args Pair) interface{} {
	result := Num(First(args))
	args = RestPair(args)
	for args != nil {
		x := Num(First(args))
		if x > result {
			result = x
		}
		args = RestPair(args)
	}
	return result
}

func primitiveMin(args Pair) interface{} {
	result := Num(First(args))
	args = RestPair(args)
	for args != nil {
		x := Num(First(args))
		if x < result {
			result = x
		}
		args = RestPair(args)
	}
	return result
}

func primitiveQuotient(args Pair) interface{} {
	x := Num(First(args))
	y := Num(Second(args))
	return math.Trunc(x / y)
}

func primitiveRemainder(args Pair) interface{} {
	x := Num(First(args))
	y := Num(Second(args))
	return x - math.Trunc(x/y)*y
}

func primitiveModulo(args Pair) interface{} {
	x := Num(First(args))
	y := Num(Second(args))
	return x - y*math.Floor(x/y)
}

func gcd(a, b float64) float64 {
	a = math.Abs(a)
	b = math.Abs(b)
	for b != 0 {
		a, b = b, math.Mod(a, b)
	}
	return a
}

func primitiveGcd(args Pair) interface{} {
	result := float64(0)
	for args != nil {
		result = gcd(result, Num(First(args)))
		args = RestPair(args)
	}
	return result
}

func primitiveLcm(args Pair) interface{} {
	result := float64(1)
	for args != nil {
		x := math.Abs(Num(First(args)))
		if x == 0 {
			return float64(0)
		}
		result = result / gcd(result, x) * x
		args = RestPair(args)
	}
	return result
}

func primitiveExpt(args Pair) interface{} {
	base := Num(First(args))
	exp := Num(Second(args))
	return math.Pow(base, exp)
}

func primitiveSquare(args Pair) interface{} {
	x := Num(First(args))
	return x * x
}

func primitiveFloor(args Pair) interface{} {
	return math.Floor(Num(First(args)))
}

func primitiveCeiling(args Pair) interface{} {
	return math.Ceil(Num(First(args)))
}

func primitiveRound(args Pair) interface{} {
	return math.RoundToEven(Num(First(args)))
}

func primitiveTruncate(args Pair) interface{} {
	return math.Trunc(Num(First(args)))
}

func primitiveExactToInexact(args Pair) interface{} {
	return Num(First(args))
}

func primitiveInexactToExact(args Pair) interface{} {
	return Num(First(args))
}

func primitiveFloorQuotient(args Pair) interface{} {
	x := Num(First(args))
	y := Num(Second(args))
	return math.Floor(x / y)
}

func primitiveFloorRemainder(args Pair) interface{} {
	x := Num(First(args))
	y := Num(Second(args))
	return x - y*math.Floor(x/y)
}
