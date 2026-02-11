package gscheme

// installMathPrimitives adds mathematical primitives to a given environment.
func installMathPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("*", minArgs, maxArgs, times))
	environment.DefineName(NewPrimitive("+", minArgs, maxArgs, plus))
	environment.DefineName(NewPrimitive("-", 1, maxArgs, minus))
	environment.DefineName(NewPrimitive("/", 1, maxArgs, divide))
	environment.DefineName(NewPrimitive("=", 2, maxArgs, numEqual))
	environment.DefineName(NewPrimitive("<", 2, maxArgs, numLessThan))
	environment.DefineName(NewPrimitive(">", 2, maxArgs, numGreaterThan))
	environment.DefineName(NewPrimitive("<=", 2, maxArgs, numLessThanOrEqual))
	environment.DefineName(NewPrimitive(">=", 2, maxArgs, numGreaterThanOrEqual))
}

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
	return Num(x) * Num(y)
}

func binaryPlus(x, y interface{}) interface{} {
	return Num(x) + Num(y)
}

func binaryMinus(x, y interface{}) interface{} {
	return Num(x) - Num(y)
}

func binaryDivide(x, y interface{}) interface{} {
	return Num(x) / Num(y)
}

var times = reduce(binaryTimes, float64(1))

var plus = reduce(binaryPlus, float64(0))

func minus(args Pair) interface{} {
	if Rest(args) == nil {
		return binaryMinus(float64(0), First(args))
	} else {
		return reduce(binaryMinus, Num(First(args)))(RestPair(args))
	}
}

func divide(args Pair) interface{} {
	if Rest(args) == nil {
		return binaryDivide(float64(1), First(args))
	} else {
		return reduce(binaryDivide, Num(First(args)))(RestPair(args))
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
