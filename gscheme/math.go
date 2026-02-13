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
	environment.DefineName(NewPrimitive("exact", 1, 1, primitiveInexactToExact))
	environment.DefineName(NewPrimitive("inexact", 1, 1, primitiveExactToInexact))
	environment.DefineName(NewPrimitive("numerator", 1, 1, primitiveNumerator))
	environment.DefineName(NewPrimitive("denominator", 1, 1, primitiveDenominator))
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
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt * yInt
		}
	}
	return Num(x) * Num(y)
}

func binaryPlus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) + ToComplex(y))
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt + yInt
		}
	}
	return Num(x) + Num(y)
}

func binaryMinus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) - ToComplex(y))
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt - yInt
		}
	}
	return Num(x) - Num(y)
}

func binaryDivide(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) / ToComplex(y))
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			if yInt != 0 && xInt%yInt == 0 {
				return xInt / yInt
			}
		}
	}
	return Num(x) / Num(y)
}

var times = reduce(binaryTimes, int64(1))

var plus = reduce(binaryPlus, int64(0))

func minus(args Pair) interface{} {
	first := First(args)
	if Rest(args) == nil {
		return binaryMinus(int64(0), first)
	} else {
		return reduce(binaryMinus, first)(RestPair(args))
	}
}

func divide(args Pair) interface{} {
	first := First(args)
	if Rest(args) == nil {
		return binaryDivide(int64(1), first)
	} else {
		return reduce(binaryDivide, first)(RestPair(args))
	}
}

// numCompare compares adjacent pairs of numbers using the given comparison function.
// Returns true if all adjacent pairs satisfy the comparison, false otherwise.
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
	x := First(args)
	if v, ok := x.(int64); ok {
		if v < 0 {
			return -v
		}
		return v
	}
	return math.Abs(Num(x))
}

func primitiveMax(args Pair) interface{} {
	result := First(args)
	resultF := Num(result)
	inexact := IsFloat64(result)
	args = RestPair(args)
	for args != nil {
		x := First(args)
		xF := Num(x)
		if IsFloat64(x) {
			inexact = true
		}
		if xF > resultF {
			result = x
			resultF = xF
		}
		args = RestPair(args)
	}
	if inexact {
		return resultF
	}
	return result
}

func primitiveMin(args Pair) interface{} {
	result := First(args)
	resultF := Num(result)
	inexact := IsFloat64(result)
	args = RestPair(args)
	for args != nil {
		x := First(args)
		xF := Num(x)
		if IsFloat64(x) {
			inexact = true
		}
		if xF < resultF {
			result = x
			resultF = xF
		}
		args = RestPair(args)
	}
	if inexact {
		return resultF
	}
	return result
}

func primitiveQuotient(args Pair) interface{} {
	x, y := First(args), Second(args)
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt / yInt
		}
	}
	return int64(math.Trunc(Num(x) / Num(y)))
}

func primitiveRemainder(args Pair) interface{} {
	x, y := First(args), Second(args)
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt % yInt
		}
	}
	xf, yf := Num(x), Num(y)
	return xf - math.Trunc(xf/yf)*yf
}

func primitiveModulo(args Pair) interface{} {
	x, y := First(args), Second(args)
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			m := xInt % yInt
			if m != 0 && (m < 0) != (yInt < 0) {
				m += yInt
			}
			return m
		}
	}
	xf, yf := Num(x), Num(y)
	return xf - yf*math.Floor(xf/yf)
}

func gcd64(a, b int64) int64 {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a, b = b, a%b
	}
	return a
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
	allInt := true
	resultInt := int64(0)
	resultFloat := float64(0)
	for args != nil {
		x := First(args)
		if v, ok := x.(int64); ok && allInt {
			resultInt = gcd64(resultInt, v)
		} else {
			if allInt {
				allInt = false
				resultFloat = float64(resultInt)
			}
			resultFloat = gcd(resultFloat, Num(x))
		}
		args = RestPair(args)
	}
	if allInt {
		return resultInt
	}
	return resultFloat
}

func primitiveLcm(args Pair) interface{} {
	allInt := true
	resultInt := int64(1)
	resultFloat := float64(1)
	for args != nil {
		x := First(args)
		if v, ok := x.(int64); ok && allInt {
			if v < 0 {
				v = -v
			}
			if v == 0 {
				return int64(0)
			}
			resultInt = resultInt / gcd64(resultInt, v) * v
		} else {
			if allInt {
				allInt = false
				resultFloat = float64(resultInt)
			}
			xf := math.Abs(Num(x))
			if xf == 0 {
				return float64(0)
			}
			resultFloat = resultFloat / gcd(resultFloat, xf) * xf
		}
		args = RestPair(args)
	}
	if allInt {
		return resultInt
	}
	return resultFloat
}

func primitiveExpt(args Pair) interface{} {
	base, exp := First(args), Second(args)
	if bInt, ok := base.(int64); ok {
		if eInt, ok := exp.(int64); ok && eInt >= 0 {
			result := int64(1)
			b := bInt
			e := eInt
			for e > 0 {
				if e%2 == 1 {
					result *= b
				}
				b *= b
				e /= 2
			}
			return result
		}
	}
	return math.Pow(Num(base), Num(exp))
}

func primitiveSquare(args Pair) interface{} {
	x := First(args)
	if v, ok := x.(int64); ok {
		return v * v
	}
	f := Num(x)
	return f * f
}

func primitiveFloor(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	return int64(math.Floor(Num(x)))
}

func primitiveCeiling(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	return int64(math.Ceil(Num(x)))
}

func primitiveRound(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	return int64(math.RoundToEven(Num(x)))
}

func primitiveTruncate(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	return int64(math.Trunc(Num(x)))
}

func primitiveExactToInexact(args Pair) interface{} {
	x := First(args)
	if v, ok := x.(int64); ok {
		return float64(v)
	}
	return Num(x)
}

func primitiveInexactToExact(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	f := Num(x)
	if f == math.Trunc(f) && !math.IsInf(f, 0) && !math.IsNaN(f) {
		return int64(f)
	}
	return int64(f)
}

func primitiveFloorQuotient(args Pair) interface{} {
	x, y := First(args), Second(args)
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			q := xInt / yInt
			if (xInt%yInt != 0) && ((xInt < 0) != (yInt < 0)) {
				q--
			}
			return q
		}
	}
	return int64(math.Floor(Num(x) / Num(y)))
}

func primitiveFloorRemainder(args Pair) interface{} {
	x, y := First(args), Second(args)
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			m := xInt % yInt
			if m != 0 && (m < 0) != (yInt < 0) {
				m += yInt
			}
			return m
		}
	}
	xf, yf := Num(x), Num(y)
	return xf - yf*math.Floor(xf/yf)
}

func primitiveNumerator(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	f := Num(x)
	if f == math.Trunc(f) {
		return f
	}
	// For non-integer float64, approximate via the float64 representation.
	sign := 1.0
	if f < 0 {
		sign = -1.0
		f = -f
	}
	d := 1.0
	for f != math.Trunc(f) {
		f *= 2
		d *= 2
	}
	g := gcd(f, d)
	return sign * f / g
}

func primitiveDenominator(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return int64(1)
	}
	f := Num(x)
	if f == math.Trunc(f) {
		return float64(1)
	}
	if f < 0 {
		f = -f
	}
	d := 1.0
	for f != math.Trunc(f) {
		f *= 2
		d *= 2
	}
	g := gcd(f, d)
	return d / g
}
