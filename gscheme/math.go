package gscheme

import (
	"math"
	"math/big"
)

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
	environment.DefineName(NewPrimitive("rationalize", 2, 2, primitiveRationalize))
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
			return mulInt64(xInt, yInt)
		}
	}
	if isExactInt(x) && isExactInt(y) {
		result := new(big.Int).Mul(ToBigInt(x), ToBigInt(y))
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		return SimplifyRat(new(big.Rat).Mul(ToRat(x), ToRat(y)))
	}
	return Num(x) * Num(y)
}

func binaryPlus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) + ToComplex(y))
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return addInt64(xInt, yInt)
		}
	}
	if isExactInt(x) && isExactInt(y) {
		result := new(big.Int).Add(ToBigInt(x), ToBigInt(y))
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		return SimplifyRat(new(big.Rat).Add(ToRat(x), ToRat(y)))
	}
	return Num(x) + Num(y)
}

func binaryMinus(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) - ToComplex(y))
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return subInt64(xInt, yInt)
		}
	}
	if isExactInt(x) && isExactInt(y) {
		result := new(big.Int).Sub(ToBigInt(x), ToBigInt(y))
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		return SimplifyRat(new(big.Rat).Sub(ToRat(x), ToRat(y)))
	}
	return Num(x) - Num(y)
}

func binaryDivide(x, y interface{}) interface{} {
	if isComplex(x) || isComplex(y) {
		return SimplifyComplex(ToComplex(x) / ToComplex(y))
	}
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		mod := new(big.Int).Mod(bx, by)
		if mod.Sign() == 0 {
			return SimplifyBigInt(new(big.Int).Div(bx, by))
		}
		return SimplifyRat(new(big.Rat).SetFrac(new(big.Int).Set(bx), new(big.Int).Set(by)))
	}
	if isExact(x) && isExact(y) {
		return SimplifyRat(new(big.Rat).Quo(ToRat(x), ToRat(y)))
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
// Uses int64 fast path when both operands are int64, avoiding big.Rat allocation.
// When both operands are exact (int64 or *big.Rat), comparison is done exactly via big.Rat.
func numCompare(compare func(x, y float64) bool, ratCompare func(int) bool,
	int64Compare func(x, y int64) bool) func(Pair) interface{} {
	return func(args Pair) interface{} {
		prev := First(args)
		args = RestPair(args)
		for args != nil {
			cur := First(args)
			if xInt, ok := prev.(int64); ok {
				if yInt, ok := cur.(int64); ok {
					if !int64Compare(xInt, yInt) {
						return false
					}
					prev = cur
					args = RestPair(args)
					continue
				}
			}
			if isExact(prev) && isExact(cur) {
				if !ratCompare(ToRat(prev).Cmp(ToRat(cur))) {
					return false
				}
			} else {
				if !compare(Num(prev), Num(cur)) {
					return false
				}
			}
			prev = cur
			args = RestPair(args)
		}
		return true
	}
}

var numEqual = numCompare(func(x, y float64) bool { return x == y }, func(c int) bool { return c == 0 }, func(x, y int64) bool { return x == y })
var numLessThan = numCompare(func(x, y float64) bool { return x < y }, func(c int) bool { return c < 0 }, func(x, y int64) bool { return x < y })
var numGreaterThan = numCompare(func(x, y float64) bool { return x > y }, func(c int) bool { return c > 0 }, func(x, y int64) bool { return x > y })
var numLessThanOrEqual = numCompare(func(x, y float64) bool { return x <= y }, func(c int) bool { return c <= 0 }, func(x, y int64) bool { return x <= y })
var numGreaterThanOrEqual = numCompare(func(x, y float64) bool { return x >= y }, func(c int) bool { return c >= 0 }, func(x, y int64) bool { return x >= y })

func primitiveAbs(args Pair) interface{} {
	x := First(args)
	if v, ok := x.(int64); ok {
		if v < 0 {
			return negInt64(v)
		}
		return v
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(new(big.Int).Abs(v))
	}
	if r, ok := x.(*big.Rat); ok {
		return SimplifyRat(new(big.Rat).Abs(r))
	}
	return math.Abs(Num(x))
}

func primitiveMax(args Pair) interface{} {
	result := First(args)
	inexact := !isExact(result)
	args = RestPair(args)
	for args != nil {
		x := First(args)
		if !isExact(x) {
			inexact = true
		}
		if isExact(result) && isExact(x) {
			if ToRat(x).Cmp(ToRat(result)) > 0 {
				result = x
			}
		} else {
			if Num(x) > Num(result) {
				result = x
			}
		}
		args = RestPair(args)
	}
	if inexact {
		return Num(result)
	}
	return result
}

func primitiveMin(args Pair) interface{} {
	result := First(args)
	inexact := !isExact(result)
	args = RestPair(args)
	for args != nil {
		x := First(args)
		if !isExact(x) {
			inexact = true
		}
		if isExact(result) && isExact(x) {
			if ToRat(x).Cmp(ToRat(result)) < 0 {
				result = x
			}
		} else {
			if Num(x) < Num(result) {
				result = x
			}
		}
		args = RestPair(args)
	}
	if inexact {
		return Num(result)
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
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		result := new(big.Int).Quo(bx, by)
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		xr, yr := ToRat(x), ToRat(y)
		q := new(big.Rat).Quo(xr, yr)
		num, den := q.Num(), q.Denom()
		result := new(big.Int).Quo(num, den)
		return SimplifyBigInt(result)
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
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		result := new(big.Int).Rem(bx, by)
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		xr, yr := ToRat(x), ToRat(y)
		q := new(big.Rat).Quo(xr, yr)
		qTrunc := new(big.Int).Quo(q.Num(), q.Denom())
		rem := new(big.Rat).Sub(xr, new(big.Rat).Mul(new(big.Rat).SetInt(qTrunc), yr))
		return SimplifyRat(rem)
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
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		result := new(big.Int).Mod(bx, by)
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		xr, yr := ToRat(x), ToRat(y)
		q := new(big.Rat).Quo(xr, yr)
		qNum, qDen := q.Num(), q.Denom()
		qFloor := new(big.Int).Div(qNum, qDen)
		mod := new(big.Rat).Sub(xr, new(big.Rat).Mul(new(big.Rat).SetInt(qFloor), yr))
		return SimplifyRat(mod)
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
	allExactInt := true
	resultBig := big.NewInt(0)
	resultFloat := float64(0)
	for args != nil {
		x := First(args)
		if r, ok := x.(*big.Rat); ok && r.IsInt() {
			x = SimplifyBigInt(r.Num())
		}
		if isExactInt(x) && allExactInt {
			resultBig.GCD(nil, nil, resultBig, new(big.Int).Abs(ToBigInt(x)))
		} else {
			if allExactInt {
				allExactInt = false
				resultFloat, _ = new(big.Float).SetInt(resultBig).Float64()
			}
			resultFloat = gcd(resultFloat, Num(x))
		}
		args = RestPair(args)
	}
	if allExactInt {
		return SimplifyBigInt(resultBig)
	}
	return resultFloat
}

func primitiveLcm(args Pair) interface{} {
	allExactInt := true
	resultBig := big.NewInt(1)
	resultFloat := float64(1)
	for args != nil {
		x := First(args)
		if r, ok := x.(*big.Rat); ok && r.IsInt() {
			x = SimplifyBigInt(r.Num())
		}
		if isExactInt(x) && allExactInt {
			v := new(big.Int).Abs(ToBigInt(x))
			if v.Sign() == 0 {
				return int64(0)
			}
			g := new(big.Int).GCD(nil, nil, resultBig, v)
			resultBig.Div(resultBig, g)
			resultBig.Mul(resultBig, v)
		} else {
			if allExactInt {
				allExactInt = false
				resultFloat, _ = new(big.Float).SetInt(resultBig).Float64()
			}
			xf := math.Abs(Num(x))
			if xf == 0 {
				return float64(0)
			}
			resultFloat = resultFloat / gcd(resultFloat, xf) * xf
		}
		args = RestPair(args)
	}
	if allExactInt {
		return SimplifyBigInt(resultBig)
	}
	return resultFloat
}

func primitiveExpt(args Pair) interface{} {
	base, exp := First(args), Second(args)
	// Exact integer base with non-negative exact integer exponent
	if isExactInt(base) {
		if eInt, ok := exp.(int64); ok && eInt >= 0 {
			b := ToBigInt(base)
			result := new(big.Int).Exp(b, big.NewInt(eInt), nil)
			return SimplifyBigInt(result)
		}
		if eBig, ok := exp.(*big.Int); ok && eBig.Sign() >= 0 {
			b := ToBigInt(base)
			result := new(big.Int).Exp(b, eBig, nil)
			return SimplifyBigInt(result)
		}
	}
	if bRat, ok := base.(*big.Rat); ok {
		if eInt, ok := exp.(int64); ok && eInt >= 0 {
			result := new(big.Rat).SetInt64(1)
			b := new(big.Rat).Set(bRat)
			e := eInt
			for e > 0 {
				if e%2 == 1 {
					result.Mul(result, b)
				}
				b.Mul(b, b)
				e /= 2
			}
			return SimplifyRat(result)
		}
	}
	return math.Pow(Num(base), Num(exp))
}

func primitiveSquare(args Pair) interface{} {
	x := First(args)
	if v, ok := x.(int64); ok {
		return mulInt64(v, v)
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(new(big.Int).Mul(v, v))
	}
	if r, ok := x.(*big.Rat); ok {
		return SimplifyRat(new(big.Rat).Mul(r, r))
	}
	f := Num(x)
	return f * f
}

func primitiveFloor(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(v)
	}
	if r, ok := x.(*big.Rat); ok {
		q := new(big.Int).Div(r.Num(), r.Denom())
		return SimplifyBigInt(q)
	}
	return int64(math.Floor(Num(x)))
}

func primitiveCeiling(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(v)
	}
	if r, ok := x.(*big.Rat); ok {
		if r.IsInt() {
			return SimplifyBigInt(r.Num())
		}
		q := new(big.Int).Div(r.Num(), r.Denom())
		q.Add(q, big.NewInt(1))
		return SimplifyBigInt(q)
	}
	return int64(math.Ceil(Num(x)))
}

func primitiveRound(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(v)
	}
	if r, ok := x.(*big.Rat); ok {
		f, _ := r.Float64()
		return int64(math.RoundToEven(f))
	}
	return int64(math.RoundToEven(Num(x)))
}

func primitiveTruncate(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if v, ok := x.(*big.Int); ok {
		return SimplifyBigInt(v)
	}
	if r, ok := x.(*big.Rat); ok {
		num, den := r.Num(), r.Denom()
		q := new(big.Int).Quo(num, den)
		return SimplifyBigInt(q)
	}
	return int64(math.Trunc(Num(x)))
}

func primitiveExactToInexact(args Pair) interface{} {
	x := First(args)
	if v, ok := x.(int64); ok {
		return float64(v)
	}
	if v, ok := x.(*big.Int); ok {
		f, _ := new(big.Float).SetInt(v).Float64()
		return f
	}
	if r, ok := x.(*big.Rat); ok {
		f, _ := r.Float64()
		return f
	}
	return Num(x)
}

func primitiveInexactToExact(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if _, ok := x.(*big.Int); ok {
		return x
	}
	if _, ok := x.(*big.Rat); ok {
		return x
	}
	f := Num(x)
	r := new(big.Rat).SetFloat64(f)
	if r == nil {
		// SetFloat64 returns nil for Inf/NaN
		return int64(f)
	}
	return SimplifyRat(r)
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
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		result := new(big.Int).Div(bx, by)
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		xr, yr := ToRat(x), ToRat(y)
		q := new(big.Rat).Quo(xr, yr)
		result := new(big.Int).Div(q.Num(), q.Denom())
		return SimplifyBigInt(result)
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
	if isExactInt(x) && isExactInt(y) {
		bx, by := ToBigInt(x), ToBigInt(y)
		result := new(big.Int).Mod(bx, by)
		return SimplifyBigInt(result)
	}
	if isExact(x) && isExact(y) {
		xr, yr := ToRat(x), ToRat(y)
		q := new(big.Rat).Quo(xr, yr)
		qFloor := new(big.Int).Div(q.Num(), q.Denom())
		mod := new(big.Rat).Sub(xr, new(big.Rat).Mul(new(big.Rat).SetInt(qFloor), yr))
		return SimplifyRat(mod)
	}
	xf, yf := Num(x), Num(y)
	return xf - yf*math.Floor(xf/yf)
}

func primitiveNumerator(args Pair) interface{} {
	x := First(args)
	if _, ok := x.(int64); ok {
		return x
	}
	if _, ok := x.(*big.Int); ok {
		return x
	}
	if r, ok := x.(*big.Rat); ok {
		return SimplifyBigInt(r.Num())
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
	if _, ok := x.(*big.Int); ok {
		return int64(1)
	}
	if r, ok := x.(*big.Rat); ok {
		return SimplifyBigInt(r.Denom())
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

// primitiveRationalize returns the simplest rational within y of x.
// Uses the Stern-Brocot mediant search algorithm.
func primitiveRationalize(args Pair) interface{} {
	x, y := First(args), Second(args)
	exactResult := isExact(x) && isExact(y)

	// Convert to *big.Rat for the computation
	var xr, yr *big.Rat
	if isExact(x) {
		xr = ToRat(x)
	} else {
		xr = new(big.Rat).SetFloat64(Num(x))
	}
	if isExact(y) {
		yr = new(big.Rat).Abs(ToRat(y))
	} else {
		yr = new(big.Rat).SetFloat64(math.Abs(Num(y)))
	}

	lo := new(big.Rat).Sub(xr, yr)
	hi := new(big.Rat).Add(xr, yr)

	result := sternBrocot(lo, hi)

	if exactResult {
		return SimplifyRat(result)
	}
	f, _ := result.Float64()
	return f
}

// sternBrocot finds the simplest rational (smallest denominator) in [lo, hi].
func sternBrocot(lo, hi *big.Rat) *big.Rat {
	// If interval contains 0, return 0
	if lo.Sign() <= 0 && hi.Sign() >= 0 {
		return new(big.Rat)
	}

	// If interval is entirely negative, negate, find, negate result
	if hi.Sign() < 0 {
		result := sternBrocot(new(big.Rat).Neg(hi), new(big.Rat).Neg(lo))
		return result.Neg(result)
	}

	// Now lo > 0. Check if interval contains an integer.
	// The smallest positive integer in [lo, hi] is ceil(lo).
	loNum, loDen := lo.Num(), lo.Denom()
	ceilLo := new(big.Int).Add(new(big.Int).Div(loNum, loDen), big.NewInt(1))
	// If lo is itself an integer, ceil(lo) = lo
	if new(big.Int).Mod(loNum, loDen).Sign() == 0 {
		ceilLo = new(big.Int).Div(loNum, loDen)
	}
	ceilRat := new(big.Rat).SetInt(ceilLo)
	if ceilRat.Cmp(lo) >= 0 && ceilRat.Cmp(hi) <= 0 {
		return ceilRat
	}

	// No integer in interval. Use Stern-Brocot / continued fraction approach.
	// Both lo and hi are in (n, n+1) for some integer n. Subtract n, invert, recurse.
	n := new(big.Int).Div(lo.Num(), lo.Denom()) // floor(lo)
	nRat := new(big.Rat).SetInt(n)

	// newLo = 1/(hi - n), newHi = 1/(lo - n)
	loMinusN := new(big.Rat).Sub(lo, nRat)
	hiMinusN := new(big.Rat).Sub(hi, nRat)

	newLo := new(big.Rat).Inv(hiMinusN)
	newHi := new(big.Rat).Inv(loMinusN)

	sub := sternBrocot(newLo, newHi)

	// result = n + 1/sub
	return new(big.Rat).Add(nRat, new(big.Rat).Inv(sub))
}
