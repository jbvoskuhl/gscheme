package gscheme

import (
	"math"
	"math/big"
	"math/cmplx"
)

// installComplexPrimitives adds complex number primitives to a given environment.
func installComplexPrimitives(environment Environment) {
	// Type predicates
	environment.DefineName(NewPrimitive("complex?", 1, 1, primitiveComplexP))
	environment.DefineName(NewPrimitive("real?", 1, 1, primitiveRealP))

	// Constructors
	environment.DefineName(NewPrimitive("make-rectangular", 2, 2, primitiveMakeRectangular))
	environment.DefineName(NewPrimitive("make-polar", 2, 2, primitiveMakePolar))

	// Accessors
	environment.DefineName(NewPrimitive("real-part", 1, 1, primitiveRealPart))
	environment.DefineName(NewPrimitive("imag-part", 1, 1, primitiveImagPart))
	environment.DefineName(NewPrimitive("magnitude", 1, 1, primitiveMagnitude))
	environment.DefineName(NewPrimitive("angle", 1, 1, primitiveAngle))

	// Additional math functions that work with complex numbers
	environment.DefineName(NewPrimitive("sqrt", 1, 1, primitiveSqrt))
	environment.DefineName(NewPrimitive("exp", 1, 1, primitiveExp))
	environment.DefineName(NewPrimitive("log", 1, 1, primitiveLog))
	environment.DefineName(NewPrimitive("sin", 1, 1, primitiveSin))
	environment.DefineName(NewPrimitive("cos", 1, 1, primitiveCos))
	environment.DefineName(NewPrimitive("tan", 1, 1, primitiveTan))
	environment.DefineName(NewPrimitive("asin", 1, 1, primitiveAsin))
	environment.DefineName(NewPrimitive("acos", 1, 1, primitiveAcos))
	environment.DefineName(NewPrimitive("atan", 1, 2, primitiveAtan))
}

// primitiveComplexP implements complex? which tests if the argument is a complex number.
// In Scheme, all numbers are complex (reals are complex with zero imaginary part).
func primitiveComplexP(args Pair) interface{} {
	x := First(args)
	switch x.(type) {
	case int64, *big.Rat, float64, complex128:
		return true
	default:
		return false
	}
}

// primitiveRealP implements real? which tests if the argument is a real number.
// A complex number is real if its imaginary part is zero.
func primitiveRealP(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return true
	case *big.Rat:
		_ = v
		return true
	case float64:
		return true
	case complex128:
		return imag(v) == 0
	default:
		return false
	}
}

// primitiveMakeRectangular implements make-rectangular which creates a complex number
// from real and imaginary parts.
func primitiveMakeRectangular(args Pair) interface{} {
	realPart := Num(First(args))
	imagPart := Num(Second(args))
	if imagPart == 0 {
		return realPart
	}
	return complex(realPart, imagPart)
}

// primitiveMakePolar implements make-polar which creates a complex number
// from magnitude and angle (in radians).
func primitiveMakePolar(args Pair) interface{} {
	mag := Num(First(args))
	ang := Num(Second(args))
	realPart := mag * math.Cos(ang)
	imagPart := mag * math.Sin(ang)
	if imagPart == 0 {
		return realPart
	}
	return complex(realPart, imagPart)
}

// primitiveRealPart implements real-part which returns the real part of a complex number.
func primitiveRealPart(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return v
	case *big.Rat:
		return v
	case float64:
		return v
	case complex128:
		return real(v)
	default:
		return Err("real-part: expected number", args)
	}
}

// primitiveImagPart implements imag-part which returns the imaginary part of a complex number.
func primitiveImagPart(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		return int64(0)
	case *big.Rat:
		_ = v
		return int64(0)
	case float64:
		_ = v
		return float64(0)
	case complex128:
		return imag(v)
	default:
		return Err("imag-part: expected number", args)
	}
}

// primitiveMagnitude implements magnitude which returns the absolute value of a complex number.
func primitiveMagnitude(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		if v < 0 {
			return -v
		}
		return v
	case *big.Rat:
		return SimplifyRat(new(big.Rat).Abs(v))
	case float64:
		return math.Abs(v)
	case complex128:
		return cmplx.Abs(v)
	default:
		return Err("magnitude: expected number", args)
	}
}

// primitiveAngle implements angle which returns the angle (argument) of a complex number in radians.
func primitiveAngle(args Pair) interface{} {
	x := First(args)
	switch v := x.(type) {
	case int64:
		if v >= 0 {
			return float64(0)
		}
		return math.Pi
	case *big.Rat:
		if v.Sign() >= 0 {
			return float64(0)
		}
		return math.Pi
	case float64:
		if v >= 0 {
			return float64(0)
		}
		return math.Pi
	case complex128:
		return cmplx.Phase(v)
	default:
		return Err("angle: expected number", args)
	}
}

// numToFloat64 converts int64 or float64 to float64 for math functions.
func numToFloat64(x interface{}) (float64, bool) {
	switch v := x.(type) {
	case int64:
		return float64(v), true
	case *big.Rat:
		f, _ := v.Float64()
		return f, true
	case float64:
		return v, true
	default:
		return 0, false
	}
}

// primitiveSqrt implements sqrt which returns the square root of a number.
// For negative reals, returns a complex result.
func primitiveSqrt(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		if f >= 0 {
			result := math.Sqrt(f)
			if IsInt64(x) && result == math.Trunc(result) {
				return int64(result)
			}
			return result
		}
		return complex(0, math.Sqrt(-f))
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Sqrt(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("sqrt: expected number", args)
}

// primitiveExp implements exp (e^x) for real and complex numbers.
func primitiveExp(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		return math.Exp(f)
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Exp(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("exp: expected number", args)
}

// primitiveLog implements log (natural logarithm) for real and complex numbers.
func primitiveLog(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		if f > 0 {
			return math.Log(f)
		}
		return cmplx.Log(complex(f, 0))
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Log(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("log: expected number", args)
}

// primitiveSin implements sin for real and complex numbers.
func primitiveSin(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		return math.Sin(f)
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Sin(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("sin: expected number", args)
}

// primitiveCos implements cos for real and complex numbers.
func primitiveCos(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		return math.Cos(f)
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Cos(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("cos: expected number", args)
}

// primitiveTan implements tan for real and complex numbers.
func primitiveTan(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		return math.Tan(f)
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Tan(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("tan: expected number", args)
}

// primitiveAsin implements asin for real and complex numbers.
func primitiveAsin(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		if f >= -1 && f <= 1 {
			return math.Asin(f)
		}
		return cmplx.Asin(complex(f, 0))
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Asin(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("asin: expected number", args)
}

// primitiveAcos implements acos for real and complex numbers.
func primitiveAcos(args Pair) interface{} {
	x := First(args)
	if f, ok := numToFloat64(x); ok {
		if f >= -1 && f <= 1 {
			return math.Acos(f)
		}
		return cmplx.Acos(complex(f, 0))
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Acos(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("acos: expected number", args)
}

// primitiveAtan implements atan for real and complex numbers.
// With one argument: atan(x)
// With two arguments: atan2(y, x)
func primitiveAtan(args Pair) interface{} {
	x := First(args)
	if Rest(args) != nil {
		y := Num(x)
		xVal := Num(Second(args))
		return math.Atan2(y, xVal)
	}
	if f, ok := numToFloat64(x); ok {
		return math.Atan(f)
	}
	if c, ok := x.(complex128); ok {
		result := cmplx.Atan(c)
		if imag(result) == 0 {
			return real(result)
		}
		return result
	}
	return Err("atan: expected number", args)
}

// ToComplex converts a Scheme number to complex128.
func ToComplex(x interface{}) complex128 {
	switch v := x.(type) {
	case int64:
		return complex(float64(v), 0)
	case *big.Rat:
		f, _ := v.Float64()
		return complex(f, 0)
	case float64:
		return complex(v, 0)
	case complex128:
		return v
	default:
		return complex(Num(x), 0)
	}
}

// SimplifyComplex returns a float64 if the imaginary part is zero, otherwise the complex128.
func SimplifyComplex(c complex128) interface{} {
	if imag(c) == 0 {
		return real(c)
	}
	return c
}
