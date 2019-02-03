package gscheme

// installMathPrimitives adds mathematical primitives to a given environment.
func installMathPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("*", minArgs, maxArgs, times))
	environment.DefineName(NewPrimitive("+", minArgs, maxArgs, plus))
	environment.DefineName(NewPrimitive("-", 1, maxArgs, minus))
	environment.DefineName(NewPrimitive("/", 1, maxArgs, divide))
}

func reduce(binary func(x, y interface{}) interface{}, unary interface{}) func(interface{}) interface{} {
	return func(args interface{}) interface{} {
		result := unary
		for args != nil {
			result = binary(result, First(args))
			args = Rest(args)
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

func minus(args interface{}) interface{} {
	if Rest(args) == nil {
		return binaryMinus(float64(0), First(args))
	} else {
		return reduce(binaryMinus, Num(First(args)))(Rest(args))
	}
}

func divide(args interface{}) interface{} {
	if Rest(args) == nil {
		return binaryDivide(float64(1), First(args))
	} else {
		return reduce(binaryDivide, Num(First(args)))(Rest(args))
	}
}
