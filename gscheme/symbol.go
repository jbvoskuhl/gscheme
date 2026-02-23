package gscheme

// Symbol represents an identifier in Scheme.
type Symbol string

// installSymbolPrimitives adds all the symbol related primitives to a given environment.
func installSymbolPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("symbol?", 1, 1, isSymbolPrimitive))
	environment.DefineName(NewPrimitive("symbol=?", 2, maxArgs, primitiveSymbolEqual))
	environment.DefineName(NewPrimitive("symbol->string", 1, 1, symbolToStringPrimitive))
	environment.DefineName(NewPrimitive("string->symbol", 1, 1, stringToSymbolPrimitive))
}

// primitiveSymbolEqual checks if all symbol arguments are equal.
func primitiveSymbolEqual(args Pair) interface{} {
	first := symbolConstraint(First(args))
	rest := RestPair(args)
	for rest != nil {
		if symbolConstraint(First(rest)) != first {
			return false
		}
		rest = RestPair(rest)
	}
	return true
}

// isSymbol is shorthand for a test for whether an object is a Symbol or not.
func isSymbolPrimitive(args Pair) interface{} {
	arg := First(args)
	_, ok := arg.(Symbol)
	return ok
}

// symbolToString converts a symbol into the corresponding string.
func symbolToStringPrimitive(args Pair) interface{} {
	return string(symbolConstraint(First(args)))
}

// stringToSymbol converts a string into the corresponding Symbol.
func stringToSymbolPrimitive(args Pair) interface{} {
	return Symbol(stringConstraint(First(args)))
}

func (s Symbol) String() string {
	return string(s)
}
