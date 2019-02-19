package gscheme

// installBooleanPrimitives adds the primitives that deal with boolean objects.
func installBooleanPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("boolean?", 1, 1, primitiveBoolean))
	environment.DefineName(NewPrimitive("boolean=?", 2, maxArgs, primitiveBooleanEqual))
	environment.DefineName(NewPrimitive("not", 1, 1, primitiveNot))
}

// primitiveBoolean tests if an object is a boolean.
func primitiveBoolean(args Pair) interface{} {
	arg := First(args)
	_, ok := arg.(bool)
	return ok
}

// primitiveBooleanEqual evaluates whether or not all the arguments are equal or not.
func primitiveBooleanEqual(args Pair) interface{} {
	current := booleanConstraint(First(args))
	args = RestPair(args)
	for args != nil {
		next := booleanConstraint(First(args))
		if current != next {
			return false
		}
		current = next
		args = RestPair(args)
	}
	return true
}

// primitiveNot negates the truthiness of its input.
func primitiveNot(args Pair) interface{} {
	return !Truth(First(args))
}
