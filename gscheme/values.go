package gscheme

// MultipleValues represents the result of (values v1 v2 ...).
// (values x) with a single argument returns x directly for transparency in
// single-value contexts. Zero or two-or-more arguments return a MultipleValues.
type MultipleValues []interface{}

// installValuesPrimitives adds values and call-with-values to the environment.
func installValuesPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("values", 0, maxArgs, primitiveValues))
	environment.DefineName(NewHigherOrderPrimitive("call-with-values", 2, 2, primitiveCallWithValues))
}

// primitiveValues returns its arguments as multiple values.
// (values x) with a single argument passes through x unchanged.
// (values) with no arguments returns an empty MultipleValues.
func primitiveValues(args Pair) interface{} {
	if args == nil {
		return MultipleValues{}
	}
	if Rest(args) == nil {
		return First(args)
	}
	var result []interface{}
	for args != nil {
		result = append(result, First(args))
		args = RestPair(args)
	}
	return MultipleValues(result)
}

// primitiveCallWithValues calls producer (a zero-argument thunk), then applies
// consumer to the resulting values as individual arguments.
func primitiveCallWithValues(s Scheme, args Pair, env Environment) interface{} {
	producer, ok := First(args).(Applyer)
	if !ok {
		return Err("call-with-values: first argument must be a procedure", List(First(args)))
	}
	consumer, ok := Second(args).(Applyer)
	if !ok {
		return Err("call-with-values: second argument must be a procedure", List(Second(args)))
	}
	result := producer.Apply(s, nil, env)

	// Unwrap multiple values; a plain result is treated as (values x).
	var vals []interface{}
	if mv, ok := result.(MultipleValues); ok {
		vals = []interface{}(mv)
	} else {
		vals = []interface{}{result}
	}

	// Build a quoted-args list so consumer.Apply evaluates each value as itself.
	var quotedArgs Pair
	var tail Pair
	for _, v := range vals {
		quoted := List(Symbol("quote"), v)
		newPair := NewPair(quoted, nil)
		if quotedArgs == nil {
			quotedArgs = newPair
			tail = quotedArgs
		} else {
			tail.SetRest(newPair)
			tail = newPair
		}
	}
	return consumer.Apply(s, quotedArgs, env)
}
