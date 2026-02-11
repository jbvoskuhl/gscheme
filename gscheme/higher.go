package gscheme

// HigherOrderPrimitive is a primitive that needs access to the interpreter for evaluation.
type HigherOrderPrimitive interface {
	Procedure
}

type higherOrderPrimitive struct {
	procedure
	minArgs, maxArgs uint16
	body             func(Scheme, Pair, Environment) interface{}
}

// NewHigherOrderPrimitive creates a primitive that has access to the interpreter.
func NewHigherOrderPrimitive(name Symbol, minArgs, maxArgs uint16, body func(Scheme, Pair, Environment) interface{}) HigherOrderPrimitive {
	return &higherOrderPrimitive{
		procedure: procedure{name: name},
		body:      body,
		minArgs:   minArgs,
		maxArgs:   maxArgs,
	}
}

// Apply evaluates the arguments and calls the body with interpreter access.
func (p *higherOrderPrimitive) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	numArgs := Len(args)
	if numArgs < int(p.minArgs) {
		return Err("Too few args for "+string(p.name), List(args))
	} else if numArgs > int(p.maxArgs) {
		return Err("Too many args for "+string(p.name), List(args))
	}
	evaledArgs := interpreter.EvalList(args, environment)
	return p.body(interpreter, evaledArgs, environment)
}

// installHigherOrderPrimitives adds primitives that need interpreter access.
func installHigherOrderPrimitives(environment Environment) {
	environment.DefineName(NewHigherOrderPrimitive("map", 2, 2, primitiveMap))
	environment.DefineName(NewHigherOrderPrimitive("apply", 2, 2, primitiveApply))
	environment.DefineName(NewHigherOrderPrimitive("eval", 1, 2, primitiveEval))
	environment.DefineName(NewPrimitive("member", 2, 2, primitiveMember))
	environment.DefineName(NewPrimitive("error", 1, maxArgs, primitiveError))
}

// primitiveMap applies a procedure to each element of a list.
func primitiveMap(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("map: first argument must be a procedure", List(First(args)))
	}
	list := Second(args)
	if list == nil {
		return nil
	}
	listPair, ok := list.(Pair)
	if !ok {
		return Err("map: second argument must be a list", List(list))
	}

	var result Pair
	var tail Pair
	for listPair != nil {
		// Apply the procedure to the current element
		// Quote the element since Apply will try to evaluate it
		elem := First(listPair)
		quotedArg := List(List(Symbol("quote"), elem))
		mapped := proc.Apply(interpreter, quotedArg, environment)
		newPair := NewPair(mapped, nil)
		if result == nil {
			result = newPair
			tail = result
		} else {
			tail.SetRest(newPair)
			tail = newPair
		}
		listPair = RestPair(listPair)
	}
	return result
}

// primitiveApply applies a procedure to a list of arguments.
func primitiveApply(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("apply: first argument must be a procedure", List(First(args)))
	}
	argList := Second(args)
	// Quote each argument since Apply will try to evaluate them
	var quotedArgs Pair
	var tail Pair
	for argPair, ok := argList.(Pair); ok; argPair, ok = Rest(argPair).(Pair) {
		quoted := List(Symbol("quote"), First(argPair))
		newPair := NewPair(quoted, nil)
		if quotedArgs == nil {
			quotedArgs = newPair
			tail = quotedArgs
		} else {
			tail.SetRest(newPair)
			tail = newPair
		}
	}
	return proc.Apply(interpreter, quotedArgs, environment)
}

// primitiveEval evaluates an expression in the given environment (or global if not provided).
func primitiveEval(interpreter Scheme, args Pair, environment Environment) interface{} {
	expr := First(args)
	env := environment
	if Second(args) != nil {
		if e, ok := Second(args).(Environment); ok {
			env = e
		}
	}
	return interpreter.Eval(expr, env)
}

// primitiveMember returns the sublist starting with the first occurrence of obj, or #f.
func primitiveMember(args Pair) interface{} {
	obj := First(args)
	list := Second(args)
	for list != nil {
		if listPair, ok := list.(Pair); ok {
			if equal(obj, First(listPair)) {
				return list
			}
			list = listPair.Rest()
		} else {
			break
		}
	}
	return false
}

// primitiveError raises an error with the given message.
func primitiveError(args Pair) interface{} {
	return Err(Stringify(First(args)), RestPair(args))
}
