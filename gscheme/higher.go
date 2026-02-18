package gscheme

import "math/big"

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
	environment.DefineName(NewHigherOrderPrimitive("for-each", 2, 2, primitiveForEach))
	environment.DefineName(NewPrimitive("member", 2, 2, primitiveMember))
	environment.DefineName(NewPrimitive("memq", 2, 2, primitiveMemq))
	environment.DefineName(NewPrimitive("memv", 2, 2, primitiveMemv))
	environment.DefineName(NewPrimitive("assoc", 2, 2, primitiveAssoc))
	environment.DefineName(NewPrimitive("assq", 2, 2, primitiveAssq))
	environment.DefineName(NewPrimitive("assv", 2, 2, primitiveAssv))
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

// primitiveForEach applies a procedure to each element of a list for side effects, returns nil.
func primitiveForEach(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("for-each: first argument must be a procedure", List(First(args)))
	}
	list := Second(args)
	if list == nil {
		return nil
	}
	listPair, ok := list.(Pair)
	if !ok {
		return Err("for-each: second argument must be a list", List(list))
	}
	for listPair != nil {
		elem := First(listPair)
		quotedArg := List(List(Symbol("quote"), elem))
		proc.Apply(interpreter, quotedArg, environment)
		listPair = RestPair(listPair)
	}
	return nil
}

// eqv compares two values using eqv? semantics (identity + number/char value comparison).
// Per R7RS, (eqv? 1 1.0) is #f (different exactness).
func eqv(x, y interface{}) bool {
	if x == y {
		return true
	}
	if xInt, ok := x.(int64); ok {
		if yInt, ok := y.(int64); ok {
			return xInt == yInt
		}
		return false
	}
	if xRat, ok := x.(*big.Rat); ok {
		if yRat, ok := y.(*big.Rat); ok {
			return xRat.Cmp(yRat) == 0
		}
		return false
	}
	if xNum, ok := x.(float64); ok {
		if yNum, ok := y.(float64); ok {
			return xNum == yNum
		}
		return false
	}
	if xChar, ok := x.(rune); ok {
		if yChar, ok := y.(rune); ok {
			return xChar == yChar
		}
	}
	return false
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

// primitiveMemq returns the sublist starting with the first occurrence of obj using eq? (identity).
func primitiveMemq(args Pair) interface{} {
	obj := First(args)
	list := Second(args)
	for list != nil {
		if listPair, ok := list.(Pair); ok {
			if obj == First(listPair) {
				return list
			}
			list = listPair.Rest()
		} else {
			break
		}
	}
	return false
}

// primitiveMemv returns the sublist starting with the first occurrence of obj using eqv? comparison.
func primitiveMemv(args Pair) interface{} {
	obj := First(args)
	list := Second(args)
	for list != nil {
		if listPair, ok := list.(Pair); ok {
			if eqv(obj, First(listPair)) {
				return list
			}
			list = listPair.Rest()
		} else {
			break
		}
	}
	return false
}

// primitiveAssoc searches an alist for a pair whose car equals key using equal?.
func primitiveAssoc(args Pair) interface{} {
	key := First(args)
	alist := Second(args)
	for alist != nil {
		if alistPair, ok := alist.(Pair); ok {
			entry := First(alistPair)
			if entryPair, ok := entry.(Pair); ok {
				if equal(key, First(entryPair)) {
					return entry
				}
			}
			alist = alistPair.Rest()
		} else {
			break
		}
	}
	return false
}

// primitiveAssq searches an alist for a pair whose car equals key using eq? (identity).
func primitiveAssq(args Pair) interface{} {
	key := First(args)
	alist := Second(args)
	for alist != nil {
		if alistPair, ok := alist.(Pair); ok {
			entry := First(alistPair)
			if entryPair, ok := entry.(Pair); ok {
				if key == First(entryPair) {
					return entry
				}
			}
			alist = alistPair.Rest()
		} else {
			break
		}
	}
	return false
}

// primitiveAssv searches an alist for a pair whose car equals key using eqv? comparison.
func primitiveAssv(args Pair) interface{} {
	key := First(args)
	alist := Second(args)
	for alist != nil {
		if alistPair, ok := alist.(Pair); ok {
			entry := First(alistPair)
			if entryPair, ok := entry.(Pair); ok {
				if eqv(key, First(entryPair)) {
					return entry
				}
			}
			alist = alistPair.Rest()
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
