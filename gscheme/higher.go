package gscheme

import (
	"fmt"
	"math/big"
	"runtime"
	"time"
)

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

func (p *higherOrderPrimitive) String() string {
	return fmt.Sprintf("#<primitive %v>", p.name)
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
	environment.DefineName(NewHigherOrderPrimitive("map", 2, maxArgs, primitiveMap))
	environment.DefineName(NewHigherOrderPrimitive("apply", 2, maxArgs, primitiveApply))
	environment.DefineName(NewHigherOrderPrimitive("eval", 1, 2, primitiveEval))
	environment.DefineName(NewHigherOrderPrimitive("for-each", 2, maxArgs, primitiveForEach))
	environment.DefineName(NewHigherOrderPrimitive("member", 2, 3, primitiveMember))
	environment.DefineName(NewPrimitive("memq", 2, 2, primitiveMemq))
	environment.DefineName(NewPrimitive("memv", 2, 2, primitiveMemv))
	environment.DefineName(NewHigherOrderPrimitive("assoc", 2, 3, primitiveAssoc))
	environment.DefineName(NewPrimitive("assq", 2, 2, primitiveAssq))
	environment.DefineName(NewPrimitive("assv", 2, 2, primitiveAssv))
	environment.DefineName(NewPrimitive("error", 1, maxArgs, primitiveError))
	environment.DefineName(NewPrimitive("raise", 1, 1, primitiveRaise))
	environment.DefineName(NewPrimitive("error-object-message", 1, 1, primitiveErrorObjectMessage))
	environment.DefineName(NewPrimitive("error-object-irritants", 1, 1, primitiveErrorObjectIrritants))
	environment.DefineName(NewHigherOrderPrimitive("call-with-current-continuation", 1, 1, primitiveCallCC))
	environment.DefineName(NewHigherOrderPrimitive("call/cc", 1, 1, primitiveCallCC))
	environment.DefineName(NewHigherOrderPrimitive("interaction-environment", 0, 0,
		func(s Scheme, args Pair, env Environment) interface{} {
			return s.Environment()
		}))
	environment.DefineName(NewHigherOrderPrimitive("with-exception-handler", 2, 2, primitiveWithExceptionHandler))
	environment.DefineName(NewPrimitive("raise-continuable", 1, 1, primitiveRaise))
	environment.DefineName(NewHigherOrderPrimitive("macro-expand", 1, 1, primitiveMacroExpand))
	environment.DefineName(NewHigherOrderPrimitive("time-call", 1, 2, primitiveTimeCall))
}

// primitiveMap applies a procedure to corresponding elements of one or more lists.
func primitiveMap(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("map: first argument must be a procedure", List(First(args)))
	}
	// Collect list cursors from all args after proc
	rest := RestPair(args)
	var cursors []interface{}
	for rest != nil {
		cursors = append(cursors, First(rest))
		rest = RestPair(rest)
	}
	// Check if any cursor is already nil
	for _, c := range cursors {
		if c == nil {
			return nil
		}
	}
	var result Pair
	var tail Pair
	for {
		// Build quoted arg list from First() of each cursor
		var quotedArgs Pair
		var qTail Pair
		for i, c := range cursors {
			listPair, ok := c.(Pair)
			if !ok {
				return Err("map: argument must be a list", List(c))
			}
			quoted := List(Symbol("quote"), First(listPair))
			newPair := NewPair(quoted, nil)
			if quotedArgs == nil {
				quotedArgs = newPair
				qTail = quotedArgs
			} else {
				qTail.SetRest(newPair)
				qTail = newPair
			}
			cursors[i] = listPair.Rest()
		}
		mapped := proc.Apply(interpreter, quotedArgs, environment)
		newPair := NewPair(mapped, nil)
		if result == nil {
			result = newPair
			tail = result
		} else {
			tail.SetRest(newPair)
			tail = newPair
		}
		// Stop when any cursor is nil
		done := false
		for _, c := range cursors {
			if c == nil {
				done = true
				break
			}
		}
		if done {
			break
		}
	}
	return result
}

// primitiveApply applies a procedure to arguments. The last argument must be a list;
// preceding arguments are prepended: (apply + 1 2 '(3 4)) applies + to (1 2 3 4).
func primitiveApply(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("apply: first argument must be a procedure", List(First(args)))
	}
	// Collect all args after proc
	rest := RestPair(args)
	var allArgs []interface{}
	for rest != nil {
		allArgs = append(allArgs, First(rest))
		rest = RestPair(rest)
	}
	// Last arg is the list tail; preceding args are prepended
	lastArg := allArgs[len(allArgs)-1]
	// Build the combined arg list: prepend individual args, then the final list
	var combined interface{} = lastArg
	for i := len(allArgs) - 2; i >= 0; i-- {
		combined = Cons(allArgs[i], combined)
	}
	// Quote each argument since Apply will try to evaluate them
	var quotedArgs Pair
	var tail Pair
	for argPair, ok := combined.(Pair); ok; argPair, ok = Rest(argPair).(Pair) {
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

// primitiveForEach applies a procedure to corresponding elements of one or more lists for side effects.
func primitiveForEach(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("for-each: first argument must be a procedure", List(First(args)))
	}
	// Collect list cursors from all args after proc
	rest := RestPair(args)
	var cursors []interface{}
	for rest != nil {
		cursors = append(cursors, First(rest))
		rest = RestPair(rest)
	}
	// Check if any cursor is already nil
	for _, c := range cursors {
		if c == nil {
			return nil
		}
	}
	for {
		// Build quoted arg list from First() of each cursor
		var quotedArgs Pair
		var qTail Pair
		for i, c := range cursors {
			listPair, ok := c.(Pair)
			if !ok {
				return Err("for-each: argument must be a list", List(c))
			}
			quoted := List(Symbol("quote"), First(listPair))
			newPair := NewPair(quoted, nil)
			if quotedArgs == nil {
				quotedArgs = newPair
				qTail = quotedArgs
			} else {
				qTail.SetRest(newPair)
				qTail = newPair
			}
			cursors[i] = listPair.Rest()
		}
		proc.Apply(interpreter, quotedArgs, environment)
		// Stop when any cursor is nil
		done := false
		for _, c := range cursors {
			if c == nil {
				done = true
				break
			}
		}
		if done {
			break
		}
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
		if yBig, ok := y.(*big.Int); ok {
			return yBig.IsInt64() && yBig.Int64() == xInt
		}
		return false
	}
	if xBig, ok := x.(*big.Int); ok {
		if yBig, ok := y.(*big.Int); ok {
			return xBig.Cmp(yBig) == 0
		}
		if yInt, ok := y.(int64); ok {
			return xBig.IsInt64() && xBig.Int64() == yInt
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
// Optional third argument is a comparison procedure.
func primitiveMember(interpreter Scheme, args Pair, environment Environment) interface{} {
	obj := First(args)
	list := Second(args)
	compare := Third(args)
	var comparator Applyer
	if compare != nil {
		var ok bool
		comparator, ok = compare.(Applyer)
		if !ok {
			return Err("member: third argument must be a procedure", List(compare))
		}
	}
	for list != nil {
		if listPair, ok := list.(Pair); ok {
			if comparator != nil {
				quotedArgs := List(List(Symbol("quote"), obj), List(Symbol("quote"), First(listPair)))
				result := comparator.Apply(interpreter, quotedArgs, environment)
				if Truth(result) {
					return list
				}
			} else if equal(obj, First(listPair)) {
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
// Optional third argument is a comparison procedure.
func primitiveAssoc(interpreter Scheme, args Pair, environment Environment) interface{} {
	key := First(args)
	alist := Second(args)
	compare := Third(args)
	var comparator Applyer
	if compare != nil {
		var ok bool
		comparator, ok = compare.(Applyer)
		if !ok {
			return Err("assoc: third argument must be a procedure", List(compare))
		}
	}
	for alist != nil {
		if alistPair, ok := alist.(Pair); ok {
			entry := First(alistPair)
			if entryPair, ok := entry.(Pair); ok {
				if comparator != nil {
					quotedArgs := List(List(Symbol("quote"), key), List(Symbol("quote"), First(entryPair)))
					result := comparator.Apply(interpreter, quotedArgs, environment)
					if Truth(result) {
						return entry
					}
				} else if equal(key, First(entryPair)) {
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

// primitiveErrorObjectMessage returns the message string from an error object.
func primitiveErrorObjectMessage(args Pair) interface{} {
	err, ok := First(args).(Error)
	if !ok {
		return Err("error-object-message: not an error object", args)
	}
	return err.GetMessage()
}

// primitiveErrorObjectIrritants returns the list of irritants from an error object.
func primitiveErrorObjectIrritants(args Pair) interface{} {
	err, ok := First(args).(Error)
	if !ok {
		return Err("error-object-irritants: not an error object", args)
	}
	irritants := err.GetIrritants()
	if irritants == nil {
		return nil
	}
	return irritants
}

// primitiveRaise raises a value as an exception. If the value is already an Error, re-raise it directly.
func primitiveRaise(args Pair) interface{} {
	value := First(args)
	if err, ok := value.(Error); ok {
		panic(err)
	}
	panic(&raisedError{value})
}

// primitiveWithExceptionHandler installs a handler and calls a thunk.
// If the thunk raises an exception, the handler is called with the raised value.
func primitiveWithExceptionHandler(s Scheme, args Pair, env Environment) interface{} {
	handler, ok := First(args).(Applyer)
	if !ok {
		return Err("with-exception-handler: first argument must be a procedure", List(First(args)))
	}
	thunk, ok := Second(args).(Applyer)
	if !ok {
		return Err("with-exception-handler: second argument must be a procedure", List(Second(args)))
	}
	result := thunk.Apply(s, nil, env)
	if err, ok := result.(Error); ok {
		val := unwrapRaisedValue(err)
		quoted := List(List(Symbol("quote"), val))
		return handler.Apply(s, quoted, env)
	}
	return result
}

// primitiveMacroExpand expands a macro call without evaluating the result.
// Usage: (macro-expand '(when #t 42)) — the expression must be quoted.
func primitiveMacroExpand(interpreter Scheme, args Pair, environment Environment) interface{} {
	expr := First(args)
	exprPair, ok := expr.(Pair)
	if !ok {
		return expr
	}
	sym, ok := First(exprPair).(Symbol)
	if !ok {
		return expr
	}
	val := interpreter.Eval(sym, environment)
	m, ok := val.(Macro)
	if !ok {
		return expr
	}
	return m.Expand(interpreter, RestPair(exprPair), environment)
}

// primitiveTimeCall times the execution of a zero-argument thunk.
// Usage: (time-call thunk [n-times])
// Returns (result ((time-ms "elapsed") (mem-bytes "allocated")))
func primitiveTimeCall(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("time-call: first argument must be a procedure", List(First(args)))
	}
	n := int64(1)
	if Second(args) != nil {
		switch v := Second(args).(type) {
		case int64:
			n = v
		case float64:
			n = int64(v)
		default:
			return Err("time-call: second argument must be a number", List(Second(args)))
		}
	}

	var memBefore runtime.MemStats
	runtime.ReadMemStats(&memBefore)

	start := time.Now()
	var result interface{}
	for i := int64(0); i < n; i++ {
		result = proc.Apply(interpreter, nil, environment)
	}
	elapsed := time.Since(start)

	var memAfter runtime.MemStats
	runtime.ReadMemStats(&memAfter)
	allocBytes := memAfter.TotalAlloc - memBefore.TotalAlloc

	timeEntry := List(Symbol("time-ms"), fmt.Sprintf("%.3f", float64(elapsed.Microseconds())/1000.0))
	memEntry := List(Symbol("mem-bytes"), fmt.Sprintf("%d", allocBytes))
	stats := List(timeEntry, memEntry)
	return Cons(result, Cons(stats, nil))
}
