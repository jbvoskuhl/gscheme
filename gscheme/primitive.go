package gscheme

import "math"

// Primitive is a lambda that is defined as part of the Scheme report, and has a body implemented in Go code.
type Primitive interface {
	Procedure
}

type primitive struct {
	procedure
	minArgs, maxArgs uint16
	body             func(interface{}) interface{}
}

const maxArgs = math.MaxUint16
const minArgs = uint16(0)

func NewPrimitive(name Symbol, minArgs, maxArgs uint16, body func(interface{}) interface{}) Primitive {
	return &primitive{
		procedure: procedure{name: name},
		body:      body,
		minArgs:   minArgs,
		maxArgs:   maxArgs}
}

func installPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("*", minArgs, maxArgs, times))
	environment.DefineName(NewPrimitive("+", minArgs, maxArgs, plus))
	environment.DefineName(NewPrimitive("-", 1, maxArgs, minus))
	environment.DefineName(NewPrimitive("/", 1, maxArgs, divide))
}

func (p primitive) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	numArgs := Len(args)
	if numArgs < int(p.minArgs) {
		return Err("Too few args, %d, for %s: %v", List(numArgs, p.name, args))
	} else if numArgs > int(p.maxArgs) {
		return Err("Too many args, %d, for %s: %v", List(numArgs, p.name, args))
	}
	return p.body(interpreter.EvalList(args, environment))
}
