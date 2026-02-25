package gscheme

import (
	"fmt"
	"math"
)

// Primitive is a lambda that is defined as part of the Scheme report, and has a body implemented in Go code.
type Primitive interface {
	Procedure
}

type primitive struct {
	procedure
	minArgs, maxArgs uint16
	body             func(Pair) interface{}
}

const maxArgs = math.MaxUint16
const minArgs = uint16(0)

func NewPrimitive(name Symbol, minArgs, maxArgs uint16, body func(Pair) interface{}) Primitive {
	return &primitive{
		procedure: procedure{name: name},
		body:      body,
		minArgs:   minArgs,
		maxArgs:   maxArgs}
}

func installPrimitives(environment Environment) {
	installMathPrimitives(environment)
	installSymbolPrimitives(environment)
}

func (p primitive) String() string {
	return fmt.Sprintf("#<primitive %v>", p.name)
}

func (p primitive) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	numArgs := Len(args)
	if numArgs < int(p.minArgs) {
		return Err(fmt.Sprintf("Too few args, %d, for %s: %v", numArgs, p.name, args), nil)
	} else if numArgs > int(p.maxArgs) {
		return Err(fmt.Sprintf("Too many args, %d, for %s: %v", numArgs, p.name, args), nil)
	}
	return p.body(interpreter.EvalList(args, environment))
}
