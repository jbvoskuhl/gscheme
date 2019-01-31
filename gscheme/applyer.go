package gscheme

// Applyer is implemented by anything that's callable including macros, lambdas, primitives and user-defined functions.
type Applyer interface {
	Apply(interpreter Scheme, args Pair, environment Environment) interface{}
}
