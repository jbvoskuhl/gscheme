package gscheme

// SpecialForm interface implements a special form like lambda or quote.  Note that any special form that has special
// tail-call handling can't be implemented this way and must be special cased inside of Scheme.Eval().
type SpecialForm interface {
	Procedure
}

type specialForm struct {
	procedure
	body func(Scheme, Pair, Environment) interface{}
}

// Return a new special form that hands off control to a function that gets the unevaluated arguments.
func NewSpecialForm(name Symbol, body func(Scheme, Pair, Environment) interface{}) SpecialForm {
	return &specialForm{
		procedure: procedure{name: name},
		body:      body,
	}
}

// Apply call the body function with the arguments unevaluated, unlike a lambda or primitive.
func (s specialForm) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	return s.body(interpreter, args, environment)
}

// installSpecialForms will add all the special forms to a given environment to bootstrap them.
func installSpecialForms(environment Environment) {
	environment.DefineName(NewSpecialForm(Symbol("quote"), quoteSpecialForm))
}

// quoteSpecialForm implements quote from Scheme and returns the first argument unevaluated.
func quoteSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	return First(args)
}
