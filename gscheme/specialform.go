package gscheme

// SpecialForm interface implements a special form like lambda or quote.  The built-in special forms
// (quote, define, set!, lambda, macro, begin, if, cond) are handled inline in Eval() for tail call
// optimization.  This type exists for user-defined special forms.
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