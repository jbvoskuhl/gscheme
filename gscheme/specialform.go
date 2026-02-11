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
	environment.DefineName(NewSpecialForm(Symbol("define"), defineSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("lambda"), lambdaSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("begin"), beginSpecialForm))
}

// quoteSpecialForm implements quote from Scheme and returns the first argument unevaluated.
func quoteSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	return First(args)
}

// defineSpecialForm implements define which binds a name to a value in the current environment.
// Supports two forms:
//   - (define name value) - binds name to the evaluated value
//   - (define (name params...) body...) - shorthand for (define name (lambda (params...) body...))
func defineSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	first := First(args)
	if firstPair, ok := first.(Pair); ok {
		// Function shorthand: (define (name params...) body...)
		// Transform to: (define name (lambda (params...) body...))
		name, ok := First(firstPair).(Symbol)
		if !ok {
			return Err("define: first element must be a symbol", List(first))
		}
		params := Rest(firstPair)
		body := Rest(args)
		lambda := NewClosure(params, body, environment)
		lambda.SetName(name)
		return environment.Define(name, lambda)
	}
	// Simple form: (define name value)
	name, ok := first.(Symbol)
	if !ok {
		return Err("define: expected symbol", List(first))
	}
	value := interpreter.Eval(Second(args), environment)
	// If we're defining a procedure, give it a name
	if proc, ok := value.(Procedure); ok && proc.Name() == "anonymous procedure" {
		proc.SetName(name)
	}
	return environment.Define(name, value)
}

// lambdaSpecialForm implements lambda which creates a closure (user-defined procedure).
// Form: (lambda (params...) body...)
func lambdaSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	params := First(args)
	body := Rest(args)
	return NewClosure(params, body, environment)
}

// beginSpecialForm implements begin which evaluates expressions in sequence and returns the last result.
// Form: (begin expr1 expr2 ...)
func beginSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	var result interface{}
	for args != nil {
		result = interpreter.Eval(First(args), environment)
		args = RestPair(args)
	}
	return result
}
