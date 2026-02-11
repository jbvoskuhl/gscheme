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
	environment.DefineName(NewSpecialForm(Symbol("set!"), setSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("lambda"), lambdaSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("macro"), macroSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("begin"), beginSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("if"), ifSpecialForm))
	environment.DefineName(NewSpecialForm(Symbol("cond"), condSpecialForm))
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

// macroSpecialForm implements macro which creates a syntax transformer.
// Form: (macro (params...) body...)
// Unlike lambda, a macro receives unevaluated arguments and returns code to be evaluated.
func macroSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	params := First(args)
	body := Rest(args)
	return NewMacro(params, body, environment)
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

// ifSpecialForm implements if which evaluates a condition and returns one of two branches.
// Form: (if test consequent) or (if test consequent alternate)
// If test is true (anything except #f), evaluates and returns consequent.
// Otherwise, evaluates and returns alternate (or nil if not provided).
func ifSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	test := interpreter.Eval(First(args), environment)
	if Truth(test) {
		return interpreter.Eval(Second(args), environment)
	}
	return interpreter.Eval(Third(args), environment)
}

// setSpecialForm implements set! which modifies an existing variable binding.
// Form: (set! name value)
// Returns the new value, or an error if the variable is unbound.
func setSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	name, ok := First(args).(Symbol)
	if !ok {
		return Err("set!: expected symbol", List(First(args)))
	}
	value := interpreter.Eval(Second(args), environment)
	if !environment.Set(name, value) {
		return Err("set!: unbound variable", List(name))
	}
	return value
}

// condSpecialForm implements cond which is a multi-way conditional.
// Form: (cond clause1 clause2 ...)
// Each clause is (test expr1 expr2 ...) or (test => proc) or (else expr1 expr2 ...)
// Evaluates tests in order until one is true, then evaluates its expressions.
func condSpecialForm(interpreter Scheme, args Pair, environment Environment) interface{} {
	for args != nil {
		clause := First(args)
		clausePair, ok := clause.(Pair)
		if !ok {
			return Err("cond: bad clause", List(clause))
		}

		test := First(clausePair)
		var result interface{}

		// Check for 'else' clause
		if test == Symbol("else") {
			result = true
		} else {
			result = interpreter.Eval(test, environment)
		}

		if Truth(result) {
			rest := Rest(clausePair)
			// If no expressions after test, return the test result
			if rest == nil {
				return result
			}
			// Check for => syntax: (test => proc)
			if First(rest) == Symbol("=>") {
				proc := interpreter.Eval(Second(rest), environment)
				if applyer, ok := proc.(Applyer); ok {
					return applyer.Apply(interpreter, List(result), environment)
				}
				return Err("cond: => requires a procedure", List(proc))
			}
			// Otherwise evaluate all expressions and return the last
			return beginSpecialForm(interpreter, RestPair(clausePair), environment)
		}

		args = RestPair(args)
	}
	// No clause matched
	return false
}
