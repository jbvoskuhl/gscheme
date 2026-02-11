package gscheme

// Macro is a syntax transformer. Unlike a closure which receives evaluated arguments
// and returns a value, a macro receives unevaluated arguments (syntax) and returns
// code to be evaluated.
type Macro interface {
	Procedure
	Expand(interpreter Scheme, args Pair, environment Environment) interface{}
}

type macro struct {
	procedure
	params interface{} // Parameter list (may be a Pair, Symbol, or nil)
	body   interface{} // Body expression
	env    Environment // Captured environment
}

// NewMacro creates a macro from a parameter list, body, and environment.
func NewMacro(params interface{}, body interface{}, env Environment) Macro {
	// If body is a single expression, unwrap it; otherwise wrap in begin
	bodyExpr := body
	if bodyPair, ok := body.(Pair); ok && Rest(bodyPair) == nil {
		bodyExpr = First(bodyPair)
	} else if body != nil {
		bodyExpr = Cons(Symbol("begin"), body)
	}
	return &macro{
		params: params,
		body:   bodyExpr,
		env:    env,
	}
}

// Apply for a macro shouldn't normally be called directly - use Expand instead.
// This is here to satisfy the Procedure interface.
func (m *macro) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	// Macros receive unevaluated arguments, so we bind them directly without evaluation
	newEnv := NewChildEnvironmentWithBindings(m.params, args, m.env)
	return interpreter.Eval(m.body, newEnv)
}

// Expand applies the macro to the unevaluated arguments and returns the expansion.
// The expansion is code that should then be evaluated.
func (m *macro) Expand(interpreter Scheme, args Pair, environment Environment) interface{} {
	// Bind the unevaluated arguments to the macro parameters
	newEnv := NewChildEnvironmentWithBindings(m.params, args, m.env)
	// Evaluate the macro body to get the expansion
	return interpreter.Eval(m.body, newEnv)
}

// IsMacro checks if a value is a macro.
func IsMacro(x interface{}) bool {
	_, ok := x.(Macro)
	return ok
}
