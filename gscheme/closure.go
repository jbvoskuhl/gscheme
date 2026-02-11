package gscheme

// Closure is a user-defined procedure. It is "closed" over the environment in which it was created.
// To apply the procedure, bind the parameters to the passed in variables, and evaluate the body.
type Closure interface {
	Procedure
}

type closure struct {
	procedure
	params interface{} // Parameter list (may be a Pair, Symbol, or nil)
	body   interface{} // Body expression (single expression or begin block)
	env    Environment // Captured environment
}

// NewClosure creates a closure from a parameter list, body, and environment.
func NewClosure(params interface{}, body interface{}, env Environment) Closure {
	// If body is a single expression, unwrap it; otherwise wrap in begin
	bodyExpr := body
	if bodyPair, ok := body.(Pair); ok && Rest(bodyPair) == nil {
		bodyExpr = First(bodyPair)
	} else if body != nil {
		bodyExpr = Cons(Symbol("begin"), body)
	}
	return &closure{
		params: params,
		body:   bodyExpr,
		env:    env,
	}
}

// Apply evaluates the closure body in a new environment with parameters bound to arguments.
func (c *closure) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	// Evaluate the arguments in the caller's environment
	evaledArgs := interpreter.EvalList(args, environment)
	// Create a new environment extending the closure's captured environment
	newEnv := NewChildEnvironmentWithBindings(c.params, evaledArgs, c.env)
	// Evaluate the body in the new environment
	return interpreter.Eval(c.body, newEnv)
}
