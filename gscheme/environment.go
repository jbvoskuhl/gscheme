package gscheme

// Environment defines a set of bindings that symbols are evaluated against.
type Environment interface {
	Define(symbol Symbol, value interface{}) interface{}
	DefineName(name Namer) interface{}
	Set(symbol Symbol, value interface{}) bool
	Lookup(symbol Symbol) (result interface{}, ok bool)
}

// Namer is an interface on any object that knows its own name.
type Namer interface {
	Name() Symbol
}

// environment is the internal representation of a scheme environment.
type environment struct {
	bindings map[Symbol]interface{}
	parent   Environment
}

// NewRootEnvironment creates a new empty root environment to create bindings in via Set or Define.
func NewRootEnvironment() Environment {
	return &environment{bindings: make(map[Symbol]interface{})}
}

// NewChildEnvironment creates an new environment that defers lookups to its parent.
func NewChildEnvironment(parent Environment) Environment {
	return &environment{bindings: make(map[Symbol]interface{}), parent: parent}
}

// NewChildEnvironmentWithBindings creates a new environment with parameters bound to values.
// Params can be:
//   - A Pair (list) of symbols: each symbol is bound to the corresponding argument
//   - A single Symbol: the symbol is bound to the entire argument list (variadic)
//   - nil: no parameters to bind
func NewChildEnvironmentWithBindings(params interface{}, args Pair, parent Environment) Environment {
	env := NewChildEnvironment(parent)
	bindParams(env, params, args)
	return env
}

// bindParams binds parameter names to argument values in the given environment.
func bindParams(env Environment, params interface{}, args Pair) {
	for {
		switch p := params.(type) {
		case nil:
			return
		case Symbol:
			// Rest parameter: bind the symbol to remaining args
			env.Define(p, args)
			return
		case Pair:
			// Bind first param to first arg, continue with rest
			if sym, ok := First(p).(Symbol); ok {
				env.Define(sym, First(args))
			}
			params = Rest(p)
			args = RestPair(args)
		default:
			return
		}
	}
}

// Define creates or silently overwrites a binding.
func (e environment) Define(symbol Symbol, value interface{}) interface{} {
	e.bindings[symbol] = value
	return value
}

// DefineNamed creates or silently overwrites a binding for an object that knows its own name.
func (e environment) DefineName(name Namer) interface{} {
	return e.Define(Symbol(name.Name()), name)
}

// Set modifies an existing binding in this environment or a parent environment.
// Returns true if the binding was found and updated, false if the variable is unbound.
func (e *environment) Set(symbol Symbol, value interface{}) bool {
	// Check if bound locally
	if _, ok := e.bindings[symbol]; ok {
		e.bindings[symbol] = value
		return true
	}
	// If not, try parent
	if e.parent != nil {
		return e.parent.Set(symbol, value)
	}
	return false
}

// Lookup finds the value bound o a given symbol if it exists.
func (e environment) Lookup(symbol Symbol) (result interface{}, ok bool) {
	result, ok = e.bindings[symbol]
	if !ok && e.parent != nil {
		return e.parent.Lookup(symbol)
	}
	return result, ok
}
