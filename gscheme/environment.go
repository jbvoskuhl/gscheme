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

// Define creates or silently overwrites a binding.
func (e environment) Define(symbol Symbol, value interface{}) interface{} {
	e.bindings[symbol] = value
	return value
}

// DefineNamed creates or silently overwrites a binding for an object that knows its own name.
func (e environment) DefineName(name Namer) interface{} {
	return e.Define(Symbol(name.Name()), name)
}

// Set assigns a symbol a value in this environment if it doesn't already exist.
func (e environment) Set(symbol Symbol, value interface{}) bool {
	_, ok := e.Lookup(symbol)
	e.bindings[symbol] = value
	return ok
}

// Lookup finds the value bound o a given symbol if it exists.
func (e environment) Lookup(symbol Symbol) (result interface{}, ok bool) {
	result, ok = e.bindings[symbol]
	if !ok && e.parent != nil {
		return e.parent.Lookup(symbol)
	}
	return result, ok
}
