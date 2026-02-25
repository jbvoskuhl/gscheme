package gscheme

import "fmt"

// Procedure is the interfacing describing anything callable in Scheme – primitives, lambdas, macros or special forms.
type Procedure interface {
	fmt.Stringer
	Applyer
	Namer
	SetName(name Symbol)
}

// procedure objects (usually) know their own name.
type procedure struct {
	name Symbol
}

// NewProcedure creates a new anonymous procedure.
func NewProcedure() Procedure {
	return &procedure{}
}

// Apply defines the basic calling convention for Scheme.
func (p procedure) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	return Err("Unimplemented procedure has been invoked.  This is likely a bug: ", args)
}

// String creates the display form of a procedure object.
func (p procedure) String() string {
	if p.name == "" {
		return "#<procedure>"
	}
	return fmt.Sprintf("#<procedure %v>", p.name)
}

// Name returns the name of procedure; it returns an empty symbol if no name has been set.
func (p procedure) Name() Symbol {
	return p.name
}

// SetName rebinds the name to something new.
func (p *procedure) SetName(name Symbol) {
	p.name = name
}
