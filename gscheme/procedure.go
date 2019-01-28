package gscheme

import "fmt"

type Procedure interface {
	fmt.Stringer
	Applyer
	Named
	SetName(name Symbol)
}

type procedure struct {
	name Symbol
}

func NewProcedure() Procedure {
	return &procedure{}
}

func (p procedure) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	return Err("Unimplemented procedure has been invoked.  This is likely a bug: ", args)
}

func (p procedure) String() string {
	return fmt.Sprintf("{%v}", p.Name())
}

func (p procedure) Name() Symbol {
	if p.name == "" {
		return Symbol("anonymous procedure")
	}
	return p.name
}

func (p *procedure) SetName(name Symbol) {
	p.name = name
}
