package gscheme

// SpecialForm interface implements a special form like if, cond, when and the like.
type SpecialForm interface {
	Procedure
}

type specialForm struct {
	procedure
	body func(Scheme, interface{}, Environment) interface{}
}

func NewSpecialForm(name Symbol, body func(Scheme, interface{}, Environment) interface{}) SpecialForm {
	return &specialForm{
		procedure: procedure{name: name},
		body:      body,
	}
}

func (s specialForm) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	return s.body(interpreter, args, environment)
}

func installSpecialForms(environment Environment) {
	environment.DefineNamed(NewSpecialForm(Symbol("if"), specialIf))
}

func specialIf(interpreter Scheme, args interface{}, environment Environment) interface{} {
	condition := First(args)
	if Truth(interpreter.Eval(condition, environment)) {
		consequent := Second(args)
		return interpreter.Eval(consequent, environment)
	} else {
		alternate := Third(args)
		return interpreter.Eval(alternate, environment)
	}
}
