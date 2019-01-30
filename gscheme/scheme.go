// gscheme implements a simple lightweight Scheme interpreter suitable for embedding into Go programs.
package gscheme

import (
	"fmt"
	"io/ioutil"
)

// This respresents an instance of the Scheme interpreter.  Instantiate this then evaluate programs here.
type Scheme interface {
	Environment() Environment
	Eval(x interface{}, environment Environment) interface{}
	EvalList(list interface{}, environment Environment) interface{}
	EvalGlobal(x interface{}) interface{}
	LoadCode(code string) Scheme
	LoadFile(file string) Scheme
	LoadFiles(file []string) Scheme
	ReadEvalPrintLoop()
}

// scheme is the internal representation of the lightweight gscheme runtime.
type scheme struct {
	// input InputPort
	// output *Printer
	environment Environment
}

// Create a new gscheme interpreter – they are completely independent and it is safe to create several of them
// as needed.  Extend the set of built-in primitives to allow your go code to interact with Scheme.
func New() Scheme {
	result := &scheme{environment: NewRootEnvironment()}
	installSpecialForms(result.environment)
	installPrimitives(result.environment)
	result.LoadCode(SCHEME_PRIMITIVES_CODE)
	return result
}

// LoadCode evaluates the code represented as a string to bootstrap the environment.
func (s *scheme) LoadCode(code string) Scheme {
	return s
}

// LoadFile loads code from a file to bootstrap the environment.
func (s *scheme) LoadFile(filename string) Scheme {
	content, err := ioutil.ReadFile(filename)
	if err != nil {
		// TODO(jbvoskuhl): Make this fail more gracefully; bootstrap files must exist or later the system will explode.
		return s
	}
	return s.LoadCode(string(content))
}

// LoadFiles loads code from several files to bootstrap the environment.
func (s *scheme) LoadFiles(filenames []string) Scheme {
	for _, filename := range filenames {
		s.LoadFile(filename)
	}
	return s
}

// ReadEvalPrintLoop is a convenient option for exploring the gscheme environment outside of its use as an embedded
// scripting language.
func (s *scheme) ReadEvalPrintLoop() {
	fmt.Println("> ")
}

func (s *scheme) Environment() Environment {
	return s.environment
}

// Eval will evaluate a single Scheme expression with respect to a given environment and return the result.
func (s *scheme) Eval(x interface{}, environment Environment) interface{} {
	// The purpose of the while loop is to allow tail recursion.
	// The idea is that in a tail recursive position, we do "x = ..."
	// and loop, rather than doing "return eval(...)".
	for {
		switch value := x.(type) {
		case Symbol:
			variable, ok := environment.Lookup(value)
			if !ok {
				return Err("Unbound variable: ", List(variable))
			}
			return variable
		case Pair:
			return s.EvalCombination(s.Eval(First(value), environment), Rest(value), environment)
		default:
			return x
		}
	}
}

// EvalList evaluates each expression in turn and returns the last one.
func (s *scheme) EvalList(list interface{}, environment Environment) interface{} {
	if list == nil {
		return nil
	} else {
		return Cons(s.Eval(First(list), environment), s.EvalList(Rest(list), environment))
	}
}

// EvalCombination evaluates the first item in the list and applies the remaining arguments to it.
func (s *scheme) EvalCombination(first interface{}, rest interface{}, environment Environment) interface{} {
	p, ok := first.(Applyer)
	if !ok {
		return Err("Bad Procedure: ", List(first))
	}
	args, ok := rest.(Pair)
	return p.Apply(s, args, environment)
}

func (s *scheme) EvalGlobal(x interface{}) interface{} {
	return s.Eval(x, s.environment)
}
