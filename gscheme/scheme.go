// gscheme implements a simple lightweight Scheme interpreter suitable for embedding into Go programs.
package gscheme

import (
	"bufio"
	"fmt"
	"os"
)

// This respresents an instance of the Scheme interpreter.  Instantiate this then evaluate programs here.
type Scheme interface {
	Environment() Environment
	Eval(x interface{}, environment Environment) interface{}
	EvalList(list Pair, environment Environment) Pair
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
	installBooleanPrimitives(result.environment)
	installCharacterPrimitives(result.environment)
	installListPrimitives(result.environment)
	return result
}

// LoadCode evaluates the code represented as a string to bootstrap the environment.
func (s *scheme) LoadCode(code string) Scheme {
	input := NewInputPortFromString(code)
	s.load(input)
	return s
}

// LoadFile loads code from a file to bootstrap the environment.
func (s *scheme) LoadFile(file string) Scheme {
	f, err := os.Open(file)
	if err != nil {
		Err("Could not open file: "+file, nil)
		return s
	}
	defer f.Close()
	input := NewInputPort(f)
	s.load(input)
	return s
}

// LoadFiles loads code from several files to bootstrap the environment.
func (s *scheme) LoadFiles(files []string) Scheme {
	for _, file := range files {
		s.LoadFile(file)
	}
	return s
}

// ReadEvalPrintLoop is a convenient option for exploring the gscheme environment outside of its use as an embedded
// scripting language.
func (s *scheme) ReadEvalPrintLoop() {
	input := NewInputPort(bufio.NewReader(os.Stdin))
	for {
		fmt.Print("> ")
		x := input.Read()
		if IsEOF(x) {
			return
		}
		result := s.Eval(x, s.environment)
		if !IsEOF(result) {
			fmt.Println(Stringify(result))
		}
	}
}

// load reads and evaluates expressions from an InputPort until EOF.
func (s *scheme) load(input *InputPort) {
	for {
		x := input.Read()
		if IsEOF(x) {
			return
		}
		s.Eval(x, s.environment)
	}
}

func (s *scheme) Environment() Environment {
	return s.environment
}

// Eval will evaluate a single Scheme expression with respect to a given environment and return the result.
func (s *scheme) Eval(x interface{}, environment Environment) (result interface{}) {
	// The purpose of the while loop is to allow tail recursion.
	// The idea is that in a tail recursive position, we do "x = ..."
	// and loop, rather than doing "return eval(...)".

	// If there was a scheme error raise'd by this eval, return it here.
	defer func() {
		recovered := recover()
		if recovered != nil {
			err, ok := recovered.(Error)
			if ok {
				result = err
				return
			}
			panic(err)
		}
	}()
	// Loop so we can implement tail recursion for special forms that require it.
	for {
		switch value := x.(type) {
		case Symbol:
			variable, ok := environment.Lookup(value)
			if !ok {
				return Err("Unbound variable: ", List(variable))
			}
			return variable
		case Pair:
			return s.EvalCombination(s.Eval(First(value), environment), RestPair(value), environment)
		default:
			return x
		}
	}
}

// EvalList evaluates each expression in turn and returns the last one.
func (s *scheme) EvalList(list Pair, environment Environment) Pair {
	if list == nil {
		return nil
	} else {
		return NewPair(s.Eval(First(list), environment), s.EvalList(RestPair(list), environment))
	}
}

// EvalCombination evaluates the first item in the list and applies the remaining arguments to it.
func (s *scheme) EvalCombination(first interface{}, rest Pair, environment Environment) interface{} {
	p, ok := first.(Applyer)
	if !ok {
		return Err("Bad Procedure: ", List(first))
	}
	return p.Apply(s, rest, environment)
}

func (s *scheme) EvalGlobal(x interface{}) interface{} {
	return s.Eval(x, s.environment)
}
