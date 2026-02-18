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
	installPrimitives(result.environment)
	installBooleanPrimitives(result.environment)
	installCharacterPrimitives(result.environment)
	installListPrimitives(result.environment)
	installPredicatePrimitives(result.environment)
	installComplexPrimitives(result.environment)
	installHigherOrderPrimitives(result.environment)
	installStringPrimitives(result.environment)
	installVectorPrimitives(result.environment)
	result.loadPrimitivesScheme()
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
		FileErr("Could not open file: "+file, nil)
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
// Special forms are handled inline in the for loop to enable tail call optimization:
// in tail positions we reassign x and continue the loop rather than recursively calling Eval.
func (s *scheme) Eval(x interface{}, environment Environment) (result interface{}) {
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
	// Loop so we can implement tail call optimization. In tail positions we
	// reassign x (and possibly environment) and continue rather than returning eval(...).
	for {
		switch value := x.(type) {
		case Symbol:
			variable, ok := environment.Lookup(value)
			if !ok {
				return Err("Unbound variable: ", List(variable))
			}
			return variable
		case Pair:
			fn := First(value)
			args := RestPair(value)

			// Check for special forms by symbol name
			if sym, ok := fn.(Symbol); ok {
				switch sym {
				case "quote":
					return First(args)

				case "begin":
					// Evaluate all but last expression eagerly; tail-call the last
					for args != nil && Rest(args) != nil {
						s.Eval(First(args), environment)
						args = RestPair(args)
					}
					if args == nil {
						return nil
					}
					x = First(args)
					continue

				case "if":
					test := s.Eval(First(args), environment)
					if Truth(test) {
						x = Second(args)
					} else {
						x = Third(args)
					}
					continue

				case "cond":
					x = s.reduceCond(args, environment)
					continue

				case "guard":
					// (guard (variable cond-clause ...) body ...)
					guardSpec, ok := First(args).(Pair)
					if !ok {
						return Err("guard: bad syntax", List(x))
					}
					variable, ok := First(guardSpec).(Symbol)
					if !ok {
						return Err("guard: expected variable name", List(First(guardSpec)))
					}
					clauses := RestPair(guardSpec)
					body := Rest(args)

					// Evaluate body in a nested Eval (which has its own defer/recover).
					// If body raises an error, the nested Eval catches it and returns the Error.
					bodyResult := s.Eval(Cons(Symbol("begin"), body), environment)

					// If no error was raised, return the body result
					if _, isErr := bodyResult.(Error); !isErr {
						return bodyResult
					}

					// Unwrap raisedError to get the actual raised value
					conditionValue := interface{}(bodyResult)
					if raised, ok := bodyResult.(*raisedError); ok {
						conditionValue = raised.value
					}

					// Bind condition to variable and evaluate cond clauses
					guardEnv := NewChildEnvironment(environment)
					guardEnv.Define(variable, conditionValue)
					condResult := s.reduceCond(clauses, guardEnv)
					if condResult == false {
						// No clause matched — re-raise the original error
						panic(bodyResult)
					}
					environment = guardEnv
					x = condResult
					continue

				case "define":
					first := First(args)
					if firstPair, ok := first.(Pair); ok {
						// Function shorthand: (define (name params...) body...)
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
					val := s.Eval(Second(args), environment)
					if proc, ok := val.(Procedure); ok && proc.Name() == "anonymous procedure" {
						proc.SetName(name)
					}
					return environment.Define(name, val)

				case "set!":
					name, ok := First(args).(Symbol)
					if !ok {
						return Err("set!: expected symbol", List(First(args)))
					}
					val := s.Eval(Second(args), environment)
					if !environment.Set(name, val) {
						return Err("set!: unbound variable", List(name))
					}
					return val

				case "lambda":
					params := First(args)
					body := Rest(args)
					return NewClosure(params, body, environment)

				case "macro":
					params := First(args)
					body := Rest(args)
					return NewMacro(params, body, environment)
				}
			}

			// Not a special form — evaluate the operator
			fn = s.Eval(fn, environment)

			// Macro: expand and loop
			if m, ok := fn.(Macro); ok {
				x = m.Expand(s, args, environment)
				continue
			}

			// Closure: inline tail call — bind args and loop on body
			if c, ok := fn.(Closure); ok {
				evaledArgs := s.EvalList(args, environment)
				environment = NewChildEnvironmentWithBindings(c.Params(), evaledArgs, c.Env())
				x = c.Body()
				continue
			}

			// Primitive or other applyer: call and return
			p, ok := fn.(Applyer)
			if !ok {
				return Err("Bad Procedure: ", List(fn))
			}
			return p.Apply(s, args, environment)

		default:
			return x
		}
	}
}

// reduceCond processes cond clauses and returns an expression to be evaluated in the
// current environment, preserving tail position. When a clause matches:
//   - No body: returns (quote <test-result>) to yield the test value
//   - => proc: returns (proc (quote <test-result>))
//   - Normal body: returns (begin body...)
func (s *scheme) reduceCond(clauses Pair, environment Environment) interface{} {
	for clauses != nil {
		clause := First(clauses)
		clausePair, ok := clause.(Pair)
		if !ok {
			Err("cond: bad clause", List(clause))
		}

		test := First(clausePair)
		var result interface{}

		if test == Symbol("else") {
			result = true
		} else {
			result = s.Eval(test, environment)
		}

		if Truth(result) {
			rest := Rest(clausePair)
			if rest == nil {
				// No body — return (quote result) so the test value is yielded
				return List(Symbol("quote"), result)
			}
			if First(rest) == Symbol("=>") {
				// (test => proc) — return (proc (quote result))
				proc := Second(rest)
				return List(proc, List(Symbol("quote"), result))
			}
			// Normal body — return (begin body...) for tail evaluation
			return Cons(Symbol("begin"), rest)
		}

		clauses = RestPair(clauses)
	}
	// No clause matched — return a self-evaluating false
	return false
}

// EvalList evaluates each expression in turn and returns the last one.
func (s *scheme) EvalList(list Pair, environment Environment) Pair {
	if list == nil {
		return nil
	} else {
		return NewPair(s.Eval(First(list), environment), s.EvalList(RestPair(list), environment))
	}
}

func (s *scheme) EvalGlobal(x interface{}) interface{} {
	return s.Eval(x, s.environment)
}
