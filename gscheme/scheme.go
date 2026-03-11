// gscheme implements a simple lightweight Scheme interpreter suitable for embedding into Go programs.
package gscheme

import (
	"io"
	"os"
	"strings"
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
	CurrentInputPort() *InputPort
	CurrentOutputPort() *OutputPort
	CurrentErrorPort() *OutputPort
	// RegisterLibrary makes a library available for (import ...) forms.
	RegisterLibrary(lib *Library)
	// LookupLibrary retrieves a registered library by name.
	LookupLibrary(name LibraryName) (*Library, bool)
}

// scheme is the internal representation of the lightweight gscheme runtime.
type scheme struct {
	environment Environment
	inputPort   *InputPort
	outputPort  *OutputPort
	errorPort   *OutputPort
	registry    LibraryRegistry
}

// Create a new gscheme interpreter – they are completely independent and it is safe to create several of them
// as needed.  Extend the set of built-in primitives to allow your go code to interact with Scheme.
func New() Scheme {
	result := &scheme{
		environment: NewRootEnvironment(),
		inputPort:   NewInputPort(os.Stdin),
		outputPort:  NewOutputPort(os.Stdout),
		errorPort:   NewOutputPort(os.Stderr),
		registry:    make(LibraryRegistry),
	}
	installPrimitives(result.environment)
	installBooleanPrimitives(result.environment)
	installCharacterPrimitives(result.environment)
	installListPrimitives(result.environment)
	installPredicatePrimitives(result.environment)
	installComplexPrimitives(result.environment)
	installHigherOrderPrimitives(result.environment)
	installPromisePrimitives(result.environment)
	installValuesPrimitives(result.environment)
	installStringPrimitives(result.environment)
	installVectorPrimitives(result.environment)
	installIOPrimitives(result.environment, result)
	installSystemPrimitives(result.environment, result)
	result.loadPrimitivesScheme()
	result.registerBuiltinLibraries()
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
// scripting language. When stdin is a terminal, it provides readline-style line editing with history.
func (s *scheme) ReadEvalPrintLoop() {
	rl := NewReadline()
	if rl == nil {
		s.readEvalPrintLoopPipe()
		return
	}

	rl.Completer = MakeSymbolCompleter(s.environment)

	accumulated := ""
	for {
		text, err := rl.ReadLine("> ", "  ", accumulated)
		if err == io.EOF {
			return
		}
		if err == ErrInterrupt {
			accumulated = ""
			continue
		}
		if err != nil {
			return
		}

		if strings.TrimSpace(text) == "" {
			accumulated = ""
			continue
		}

		input := NewInputPortFromString(text)
		x, incomplete := s.tryRead(input)
		if incomplete {
			accumulated = text
			continue
		}

		accumulated = ""

		if IsEOF(x) {
			continue
		}

		if _, isErr := x.(Error); isErr {
			s.outputPort.WriteString(Stringify(x))
			s.outputPort.Newline()
			s.outputPort.Flush()
			continue
		}

		result := s.Eval(x, s.environment)
		if !IsEOF(result) {
			s.outputPort.WriteString(Stringify(result))
			s.outputPort.Newline()
			s.outputPort.Flush()
		}
	}
}

// readEvalPrintLoopPipe is the original REPL for pipe/file input.
func (s *scheme) readEvalPrintLoopPipe() {
	for {
		s.outputPort.WriteString("> ")
		s.outputPort.Flush()
		x := s.inputPort.Read()
		if IsEOF(x) {
			return
		}
		result := s.Eval(x, s.environment)
		if !IsEOF(result) {
			s.outputPort.WriteString(Stringify(result))
			s.outputPort.Newline()
			s.outputPort.Flush()
		}
	}
}

// tryRead attempts to read one Scheme expression from the input port.
// Returns (expression, false) on success, or (nil, true) if the input is
// incomplete and more lines are needed.
func (s *scheme) tryRead(input *InputPort) (result interface{}, incomplete bool) {
	defer func() {
		if r := recover(); r != nil {
			if e, ok := r.(Error); ok && e.IsReadError() {
				if e.GetMessage() == "EOF during read." {
					result = nil
					incomplete = true
					return
				}
				result = e
				incomplete = false
				return
			}
			panic(r)
		}
	}()
	x := input.Read()
	return x, false
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

// CurrentInputPort returns the interpreter's current input port.
func (s *scheme) CurrentInputPort() *InputPort {
	return s.inputPort
}

// CurrentOutputPort returns the interpreter's current output port.
func (s *scheme) CurrentOutputPort() *OutputPort {
	return s.outputPort
}

// CurrentErrorPort returns the interpreter's current error port.
func (s *scheme) CurrentErrorPort() *OutputPort {
	return s.errorPort
}

// setCurrentInputPort temporarily sets the current input port (used by with-input-from-file).
func (s *scheme) setCurrentInputPort(p *InputPort) {
	s.inputPort = p
}

// setCurrentOutputPort temporarily sets the current output port (used by with-output-to-file).
func (s *scheme) setCurrentOutputPort(p *OutputPort) {
	s.outputPort = p
}

// Eval will evaluate a single Scheme expression with respect to a given environment and return the result.
// It establishes an error boundary: any Scheme error (panic with Error value) is caught and returned.
// Internal recursive calls use eval (no error boundary) for performance.
func (s *scheme) Eval(x interface{}, environment Environment) (result interface{}) {
	defer func() {
		recovered := recover()
		if recovered != nil {
			err, ok := recovered.(Error)
			if ok {
				result = err
				return
			}
			panic(recovered)
		}
	}()
	return s.eval(x, environment)
}

// eval is the core evaluator without an error boundary. Panics propagate to the nearest Eval.
// Special forms are handled inline in the for loop to enable tail call optimization:
// in tail positions we reassign x and continue the loop rather than recursively calling eval.
func (s *scheme) eval(x interface{}, environment Environment) interface{} {
	for {
		switch value := x.(type) {
		case Symbol:
			variable, ok := environment.Lookup(value)
			if !ok {
				return Err("Unbound variable: ", List(value))
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
						s.eval(First(args), environment)
						args = RestPair(args)
					}
					if args == nil {
						return nil
					}
					x = First(args)
					continue

				case "if":
					test := s.eval(First(args), environment)
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
					conditionValue := unwrapRaisedValue(bodyResult.(Error))

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
					val := s.eval(Second(args), environment)
					if proc, ok := val.(Procedure); ok && proc.Name() == "" {
						proc.SetName(name)
					}
					return environment.Define(name, val)

				case "define-values":
					// (define-values (var ...) expr)
					namesPair, ok := First(args).(Pair)
					if !ok {
						return Err("define-values: expected list of names", List(First(args)))
					}
					val := s.eval(Second(args), environment)
					var vals []interface{}
					if mv, ok := val.(MultipleValues); ok {
						vals = []interface{}(mv)
					} else {
						vals = []interface{}{val}
					}
					names := namesPair
					i := 0
					for names != nil {
						sym, ok := First(names).(Symbol)
						if !ok {
							return Err("define-values: expected symbol", List(First(names)))
						}
						if i >= len(vals) {
							return Err("define-values: too few values", List(namesPair))
						}
						environment.Define(sym, vals[i])
						names = RestPair(names)
						i++
					}
					if i < len(vals) {
						return Err("define-values: too many values", List(namesPair))
					}
					return nil

				case "set!":
					name, ok := First(args).(Symbol)
					if !ok {
						return Err("set!: expected symbol", List(First(args)))
					}
					val := s.eval(Second(args), environment)
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

				case "delay":
					thunk := NewClosure(nil, args, environment)
					return &Promise{state: &promiseState{thunk: thunk}}

				case "delay-force", "lazy":
					thunk := NewClosure(nil, args, environment)
					return &Promise{state: &promiseState{thunk: thunk, iterative: true}}

				case "define-library":
					// (define-library <name> <decl> ...)
					nameExpr := First(args)
					name, ok := parseLibraryName(nameExpr)
					if !ok {
						return Err("define-library: invalid library name", List(nameExpr))
					}
					lib := s.defineLibrary(name, RestPair(args))
					s.registry.Register(lib)
					return nil

				case "import":
					// (import <import-set> ...)
					for args != nil {
						s.importInto(First(args), environment)
						args = RestPair(args)
					}
					return nil
				}
			}

			// Not a special form — evaluate the operator
			fnExpr := fn
			fn = s.eval(fn, environment)

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
				return Err("Bad Procedure: ", List(fnExpr))
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
			result = s.eval(test, environment)
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

// EvalList evaluates each expression in a list and returns a new list of results.
func (s *scheme) EvalList(list Pair, environment Environment) Pair {
	if list == nil {
		return nil
	}
	head := NewPair(s.eval(First(list), environment), nil)
	tail := head
	for list = RestPair(list); list != nil; list = RestPair(list) {
		p := NewPair(s.eval(First(list), environment), nil)
		tail.SetRest(p)
		tail = p
	}
	return head
}

func (s *scheme) EvalGlobal(x interface{}) interface{} {
	return s.Eval(x, s.environment)
}

// RegisterLibrary makes a library available for (import ...) forms.
func (s *scheme) RegisterLibrary(lib *Library) {
	s.registry.Register(lib)
}

// LookupLibrary retrieves a registered library by name.
func (s *scheme) LookupLibrary(name LibraryName) (*Library, bool) {
	return s.registry.Lookup(name)
}

// defineLibrary processes (define-library <name> <decl> ...) declarations,
// builds a library environment, and returns the resulting Library.
func (s *scheme) defineLibrary(name LibraryName, decls Pair) *Library {
	libEnv := NewChildEnvironment(s.environment)
	exports := make(map[Symbol]Symbol)

	for decls != nil {
		decl := First(decls)
		declPair, ok := decl.(Pair)
		if !ok {
			Err("define-library: expected declaration", List(decl))
		}
		keyword, ok := First(declPair).(Symbol)
		if !ok {
			Err("define-library: declaration must start with a keyword", List(First(declPair)))
		}
		body := RestPair(declPair)

		switch keyword {
		case "export":
			for body != nil {
				spec := First(body)
				switch v := spec.(type) {
				case Symbol:
					exports[v] = v // export with same name
				case Pair:
					// (rename <internal> <external>)
					if First(v) == Symbol("rename") {
						internal := Second(v).(Symbol)
						external := Third(v).(Symbol)
						exports[external] = internal
					}
				}
				body = RestPair(body)
			}

		case "import":
			for body != nil {
				s.importInto(First(body), libEnv)
				body = RestPair(body)
			}

		case "begin":
			for body != nil {
				s.eval(First(body), libEnv)
				body = RestPair(body)
			}

		case "include":
			for body != nil {
				filename, ok := First(body).(string)
				if !ok {
					Err("define-library include: expected string filename", List(First(body)))
				}
				s.loadIntoEnv(filename, libEnv)
				body = RestPair(body)
			}

		default:
			Err("define-library: unknown declaration keyword", List(Symbol(keyword)))
		}

		decls = RestPair(decls)
	}

	return &Library{name: name, env: libEnv, exports: exports}
}

// importInto processes one import set and defines the resulting bindings in target.
// An import set is one of:
//
//	<library-name>                     import all exports
//	(only <set> <id> ...)              import only the named identifiers
//	(except <set> <id> ...)            import all except the named identifiers
//	(prefix <set> <prefix>)            import all with a prefix prepended
//	(rename <set> (<from> <to>) ...)   import with selective renaming
func (s *scheme) importInto(importSet interface{}, target Environment) {
	bindings := s.resolveImportSet(importSet)
	for name, val := range bindings {
		target.Define(name, val)
	}
}

// resolveImportSet resolves an import set to a flat name→value map.
func (s *scheme) resolveImportSet(importSet interface{}) map[Symbol]interface{} {
	p, ok := importSet.(Pair)
	if !ok {
		Err("import: expected import set", List(importSet))
		return nil
	}

	// Check whether the first element is a modifier keyword.
	if sym, ok := First(p).(Symbol); ok {
		rest := RestPair(p)
		switch sym {
		case "only":
			// (only <set> <id> ...)
			base := s.resolveImportSet(Second(p))
			result := make(map[Symbol]interface{})
			for ids := RestPair(rest); ids != nil; ids = RestPair(ids) {
				name := First(ids).(Symbol)
				if val, ok := base[name]; ok {
					result[name] = val
				}
			}
			return result

		case "except":
			// (except <set> <id> ...)
			base := s.resolveImportSet(Second(p))
			excluded := make(map[Symbol]bool)
			for ids := RestPair(rest); ids != nil; ids = RestPair(ids) {
				excluded[First(ids).(Symbol)] = true
			}
			result := make(map[Symbol]interface{})
			for name, val := range base {
				if !excluded[name] {
					result[name] = val
				}
			}
			return result

		case "prefix":
			// (prefix <set> <prefix-symbol>)
			base := s.resolveImportSet(Second(p))
			prefix := string(Third(p).(Symbol))
			result := make(map[Symbol]interface{})
			for name, val := range base {
				result[Symbol(prefix+string(name))] = val
			}
			return result

		case "rename":
			// (rename <set> (<from> <to>) ...)
			base := s.resolveImportSet(Second(p))
			renames := make(map[Symbol]Symbol)
			for pairs := RestPair(rest); pairs != nil; pairs = RestPair(pairs) {
				pair := First(pairs).(Pair)
				renames[First(pair).(Symbol)] = Second(pair).(Symbol)
			}
			result := make(map[Symbol]interface{})
			for name, val := range base {
				if newName, ok := renames[name]; ok {
					result[newName] = val
				} else {
					result[name] = val
				}
			}
			return result
		}
	}

	// Not a modifier — treat as a library name.
	name, ok := parseLibraryName(importSet)
	if !ok {
		Err("import: invalid library name", List(importSet))
		return nil
	}
	lib, ok := s.registry.Lookup(name)
	if !ok {
		Err("import: library not found: "+name.String(), nil)
		return nil
	}
	return lib.exportedBindings()
}

// loadIntoEnv reads and evaluates a file's expressions in the given environment.
func (s *scheme) loadIntoEnv(filename string, env Environment) {
	f, err := os.Open(filename)
	if err != nil {
		FileErr("Could not open file: "+filename, nil)
		return
	}
	defer f.Close()
	input := NewInputPort(f)
	for {
		x := input.Read()
		if IsEOF(x) {
			return
		}
		s.eval(x, env)
	}
}

// registerBuiltinLibraries pre-registers the standard R7RS library names.
// Each library is a view over the root environment restricted to the relevant names.
func (s *scheme) registerBuiltinLibraries() {
	// Helper: build a library from a fixed list of symbol names.
	filtered := func(name LibraryName, names ...Symbol) *Library {
		exports := make(map[Symbol]Symbol, len(names))
		for _, sym := range names {
			exports[sym] = sym
		}
		return &Library{name: name, env: s.environment, exports: exports}
	}

	// (scheme base) — core language: everything currently in the root environment.
	baseExports := make(map[Symbol]Symbol)
	for _, sym := range s.environment.Symbols() {
		baseExports[sym] = sym
	}
	s.registry.Register(&Library{
		name:    LibraryName{Symbol("scheme"), Symbol("base")},
		env:     s.environment,
		exports: baseExports,
	})

	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("lazy")},
		"delay", "delay-force", "force", "make-promise", "promise?", "lazy",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("eval")},
		"eval", "interaction-environment",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("load")},
		"load",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("read")},
		"read", "read-char", "read-line", "read-u8", "peek-char", "peek-u8",
		"char-ready?", "u8-ready?", "eof-object", "eof-object?",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("write")},
		"write", "display", "newline", "write-char", "write-string",
		"write-u8", "write-bytevector", "flush-output-port",
		"open-output-string", "get-output-string",
		"open-output-bytevector", "get-output-bytevector",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("file")},
		"open-input-file", "open-binary-input-file",
		"open-output-file", "open-binary-output-file",
		"with-input-from-file", "with-output-to-file",
		"call-with-input-file", "call-with-output-file",
		"file-exists?", "delete-file",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("process-context")},
		"command-line", "exit", "emergency-exit",
		"get-environment-variable", "get-environment-variables",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("time")},
		"current-second", "current-jiffy", "jiffies-per-second",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("char")},
		"char-alphabetic?", "char-numeric?", "char-whitespace?",
		"char-upper-case?", "char-lower-case?",
		"char-upcase", "char-downcase", "char-foldcase",
		"string-upcase", "string-downcase", "string-foldcase",
		"digit-value",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("complex")},
		"make-rectangular", "make-polar", "real-part", "imag-part",
		"magnitude", "angle",
	))
	s.registry.Register(filtered(
		LibraryName{Symbol("scheme"), Symbol("inexact")},
		"sin", "cos", "tan", "asin", "acos", "atan",
		"exp", "log", "sqrt", "square",
		"floor", "ceiling", "truncate", "round",
		"exact", "inexact", "finite?", "infinite?", "nan?",
	))
}
