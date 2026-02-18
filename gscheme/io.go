package gscheme

import (
	"os"
	"strings"
)

// installIOPrimitives adds I/O primitives to a given environment.
// The interpreter is captured in closures to provide access to current ports.
func installIOPrimitives(environment Environment, interpreter Scheme) {
	// Reading primitives (optional input-port argument)
	environment.DefineName(NewHigherOrderPrimitive("read", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return port.Read()
	}))
	environment.DefineName(NewHigherOrderPrimitive("read-char", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return port.ReadChar()
	}))
	environment.DefineName(NewHigherOrderPrimitive("peek-char", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return port.PeekChar()
	}))
	environment.DefineName(NewHigherOrderPrimitive("char-ready?", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		// For buffered and string ports, characters are always ready.
		return true
	}))
	environment.DefineName(NewHigherOrderPrimitive("read-line", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return readLine(port)
	}))

	// Writing primitives (optional output-port argument)
	environment.DefineName(NewHigherOrderPrimitive("write", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		obj := First(args)
		port := resolveOutputPort(RestPair(args), s)
		port.WriteString(Stringify(obj))
		port.Flush()
		return nil
	}))
	environment.DefineName(NewHigherOrderPrimitive("display", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		obj := First(args)
		port := resolveOutputPort(RestPair(args), s)
		port.WriteString(Display(obj))
		port.Flush()
		return nil
	}))
	environment.DefineName(NewHigherOrderPrimitive("newline", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveOutputPort(args, s)
		port.Newline()
		port.Flush()
		return nil
	}))
	environment.DefineName(NewHigherOrderPrimitive("write-char", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		ch := characterConstraint(First(args))
		port := resolveOutputPort(RestPair(args), s)
		port.WriteRune(ch)
		port.Flush()
		return nil
	}))

	// Port constructors
	environment.DefineName(NewPrimitive("open-input-string", 1, 1, func(args Pair) interface{} {
		s := stringConstraint(First(args))
		return NewInputPortFromString(s)
	}))
	environment.DefineName(NewPrimitive("open-input-file", 1, 1, func(args Pair) interface{} {
		filename := stringConstraint(First(args))
		f, err := os.Open(filename)
		if err != nil {
			FileErr("Could not open file: "+filename, nil)
		}
		return NewInputPort(f)
	}))
	environment.DefineName(NewPrimitive("open-output-string", 0, 0, func(args Pair) interface{} {
		return NewOutputPortFromBuilder(&strings.Builder{})
	}))
	environment.DefineName(NewPrimitive("open-output-file", 1, 1, func(args Pair) interface{} {
		filename := stringConstraint(First(args))
		f, err := os.Create(filename)
		if err != nil {
			FileErr("Could not create file: "+filename, nil)
		}
		return NewOutputPort(f)
	}))
	environment.DefineName(NewPrimitive("get-output-string", 1, 1, func(args Pair) interface{} {
		port := outputPortConstraint(First(args))
		s, ok := port.GetString()
		if !ok {
			Err("get-output-string: port is not a string port", args)
		}
		return s
	}))

	// Port operations
	environment.DefineName(NewPrimitive("close-input-port", 1, 1, func(args Pair) interface{} {
		port := inputPortConstraint(First(args))
		port.Close()
		return nil
	}))
	environment.DefineName(NewPrimitive("close-output-port", 1, 1, func(args Pair) interface{} {
		port := outputPortConstraint(First(args))
		port.Close()
		return nil
	}))
	environment.DefineName(NewPrimitive("close-port", 1, 1, func(args Pair) interface{} {
		x := First(args)
		if ip, ok := x.(*InputPort); ok {
			ip.Close()
			return nil
		}
		if op, ok := x.(*OutputPort); ok {
			op.Close()
			return nil
		}
		return Err("close-port: expected a port", args)
	}))
	environment.DefineName(NewHigherOrderPrimitive("current-input-port", 0, 0, func(s Scheme, args Pair, env Environment) interface{} {
		return s.CurrentInputPort()
	}))
	environment.DefineName(NewHigherOrderPrimitive("current-output-port", 0, 0, func(s Scheme, args Pair, env Environment) interface{} {
		return s.CurrentOutputPort()
	}))
	environment.DefineName(NewPrimitive("eof-object", 0, 0, func(args Pair) interface{} {
		return EOF
	}))
	environment.DefineName(NewHigherOrderPrimitive("flush-output-port", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveOutputPort(args, s)
		port.Flush()
		return nil
	}))
}

// resolveInputPort returns the input port from args, or the interpreter's current input port if none given.
func resolveInputPort(args Pair, interpreter Scheme) *InputPort {
	if args != nil {
		return inputPortConstraint(First(args))
	}
	return interpreter.CurrentInputPort()
}

// resolveOutputPort returns the output port from args, or the interpreter's current output port if none given.
func resolveOutputPort(args Pair, interpreter Scheme) *OutputPort {
	if args != nil {
		return outputPortConstraint(First(args))
	}
	return interpreter.CurrentOutputPort()
}

// readLine reads characters from the input port until a newline or EOF.
// Returns the string without the newline, or EOF if at end of input.
func readLine(port *InputPort) interface{} {
	var b strings.Builder
	for {
		ch := port.ReadChar()
		if IsEOF(ch) {
			if b.Len() == 0 {
				return EOF
			}
			return b.String()
		}
		r := ch.(rune)
		if r == '\n' {
			return b.String()
		}
		b.WriteRune(r)
	}
}
