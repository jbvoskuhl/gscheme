package gscheme

import (
	"bytes"
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
		return true
	}))
	environment.DefineName(NewHigherOrderPrimitive("read-line", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return readLine(port)
	}))

	// Byte input (optional input-port argument)
	environment.DefineName(NewHigherOrderPrimitive("read-u8", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return port.ReadU8()
	}))
	environment.DefineName(NewHigherOrderPrimitive("peek-u8", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveInputPort(args, s)
		return port.PeekU8()
	}))
	environment.DefineName(NewHigherOrderPrimitive("u8-ready?", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		return true
	}))

	// String input
	environment.DefineName(NewHigherOrderPrimitive("read-string", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		k := int(uint64Constraint(First(args)))
		port := resolveInputPort(RestPair(args), s)
		return port.ReadKChars(k)
	}))

	// Bytevector input
	environment.DefineName(NewHigherOrderPrimitive("read-bytevector", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		k := int(uint64Constraint(First(args)))
		port := resolveInputPort(RestPair(args), s)
		return port.ReadKBytes(k)
	}))
	environment.DefineName(NewHigherOrderPrimitive("read-bytevector!", 1, 4, func(s Scheme, args Pair, env Environment) interface{} {
		bv := bytevectorConstraint(First(args))
		rest := RestPair(args)
		var port *InputPort
		start := 0
		end := len(bv)
		if rest != nil {
			port = inputPortConstraint(First(rest))
			rest = RestPair(rest)
			if rest != nil {
				start = int(uint64Constraint(First(rest)))
				rest = RestPair(rest)
				if rest != nil {
					end = int(uint64Constraint(First(rest)))
				}
			}
		} else {
			port = s.CurrentInputPort()
		}
		n := 0
		for i := start; i < end; i++ {
			result := port.ReadU8()
			if IsEOF(result) {
				break
			}
			bv[i] = uint8(result.(int64))
			n++
		}
		if n == 0 {
			return EOF
		}
		return int64(n)
	}))

	// Writing primitives (optional output-port argument)
	environment.DefineName(NewHigherOrderPrimitive("write", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		obj := First(args)
		port := resolveOutputPort(RestPair(args), s)
		port.WriteString(Stringify(obj))
		port.Flush()
		return nil
	}))
	environment.DefineName(NewHigherOrderPrimitive("write-shared", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		obj := First(args)
		port := resolveOutputPort(RestPair(args), s)
		port.WriteString(Stringify(obj))
		port.Flush()
		return nil
	}))
	environment.DefineName(NewHigherOrderPrimitive("write-simple", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
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

	// Byte output
	environment.DefineName(NewHigherOrderPrimitive("write-u8", 1, 2, func(s Scheme, args Pair, env Environment) interface{} {
		b := byteConstraint(First(args))
		port := resolveOutputPort(RestPair(args), s)
		port.WriteByte(b)
		port.Flush()
		return nil
	}))

	// String output: (write-string string) or (write-string string port) or (write-string string port start) or (write-string string port start end)
	environment.DefineName(NewHigherOrderPrimitive("write-string", 1, 4, func(s Scheme, args Pair, env Environment) interface{} {
		str := stringConstraint(First(args))
		rest := RestPair(args)
		var port *OutputPort
		if rest != nil {
			port = outputPortConstraint(First(rest))
			rest = RestPair(rest)
		} else {
			port = s.CurrentOutputPort()
		}
		start := 0
		end := len(str)
		if rest != nil {
			start = int(uint64Constraint(First(rest)))
			rest = RestPair(rest)
			if rest != nil {
				end = int(uint64Constraint(First(rest)))
			}
		}
		port.WriteString(str[start:end])
		port.Flush()
		return nil
	}))

	// Bytevector output: (write-bytevector bv) or (write-bytevector bv port) or (write-bytevector bv port start) or (write-bytevector bv port start end)
	environment.DefineName(NewHigherOrderPrimitive("write-bytevector", 1, 4, func(s Scheme, args Pair, env Environment) interface{} {
		bv := bytevectorConstraint(First(args))
		rest := RestPair(args)
		var port *OutputPort
		if rest != nil {
			port = outputPortConstraint(First(rest))
			rest = RestPair(rest)
		} else {
			port = s.CurrentOutputPort()
		}
		start := 0
		end := len(bv)
		if rest != nil {
			start = int(uint64Constraint(First(rest)))
			rest = RestPair(rest)
			if rest != nil {
				end = int(uint64Constraint(First(rest)))
			}
		}
		for i := start; i < end; i++ {
			port.WriteByte(bv[i])
		}
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
	environment.DefineName(NewPrimitive("open-binary-input-file", 1, 1, func(args Pair) interface{} {
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
	environment.DefineName(NewPrimitive("open-binary-output-file", 1, 1, func(args Pair) interface{} {
		filename := stringConstraint(First(args))
		f, err := os.Create(filename)
		if err != nil {
			FileErr("Could not create file: "+filename, nil)
		}
		return NewOutputPort(f)
	}))
	environment.DefineName(NewPrimitive("open-input-bytevector", 1, 1, func(args Pair) interface{} {
		bv := bytevectorConstraint(First(args))
		return NewInputPort(bytes.NewReader(bv))
	}))
	environment.DefineName(NewPrimitive("open-output-bytevector", 0, 0, func(args Pair) interface{} {
		return NewOutputPortFromBuffer(&bytes.Buffer{})
	}))
	environment.DefineName(NewPrimitive("get-output-string", 1, 1, func(args Pair) interface{} {
		port := outputPortConstraint(First(args))
		s, ok := port.GetString()
		if !ok {
			Err("get-output-string: port is not a string port", args)
		}
		return s
	}))
	environment.DefineName(NewPrimitive("get-output-bytevector", 1, 1, func(args Pair) interface{} {
		port := outputPortConstraint(First(args))
		bv, ok := port.GetBytes()
		if !ok {
			Err("get-output-bytevector: port is not a bytevector port", args)
		}
		return bv
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
	environment.DefineName(NewHigherOrderPrimitive("current-error-port", 0, 0, func(s Scheme, args Pair, env Environment) interface{} {
		return s.CurrentErrorPort()
	}))
	environment.DefineName(NewPrimitive("eof-object", 0, 0, func(args Pair) interface{} {
		return EOF
	}))
	environment.DefineName(NewHigherOrderPrimitive("flush-output-port", 0, 1, func(s Scheme, args Pair, env Environment) interface{} {
		port := resolveOutputPort(args, s)
		port.Flush()
		return nil
	}))

	// File operations
	environment.DefineName(NewPrimitive("file-exists?", 1, 1, func(args Pair) interface{} {
		filename := stringConstraint(First(args))
		_, err := os.Stat(filename)
		return err == nil
	}))
	environment.DefineName(NewPrimitive("delete-file", 1, 1, func(args Pair) interface{} {
		filename := stringConstraint(First(args))
		err := os.Remove(filename)
		if err != nil {
			FileErr("Could not delete file: "+filename, nil)
		}
		return nil
	}))

	// call-with-input-file: open file, call proc with port, close on return
	environment.DefineName(NewHigherOrderPrimitive("call-with-input-file", 2, 2, func(s Scheme, args Pair, env Environment) interface{} {
		filename := stringConstraint(First(args))
		proc, ok := Second(args).(Applyer)
		if !ok {
			return Err("call-with-input-file: second argument must be a procedure", List(Second(args)))
		}
		f, err := os.Open(filename)
		if err != nil {
			FileErr("Could not open file: "+filename, nil)
		}
		port := NewInputPort(f)
		defer port.Close()
		quotedArg := List(List(Symbol("quote"), port))
		return proc.Apply(s, quotedArg, env)
	}))

	// call-with-output-file: open file, call proc with port, close on return
	environment.DefineName(NewHigherOrderPrimitive("call-with-output-file", 2, 2, func(s Scheme, args Pair, env Environment) interface{} {
		filename := stringConstraint(First(args))
		proc, ok := Second(args).(Applyer)
		if !ok {
			return Err("call-with-output-file: second argument must be a procedure", List(Second(args)))
		}
		f, err := os.Create(filename)
		if err != nil {
			FileErr("Could not create file: "+filename, nil)
		}
		port := NewOutputPort(f)
		defer port.Close()
		quotedArg := List(List(Symbol("quote"), port))
		return proc.Apply(s, quotedArg, env)
	}))

	// with-input-from-file: redirect current-input-port to file for duration of thunk
	environment.DefineName(NewHigherOrderPrimitive("with-input-from-file", 2, 2, func(s Scheme, args Pair, env Environment) interface{} {
		filename := stringConstraint(First(args))
		thunk, ok := Second(args).(Applyer)
		if !ok {
			return Err("with-input-from-file: second argument must be a procedure", List(Second(args)))
		}
		f, err := os.Open(filename)
		if err != nil {
			FileErr("Could not open file: "+filename, nil)
		}
		port := NewInputPort(f)
		si := s.(*scheme)
		oldPort := si.inputPort
		si.setCurrentInputPort(port)
		defer func() {
			si.setCurrentInputPort(oldPort)
			port.Close()
		}()
		return thunk.Apply(s, nil, env)
	}))

	// with-output-to-file: redirect current-output-port to file for duration of thunk
	environment.DefineName(NewHigherOrderPrimitive("with-output-to-file", 2, 2, func(s Scheme, args Pair, env Environment) interface{} {
		filename := stringConstraint(First(args))
		thunk, ok := Second(args).(Applyer)
		if !ok {
			return Err("with-output-to-file: second argument must be a procedure", List(Second(args)))
		}
		f, err := os.Create(filename)
		if err != nil {
			FileErr("Could not create file: "+filename, nil)
		}
		port := NewOutputPort(f)
		si := s.(*scheme)
		oldPort := si.outputPort
		si.setCurrentOutputPort(port)
		defer func() {
			si.setCurrentOutputPort(oldPort)
			port.Close()
		}()
		return thunk.Apply(s, nil, env)
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
