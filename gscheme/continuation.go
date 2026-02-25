package gscheme

// continuationEscape is the panic sentinel used by escape continuations.
// It does NOT implement the Error interface, so Eval's defer/recover
// will re-panic it, allowing it to propagate up to call/cc's own recover.
type continuationEscape struct {
	value interface{}
}

// continuation is a callable escape continuation created by call/cc.
type continuation struct {
	procedure
	sentinel *continuationEscape
}

func (c *continuation) String() string {
	return "#<continuation>"
}

// Apply invokes the escape continuation: stores the argument in the sentinel
// and panics to unwind the stack back to the call/cc frame.
func (c *continuation) Apply(interpreter Scheme, args Pair, environment Environment) interface{} {
	evaled := interpreter.EvalList(args, environment)
	c.sentinel.value = First(evaled)
	panic(c.sentinel)
}

// primitiveCallCC implements call-with-current-continuation (escape only).
func primitiveCallCC(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("call/cc: argument must be a procedure", List(First(args)))
	}

	sentinel := &continuationEscape{}
	k := &continuation{
		procedure: procedure{name: "escape continuation"},
		sentinel:  sentinel,
	}

	// Call proc with the continuation, catching escape panics.
	var result interface{}
	func() {
		defer func() {
			if r := recover(); r != nil {
				if r == sentinel {
					result = sentinel.value
					return
				}
				panic(r)
			}
		}()
		quotedK := List(List(Symbol("quote"), k))
		result = proc.Apply(interpreter, quotedK, environment)
	}()
	return result
}
