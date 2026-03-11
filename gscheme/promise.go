package gscheme

// promiseState holds the mutable fields of a promise, kept behind a pointer
// so that delay-force can flatten nested promises by swapping the state pointer.
type promiseState struct {
	forced    bool
	value     interface{}
	thunk     Applyer
	iterative bool // true for delay-force promises
}

// Promise is a lazy value created by delay, delay-force, or make-promise.
type Promise struct {
	state *promiseState
}

func (p *Promise) String() string {
	return "#<promise>"
}

// installPromisePrimitives registers force, make-promise, and promise?.
// delay and delay-force are handled as special forms in eval.
func installPromisePrimitives(environment Environment) {
	environment.DefineName(NewHigherOrderPrimitive("force", 1, 1, primitiveForce))
	environment.DefineName(NewPrimitive("make-promise", 1, 1, primitiveMakePromise))
	environment.DefineName(NewPrimitive("promise?", 1, 1, primitiveIsPromise))
}

// primitiveForce forces a promise. For non-promise values returns the value
// unchanged (R7RS 6.4). For delay-force promises iteratively flattens nested
// promises to preserve proper tail behaviour.
func primitiveForce(s Scheme, args Pair, env Environment) interface{} {
	return forceValue(s, env, First(args))
}

func forceValue(s Scheme, env Environment, x interface{}) interface{} {
	for {
		p, ok := x.(*Promise)
		if !ok {
			return x
		}
		if p.state.forced {
			return p.state.value
		}

		result := p.state.thunk.Apply(s, nil, env)

		// Re-raise errors from the thunk body so guard/with-exception-handler
		// can catch them, consistent with the surrounding evaluator.
		if err, ok := result.(Error); ok {
			panic(err)
		}

		// Another call to force inside the thunk may have already forced us.
		if p.state.forced {
			return p.state.value
		}

		if p.state.iterative {
			if inner, ok2 := result.(*Promise); ok2 {
				// Flatten: adopt inner promise's state so this promise shares it.
				// The next iteration will force the inner state if needed.
				p.state = inner.state
				continue
			}
		}

		p.state.forced = true
		p.state.value = result
		p.state.thunk = nil
		return result
	}
}

// primitiveMakePromise wraps a value in an already-forced promise.
// If the argument is already a promise, returns it unchanged (R7RS 6.4).
func primitiveMakePromise(args Pair) interface{} {
	x := First(args)
	if p, ok := x.(*Promise); ok {
		return p
	}
	return &Promise{state: &promiseState{forced: true, value: x}}
}

// primitiveIsPromise returns #t if the argument is a promise.
func primitiveIsPromise(args Pair) interface{} {
	_, ok := First(args).(*Promise)
	return ok
}
