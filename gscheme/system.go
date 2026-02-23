package gscheme

import (
	"os"
	"strings"
	"time"
)

// installSystemPrimitives adds R7RS section 6.14 system interface primitives.
func installSystemPrimitives(environment Environment, interpreter Scheme) {
	// load: read and evaluate expressions from a file
	environment.DefineName(NewHigherOrderPrimitive("load", 1, 1,
		func(s Scheme, args Pair, env Environment) interface{} {
			filename := stringConstraint(First(args))
			s.LoadFile(filename)
			return nil
		}))

	// command-line: return command-line arguments as a list of strings
	environment.DefineName(NewPrimitive("command-line", 0, 0, func(args Pair) interface{} {
		var elements []interface{}
		for _, arg := range os.Args {
			elements = append(elements, arg)
		}
		return List(elements...)
	}))

	// exit: exit the process
	environment.DefineName(NewPrimitive("exit", 0, 1, func(args Pair) interface{} {
		if args == nil {
			os.Exit(0)
		}
		val := First(args)
		if val == true {
			os.Exit(0)
		}
		if val == false {
			os.Exit(1)
		}
		if code, ok := val.(int64); ok {
			os.Exit(int(code))
		}
		os.Exit(0)
		return nil
	}))

	// emergency-exit: same as exit (no dynamic-wind cleanup)
	environment.DefineName(NewPrimitive("emergency-exit", 0, 1, func(args Pair) interface{} {
		if args == nil {
			os.Exit(0)
		}
		val := First(args)
		if val == true {
			os.Exit(0)
		}
		if val == false {
			os.Exit(1)
		}
		if code, ok := val.(int64); ok {
			os.Exit(int(code))
		}
		os.Exit(0)
		return nil
	}))

	// get-environment-variable: look up an environment variable, return #f if not found
	environment.DefineName(NewPrimitive("get-environment-variable", 1, 1, func(args Pair) interface{} {
		name := stringConstraint(First(args))
		value, ok := os.LookupEnv(name)
		if !ok {
			return false
		}
		return value
	}))

	// get-environment-variables: return an alist of all environment variables
	environment.DefineName(NewPrimitive("get-environment-variables", 0, 0, func(args Pair) interface{} {
		envVars := os.Environ()
		var elements []interface{}
		for _, envVar := range envVars {
			parts := strings.SplitN(envVar, "=", 2)
			if len(parts) == 2 {
				elements = append(elements, Cons(parts[0], parts[1]))
			}
		}
		return List(elements...)
	}))

	// current-second: return current time as seconds since Unix epoch
	environment.DefineName(NewPrimitive("current-second", 0, 0, func(args Pair) interface{} {
		return float64(time.Now().Unix())
	}))

	// current-jiffy: return current time in nanoseconds
	environment.DefineName(NewPrimitive("current-jiffy", 0, 0, func(args Pair) interface{} {
		return time.Now().UnixNano()
	}))

	// jiffies-per-second: return the number of jiffies per second
	environment.DefineName(NewPrimitive("jiffies-per-second", 0, 0, func(args Pair) interface{} {
		return int64(1_000_000_000)
	}))

	// features: return a list of feature identifiers
	environment.DefineName(NewPrimitive("features", 0, 0, func(args Pair) interface{} {
		return List(Symbol("r7rs"), Symbol("gscheme"))
	}))
}
