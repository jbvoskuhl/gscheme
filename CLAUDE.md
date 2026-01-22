# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GScheme is a dual-language Scheme interpreter implementation:
- **gscheme** (Go): A lightweight, embeddable Scheme interpreter for Go programs
- **jscheme** (Java): A full Scheme interpreter based on Peter Norvig's JScheme

## Build Commands

### Go (gscheme)
```bash
go test ./gscheme           # Run all tests
go test ./gscheme -run TestName   # Run a specific test
```

### Java (jscheme)
```bash
bazel build //jscheme:jscheme   # Build the interpreter
bazel run //jscheme:jscheme     # Run the REPL
```

## Architecture

### Go Implementation (`gscheme/`)

The interpreter follows a standard Scheme architecture with interface-based design:

- **Scheme interface** (`scheme.go`): Main entry point with `Eval()`, `EvalGlobal()`, and `EvalList()`
- **Environment** (`environment.go`): Lexical scoping via parent-chain symbol lookup
- **Applyer interface** (`applyer.go`): Base interface for anything callable
- **Procedure** (`procedure.go`): Lambda/closure implementation
- **Primitive** (`primitive.go`): Built-in functions installed at startup
- **SpecialForm** (`specialform.go`): Forms that receive unevaluated arguments (quote, if, define, etc.)

Data types:
- **Pair** (`pair.go`): Cons cells with `First()`/`Rest()` (car/cdr)
- **Symbol** (`symbol.go`): Identifiers (type alias for string)
- Numbers use `float64`, characters use `rune`, booleans use `bool`

Key patterns:
- Tail recursion optimization via while loop in `Eval()` rather than recursive calls
- Error handling via panic/recover with custom `Error` type
- Type constraints: `symbolConstraint()`, `characterConstraint()`, etc. for argument validation
- Variadic math operators use `reduce(binary, unary)` pattern

### Java Implementation (`jscheme/`)

Based on Peter Norvig's JScheme (http://www.norvig.com/jscheme.html):

- **Scheme.java**: Main interpreter with read-eval-write loop
- **Primitive.java**: 150+ built-in primitives with ID-based dispatch
- **SchemePrimitives.java**: Scheme-level macros as embedded code strings
- **primitives.scm**: Standard macros (quasiquote, let, let*, letrec, case, do, delay)

The Java implementation includes features not yet in Go: continuations, macros, vectors, Java interop.

## Creating New Primitives (Go)

Primitives are installed in their respective `install*Primitives()` functions. Example pattern:

```go
environment.Define(Symbol("name"), Primitive{
    func(s Scheme, args Pair, env Environment) interface{} {
        // Implementation
    },
})
```

Use constraint functions for type checking: `pairConstraint(args)`, `characterConstraint(args)`.
