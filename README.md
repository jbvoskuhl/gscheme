# GScheme

A lightweight, embeddable Scheme interpreter for Go programs. GScheme aims for broad coverage of the [R7RS-small](https://small.r7rs.org/) standard and is designed to be easy to embed in Go applications as a scripting or configuration language.

## Using GScheme

```go
import "gscheme"

s := gscheme.New()

// Evaluate a string of Scheme code
result := s.EvalGlobal(s.ReadString("(+ 1 2)"))

// Load and run a file
s.LoadFile("startup.scm")

// Extend with custom Go primitives
s.Environment().Define(gscheme.Symbol("my-func"), gscheme.Primitive{
    func(s gscheme.Scheme, args gscheme.Pair, env gscheme.Environment) interface{} {
        // ...
    },
})

// Start an interactive REPL (with readline support when stdin is a terminal)
s.ReadEvalPrintLoop()
```

Multiple independent interpreter instances can be created; they share no mutable state.

## Building and Testing

```bash
go test ./gscheme           # Run all tests
go test ./gscheme -run TestName   # Run a specific test
```

## Implementation Approach

### Evaluation

The core evaluator (`eval()` in `scheme.go`) is a straightforward recursive-descent interpreter operating on Scheme's native list structure (cons cells). Rather than compiling to bytecode or an AST, expressions are evaluated directly from the parsed representation.

**Tail call optimization** is implemented as a `for` loop in `eval()`. Instead of recursively calling `eval` in tail position, the loop reassigns `x` (the expression to evaluate) and `environment` and continues. Special forms — `if`, `begin`, `cond`, `guard`, `lambda`, `define`, `set!`, `quote`, `macro` — are handled inline in this loop to preserve tail position. User-defined closures are also inlined: rather than calling `Apply`, the loop binds arguments and continues with the closure body. Only primitives and other non-closure applyers make a real function call.

**Error handling** uses Go's panic/recover mechanism. Scheme errors raise a panic with a typed `Error` value; `Eval()` (the public entry point) installs a `recover()` boundary that catches these and returns them as values. Internal recursive calls go through `eval()` directly (no boundary) for performance.

### Number Tower

GScheme implements the full R7RS numeric tower:

- **int64** — the common case; all arithmetic checks for int64 arguments first and uses native Go integer operations, which is roughly 10× faster than the general path
- **\*big.Int** — arbitrary-precision integers, promoted from int64 on overflow
- **\*big.Rat** — exact rational numbers (`1/3`, etc.), automatically simplified back to integers when the denominator is 1
- **float64** — inexact real numbers
- **complex128** — complex numbers with rectangular and polar support

Exact arithmetic stays exact: `(+ 1/3 1/6)` returns `1/2`, not `0.5`.

### Data Representation

Scheme values map directly to Go types:

| Scheme type | Go type |
|---|---|
| boolean | `bool` |
| character | `rune` |
| string | `string` |
| symbol | `type Symbol string` |
| pair | `Pair` interface (cons cell) |
| vector | `[]interface{}` |
| bytevector | `[]byte` |
| closure | `Closure` interface |
| macro | `Macro` interface |
| port | `*InputPort` / `*OutputPort` |
| empty list | `nil` |

The empty list is represented as Go `nil`, which means many list operations reduce to nil checks.

### Macros

User-defined macros use a simple `macro` special form that works like `lambda` but receives its arguments unevaluated and returns an expression to be evaluated in the caller's environment. This is not hygienic — it is more like traditional Lisp `fexpr`-style macros, except the expansion is re-evaluated rather than directly called.

Several standard macros (`let`, `let*`, `letrec`, `letrec*`, `and`, `or`, `quasiquote`, `when`, `unless`, `case`, `do`, `named let`) are implemented in Scheme itself and bootstrapped at startup.

### Continuations

`call/cc` (`call-with-current-continuation`) is implemented as **escape-only continuations** using panic/recover. Invoking a captured continuation unwinds the stack to the capture point by raising a typed panic that is caught by the surrounding `Eval` boundary. This is sufficient for early exit, coroutine-like patterns, and most practical uses of `call/cc`, but it does not support re-entrant or upward continuations.

## R7RS Coverage

GScheme covers the bulk of R7RS-small:

- **6.1** Equivalence: `eq?`, `eqv?`, `equal?`
- **6.2** Numbers: full numeric tower, all required procedures
- **6.3** Booleans: `boolean?`, `not`, `boolean=?`
- **6.4** Pairs and lists: all standard procedures
- **6.5** Symbols
- **6.6** Characters: case-sensitive and case-insensitive comparisons, Unicode properties
- **6.7** Strings: all standard procedures including `string-map`, `string-for-each`
- **6.8** Vectors: all standard procedures
- **6.9** Bytevectors: all standard procedures, UTF-8 conversion
- **6.10** Control: `apply`, `map`, `for-each`, `call/cc`, `values`/`call-with-values` (partial)
- **6.11** Exceptions: `guard`, `with-exception-handler`, `raise`, `raise-continuable`, `error`
- **6.12** Environments: `interaction-environment`, `eval`
- **6.13** Ports: text and binary ports, file and string ports, standard port access
- **6.14** System: `load`, `command-line`, `exit`, `emergency-exit`, time procedures, environment variables

## Areas In Progress

### Full Continuations

`call/cc` currently captures escape-only continuations. Full, re-entrant continuations (where a continuation captured in one dynamic context can be invoked later from a different one) are not yet implemented. This means patterns like coroutines, generators implemented via continuations, and "time travel" re-invocation don't work.

### `dynamic-wind`

`dynamic-wind` is not yet implemented. Its semantics depend on full continuations — the before/after thunks must re-execute when a continuation crosses their boundary. The stub `emergency-exit` notes this: it exits without running any `dynamic-wind` cleanup.

### Hygienic Macros (`syntax-rules`)

The `macro` special form is not hygienic. R7RS specifies `syntax-rules` for pattern-based hygienic macro definitions. This is not yet implemented; `macro` is the only user-extensible macro mechanism available.

### Multiple Values

`values` and `call-with-values` are defined but not fully integrated. Passing multiple values across the full range of contexts described in R7RS is incomplete.

### Module / Library System

There is no `define-library` or module system. Everything lives in a single flat environment. For embedding use cases this is often fine, but it means there is no namespace isolation between separately loaded files.

### Lazy Evaluation

`delay`, `force`, and `make-promise` (R7RS 6.10) are not yet implemented.

### `syntax-error`

`syntax-error` is currently aliased to `error` rather than being a distinct compile-time condition.
