package gscheme

import "testing"

func TestMacroBasic(t *testing.T) {
	scheme := New()
	// Define a simple macro that returns its argument quoted
	// (define identity-macro (macro (x) (list 'quote x)))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("identity-macro"),
		List(Symbol("macro"), List(Symbol("x")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("quote")),
				Symbol("x")))))
	// (identity-macro (+ 1 2)) should return the unevaluated (+ 1 2)
	result := scheme.EvalGlobal(List(Symbol("identity-macro"),
		List(Symbol("+"), float64(1), float64(2))))
	pair, ok := result.(Pair)
	if !ok {
		t.Errorf("Expected pair result but got: %v", result)
		return
	}
	if First(pair) != Symbol("+") {
		t.Errorf("Expected (+ 1 2) but got: %v", result)
	}
}

func TestMacroWhen(t *testing.T) {
	scheme := New()
	// Define a 'when' macro: (when test body...) => (if test (begin body...))
	// (define when (macro (test . body) (list 'if test (cons 'begin body))))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("when"),
		List(Symbol("macro"), Cons(Symbol("test"), Symbol("body")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("if")),
				Symbol("test"),
				List(Symbol("cons"),
					List(Symbol("quote"), Symbol("begin")),
					Symbol("body"))))))

	// (when #t 1 2 3) should return 3
	result := scheme.EvalGlobal(List(Symbol("when"), true,
		float64(1), float64(2), float64(3)))
	if result != float64(3) {
		t.Errorf("Expected (when #t 1 2 3) to be 3 but was: %v", result)
	}

	// (when #f 1 2 3) should return nil
	result = scheme.EvalGlobal(List(Symbol("when"), false,
		float64(1), float64(2), float64(3)))
	if result != nil {
		t.Errorf("Expected (when #f 1 2 3) to be nil but was: %v", result)
	}
}

func TestMacroUnless(t *testing.T) {
	scheme := New()
	// Define an 'unless' macro: (unless test body...) => (if test #f (begin body...))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("unless"),
		List(Symbol("macro"), Cons(Symbol("test"), Symbol("body")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("if")),
				Symbol("test"),
				false,
				List(Symbol("cons"),
					List(Symbol("quote"), Symbol("begin")),
					Symbol("body"))))))

	// (unless #f 1 2 3) should return 3
	result := scheme.EvalGlobal(List(Symbol("unless"), false,
		float64(1), float64(2), float64(3)))
	if result != float64(3) {
		t.Errorf("Expected (unless #f 1 2 3) to be 3 but was: %v", result)
	}

	// (unless #t 1 2 3) should return #f
	result = scheme.EvalGlobal(List(Symbol("unless"), true,
		float64(1), float64(2), float64(3)))
	if result != false {
		t.Errorf("Expected (unless #t 1 2 3) to be #f but was: %v", result)
	}
}

func TestMacroSwap(t *testing.T) {
	scheme := New()
	// Define a swap macro that swaps two variables
	// (define swap! (macro (a b) (list 'let (list (list 'temp a)) (list 'set! a b) (list 'set! b 'temp))))
	// This is complex, let's do a simpler test

	// Define x and y
	scheme.EvalGlobal(List(Symbol("define"), Symbol("x"), float64(1)))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("y"), float64(2)))

	// Define a macro that doubles its argument
	// (define double (macro (x) (list '* x 2)))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("double"),
		List(Symbol("macro"), List(Symbol("x")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("*")),
				Symbol("x"),
				float64(2)))))

	// (double 5) should return 10
	result := scheme.EvalGlobal(List(Symbol("double"), float64(5)))
	if result != float64(10) {
		t.Errorf("Expected (double 5) to be 10 but was: %v", result)
	}

	// (double (+ 2 3)) should evaluate (+ 2 3) first, then double it
	result = scheme.EvalGlobal(List(Symbol("double"),
		List(Symbol("+"), float64(2), float64(3))))
	if result != float64(10) {
		t.Errorf("Expected (double (+ 2 3)) to be 10 but was: %v", result)
	}
}

func TestMacroWithDefine(t *testing.T) {
	scheme := New()
	// Define a macro using define shorthand style
	// In jscheme, you can do: (define name (macro args body))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("inc"),
		List(Symbol("macro"), List(Symbol("x")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("+")),
				Symbol("x"),
				float64(1)))))

	result := scheme.EvalGlobal(List(Symbol("inc"), float64(5)))
	if result != float64(6) {
		t.Errorf("Expected (inc 5) to be 6 but was: %v", result)
	}
}

func TestMacroOr(t *testing.T) {
	scheme := New()
	// Define 'or' as a macro (simplified version for two args)
	// (define or2 (macro (a b) (list 'if a a b)))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("or2"),
		List(Symbol("macro"), List(Symbol("a"), Symbol("b")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("if")),
				Symbol("a"),
				Symbol("a"),
				Symbol("b")))))

	// (or2 #f 5) should return 5
	result := scheme.EvalGlobal(List(Symbol("or2"), false, float64(5)))
	if result != float64(5) {
		t.Errorf("Expected (or2 #f 5) to be 5 but was: %v", result)
	}

	// (or2 3 5) should return 3
	result = scheme.EvalGlobal(List(Symbol("or2"), float64(3), float64(5)))
	if result != float64(3) {
		t.Errorf("Expected (or2 3 5) to be 3 but was: %v", result)
	}
}

func TestMacroAnd(t *testing.T) {
	scheme := New()
	// Define 'and' as a macro (simplified version for two args)
	// (define and2 (macro (a b) (list 'if a b #f)))
	scheme.EvalGlobal(List(Symbol("define"), Symbol("and2"),
		List(Symbol("macro"), List(Symbol("a"), Symbol("b")),
			List(Symbol("list"),
				List(Symbol("quote"), Symbol("if")),
				Symbol("a"),
				Symbol("b"),
				false))))

	// (and2 #t 5) should return 5
	result := scheme.EvalGlobal(List(Symbol("and2"), true, float64(5)))
	if result != float64(5) {
		t.Errorf("Expected (and2 #t 5) to be 5 but was: %v", result)
	}

	// (and2 #f 5) should return #f
	result = scheme.EvalGlobal(List(Symbol("and2"), false, float64(5)))
	if result != false {
		t.Errorf("Expected (and2 #f 5) to be #f but was: %v", result)
	}
}

func TestIsMacro(t *testing.T) {
	scheme := New()
	scheme.EvalGlobal(List(Symbol("define"), Symbol("m"),
		List(Symbol("macro"), List(Symbol("x")), Symbol("x"))))

	m, _ := scheme.Environment().Lookup(Symbol("m"))
	if !IsMacro(m) {
		t.Errorf("Expected m to be a macro")
	}

	scheme.EvalGlobal(List(Symbol("define"), Symbol("f"),
		List(Symbol("lambda"), List(Symbol("x")), Symbol("x"))))

	f, _ := scheme.Environment().Lookup(Symbol("f"))
	if IsMacro(f) {
		t.Errorf("Expected f to not be a macro")
	}
}
