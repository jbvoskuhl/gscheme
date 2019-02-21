package gscheme

import (
	"fmt"
	"strings"
)

// Pair has two fields, first and rest (sometimes called car and cdr).  The empty list is represented by nil.
type Pair interface {
	fmt.Stringer
	First() interface{}
	Rest() interface{}
	SetFirst(first interface{})
	SetRest(rest interface{})
}

type pair struct {
	first interface{}
	rest  interface{}
}

// NewPair creates a new cons cell.
func NewPair(first interface{}, rest interface{}) Pair {
	return &pair{first, rest}
}

// String for a list defers to the centralized string formatting utility called Stringify for list elements.
func (p *pair) String() string {
	builder := strings.Builder{}
	builder.WriteString("(")
	var current interface{} = p
	ok := true
	for ok {
		builder.WriteString(Stringify(First(current)))
		current = Rest(current)
		_, ok = current.(Pair)
		if ok {
			builder.WriteString(" ")
		}
	}
	if current != nil {
		builder.WriteString(" . ")
		builder.WriteString(Stringify(current))
	}
	builder.WriteString(")")
	return builder.String()
}

// First gives back the first field of a pair.
func (p *pair) First() interface{} {
	return p.first
}

// Rest gives back the rest field of a pair.
func (p *pair) Rest() interface{} {
	return p.rest
}

// SetFirst rebinds the first field to a new value.
func (p *pair) SetFirst(first interface{}) {
	p.first = first
}

// SetRest rebinds the rest field to a new value.
func (p *pair) SetRest(rest interface{}) {
	p.rest = rest
}

// First is like Common Lisp first; car of a Pair, or nil for anything else.
func First(object interface{}) interface{} {
	if pair, ok := object.(Pair); ok {
		return pair.First()
	}
	return nil
}

// Rest is like Common Lisp rest; cdr of a Pair, or nil for anything else.
func Rest(object interface{}) interface{} {
	if pair, ok := object.(Pair); ok {
		return pair.Rest()
	}
	return nil
}

// RestPair takes the rest of a list, always returning a Pair.
func RestPair(object interface{}) Pair {
	pair, _ := Rest(object).(Pair)
	return pair
}

// Last is like Common Lisp last; last element from a non-empty list, or nil for anything else.
func Last(list interface{}) interface{} {
	for Rest(list) != nil {
		list = Rest(list)
	}
	return First(list)
}

// SetFirst is like Common List (setf (first x) y) and sets the car of x to a new value.
func SetFirst(x interface{}, y interface{}) interface{} {
	if pair, ok := x.(Pair); ok {
		pair.SetFirst(y)
		return y
	}
	return Err("Attempt to set-car of a non-Pair:", List(y))
}

// SetRest is like Common Lisp (setf (rest x) y) and changes the cdr of x to be y.
func SetRest(x interface{}, y interface{}) interface{} {
	if pair, ok := x.(Pair); ok {
		pair.SetRest(y)
		return y
	}
	return Err("Attempt to set-cdr of a non-Pair: ", List(y))
}

// Second is like Common Lisp second and returns the second item in a list.
func Second(x interface{}) interface{} {
	return First(Rest(x))
}

// Third is like Common Lisp third and returns the third item in a list.
func Third(x interface{}) interface{} {
	return First(Rest(Rest(x)))
}

// List is a helper that builds a list from a slice of interfaces.
func List(elements ...interface{}) (result Pair) {
	for index := len(elements) - 1; index >= 0; index-- {
		result = NewPair(elements[index], result)
	}
	return result
}

// Cons is the Common Lisp way to create a pair.
func Cons(car interface{}, cdr interface{}) interface{} {
	return NewPair(car, cdr)
}

// Reverse flips the order of elements in a list.
func Reverse(list interface{}) (result interface{}) {
	for pair, ok := list.(Pair); ok; pair, ok = list.(Pair) {
		result = Cons(First(pair), result)
		list = pair.Rest()
	}
	return
}

// installListPrimitives adds the primitives that deal with list objects.
func installListPrimitives(environment Environment) {
	environment.DefineName(NewPrimitive("pair?", 1, 1, primitivePair))
	environment.DefineName(NewPrimitive("cons", 2, 2, primitiveCons))
	environment.DefineName(NewPrimitive("first", 1, 1, primitiveFirst))
	environment.DefineName(NewPrimitive("rest", 1, 1, primitiveRest))
	environment.DefineName(NewPrimitive("set-first!", 2, 2, primitiveSetFirst))
	environment.DefineName(NewPrimitive("set-car!", 2, 2, primitiveSetFirst))
	environment.DefineName(NewPrimitive("set-rest!", 2, 2, primitiveSetRest))
	environment.DefineName(NewPrimitive("set-cdr!", 2, 2, primitiveSetRest))
	installCxrPrimitives(environment) // car, cdr and the various compositions like caar/cdadar etc.
	environment.DefineName(NewPrimitive("null?", 1, 1, primitiveNull))
	environment.DefineName(NewPrimitive("list?", 1, 1, primitiveListP))
	environment.DefineName(NewPrimitive("make-list", 1, maxArgs, primitiveMakeList))
	environment.DefineName(NewPrimitive("list", 1, 1, primitiveList))
	environment.DefineName(NewPrimitive("length", 1, 1, primitiveLength))
	environment.DefineName(NewPrimitive("append", 0, 2, primitiveAppend))

}

// installCxrPrimitives adds all the various car/cdr/caar/cdar etc. primitives down to 4-levels of composition.
func installCxrPrimitives(environment Environment) {
	type cxr struct {
		name string
		f    func(interface{}) interface{}
	}
	cxrs := []cxr{{"a", First}, {"d", Rest}}
	for _, cxr1 := range cxrs {
		name1 := "c" + cxr1.name + "r"
		f1 := cxr1.f
		environment.DefineName(NewPrimitive(Symbol(name1), 1, 1,
			func(args Pair) interface{} { return f1(First(args)) }))
		for _, cxr2 := range cxrs {
			name2 := "c" + cxr1.name + cxr2.name + "r"
			f2 := cxr2.f
			environment.DefineName(NewPrimitive(Symbol(name2), 1, 1,
				func(args Pair) interface{} { return f1(f2(First(args))) }))
			for _, cxr3 := range cxrs {
				name3 := "c" + cxr1.name + cxr2.name + cxr3.name + "r"
				f3 := cxr3.f
				environment.DefineName(NewPrimitive(Symbol(name3), 1, 1,
					func(args Pair) interface{} { return f1(f2(f3(First(args)))) }))
				for _, cxr4 := range cxrs {
					name4 := "c" + cxr1.name + cxr2.name + cxr3.name + cxr4.name + "r"
					f4 := cxr4.f
					environment.DefineName(NewPrimitive(Symbol(name4), 1, 1,
						func(args Pair) interface{} { return f1(f2(f3(f4(First(args))))) }))
				}
			}
		}
	}
}

// primitivePair implements the pair? predicate that tests for pairs.
func primitivePair(args Pair) interface{} {
	_, ok := First(args).(Pair)
	return ok
}

// primitiveCons implements the cons function that builds a new pair.
func primitiveCons(args Pair) interface{} {
	return Cons(First(args), Second(args))
}

// primitiveFirst implements first/car.
func primitiveFirst(args Pair) interface{} {
	return First(First(args))
}

// primitiveRest implements rest/cdr.
func primitiveRest(args Pair) interface{} {
	return Rest(First(args))
}

// primitiveSetRest rebinds the car of a pair to a new value.
func primitiveSetFirst(args Pair) interface{} {
	return SetFirst(First(args), Second(args))
}

func hop(list Pair) (rest Pair, ok bool) {
	temp := list.Rest()
	if temp == nil {
		rest, ok = nil, true
	} else {
		rest, ok = temp.(Pair)
	}
	return
}

func hophop(list Pair) (rest Pair, ok bool) {
	rest, ok = hop(list)
	if ok && rest != nil {
		rest, ok = hop(rest)
	}
	return
}

// primitiveSetRest rebinds the cdr of a pair to a new value.
func primitiveSetRest(args Pair) interface{} {
	return SetRest(First(args), Second(args))
}

// primitiveNull implements a test for the empty list.
func primitiveNull(args Pair) interface{} {
	return First(args) == nil
}

// primitiveListP implements the list? predicate function which tests for well formed lists.
func primitiveListP(args Pair) interface{} {
	list := First(args)
	if list == nil {
		return true
	}
	single, ok := list.(Pair) // Walk over the list one hop at a time.
	if !ok {
		return false
	}
	double := single // Walk over the list two hops at a time.
	for ok {
		single, ok = hop(single)
		if !ok {
			return false
		}
		double, ok = hophop(double)
		if !ok {
			return false
		}
		if double == nil {
			return true
		}
		if double == single {
			// Do the pointers match?  If so, we have found a cycle.
			return false
		}
	}
	return false
}

// primitiveMakeList builds a list of length k, optionally filled with element fill.
func primitiveMakeList(args Pair) (result interface{}) {
	fill := Second(args)
	for k := uint64Constraint(First(args)); k > 0; k-- {
		result = Cons(fill, result)
	}
	return
}

// primitiveList implements the list primitive which builds a list of its evaluated arguments.
func primitiveList(args Pair) interface{} {
	return args
}

// primitiveLength returns the length of a list.
func primitiveLength(args Pair) interface{} {
	return Len(First(args))
}

// primitiveAppend builds a new list with the second one appended to the end of the first.
func primitiveAppend(args Pair) (result interface{}) {
	if args == nil {
		return nil
	}
	list := First(args)
	if Rest(args) == nil {
		return list
	}
	rest := Second(args)
	temp := result
	for Rest(list) != nil {
		temp = Cons(First(list), temp)
		list = Rest(list)
	}
	SetRest(result, rest)
	return result
}
