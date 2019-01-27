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

func List(elements ...interface{}) Pair {
	index := len(elements) - 1
	var result Pair = nil
	for index >= 0 {
		result = NewPair(elements[index], result)
		index--
	}
	return result
}

// Cons is the Common Lisp way to create a pair.
func Cons(car interface{}, cdr interface{}) interface{} {
	return NewPair(car, cdr)
}

// Reverse flips the order of elements in a list.
func Reverse(list interface{}) interface{} {
	var result interface{} = nil
	for pair, ok := list.(Pair); ok; pair, ok = list.(Pair) {
		result = Cons(First(pair), result)
		list = pair.Rest()
	}
	return result
}
