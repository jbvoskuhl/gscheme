package gscheme

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
	"unicode/utf8"
)

func installStringPrimitives(environment Environment) {
	// Construction & Access
	environment.DefineName(NewPrimitive("make-string", 1, 2, primitiveMakeString))
	environment.DefineName(NewPrimitive("string", 0, maxArgs, primitiveString))
	environment.DefineName(NewPrimitive("string-length", 1, 1, primitiveStringLength))
	environment.DefineName(NewPrimitive("string-ref", 2, 2, primitiveStringRef))
	environment.DefineName(NewPrimitive("substring", 3, 3, primitiveSubstring))
	environment.DefineName(NewPrimitive("string-copy", 1, 3, primitiveStringCopy))

	// Case-sensitive comparisons
	environment.DefineName(NewPrimitive("string=?", 2, maxArgs, primitiveStringEqual))
	environment.DefineName(NewPrimitive("string<?", 2, maxArgs, primitiveStringLessThan))
	environment.DefineName(NewPrimitive("string>?", 2, maxArgs, primitiveStringGreaterThan))
	environment.DefineName(NewPrimitive("string<=?", 2, maxArgs, primitiveStringLessThanOrEqual))
	environment.DefineName(NewPrimitive("string>=?", 2, maxArgs, primitiveStringGreaterThanOrEqual))

	// Case-insensitive comparisons
	environment.DefineName(NewPrimitive("string-ci=?", 2, maxArgs, primitiveStringCIEqual))
	environment.DefineName(NewPrimitive("string-ci<?", 2, maxArgs, primitiveStringCILessThan))
	environment.DefineName(NewPrimitive("string-ci>?", 2, maxArgs, primitiveStringCIGreaterThan))
	environment.DefineName(NewPrimitive("string-ci<=?", 2, maxArgs, primitiveStringCILessThanOrEqual))
	environment.DefineName(NewPrimitive("string-ci>=?", 2, maxArgs, primitiveStringCIGreaterThanOrEqual))

	// Case transformation
	environment.DefineName(NewPrimitive("string-upcase", 1, 1, primitiveStringUpcase))
	environment.DefineName(NewPrimitive("string-downcase", 1, 1, primitiveStringDowncase))
	environment.DefineName(NewPrimitive("string-foldcase", 1, 1, primitiveStringFoldcase))

	// Building
	environment.DefineName(NewPrimitive("string-append", 0, maxArgs, primitiveStringAppend))
	environment.DefineName(NewPrimitive("number->string", 1, 2, primitiveNumberToString))

	// Conversion
	environment.DefineName(NewPrimitive("string->list", 1, 3, primitiveStringToList))
	environment.DefineName(NewPrimitive("list->string", 1, 1, primitiveListToString))
	environment.DefineName(NewPrimitive("string->number", 1, 2, primitiveStringToNumber))

	// Higher-order
	environment.DefineName(NewHigherOrderPrimitive("string-map", 2, maxArgs, primitiveStringMap))
	environment.DefineName(NewHigherOrderPrimitive("string-for-each", 2, maxArgs, primitiveStringForEach))
}

// primitiveMakeString creates a string of k copies of an optional character (default space).
func primitiveMakeString(args Pair) interface{} {
	k := indexConstraint(First(args))
	ch := ' '
	if Second(args) != nil {
		ch = characterConstraint(Second(args))
	}
	return strings.Repeat(string(ch), k)
}

// primitiveString builds a string from character arguments.
func primitiveString(args Pair) interface{} {
	var builder strings.Builder
	for args != nil {
		builder.WriteRune(characterConstraint(First(args)))
		args = RestPair(args)
	}
	return builder.String()
}

// primitiveStringLength returns the length of a string in characters (runes).
func primitiveStringLength(args Pair) interface{} {
	s := stringConstraint(First(args))
	return int64(utf8.RuneCountInString(s))
}

// primitiveStringRef returns the character at index k.
func primitiveStringRef(args Pair) interface{} {
	s := stringConstraint(First(args))
	k := indexConstraint(Second(args))
	runes := []rune(s)
	if k < 0 || k >= len(runes) {
		return Err(fmt.Sprintf("string-ref: index %d out of range for string of length %d", k, len(runes)), args)
	}
	return runes[k]
}

// primitiveSubstring extracts a substring from start (inclusive) to end (exclusive).
func primitiveSubstring(args Pair) interface{} {
	s := stringConstraint(First(args))
	start := indexConstraint(Second(args))
	end := indexConstraint(Third(args))
	runes := []rune(s)
	if start < 0 || end < start || end > len(runes) {
		return Err(fmt.Sprintf("substring: invalid range [%d, %d) for string of length %d", start, end, len(runes)), args)
	}
	return string(runes[start:end])
}

// primitiveStringCopy returns a copy of the string, with optional start and end indices.
func primitiveStringCopy(args Pair) interface{} {
	s := stringConstraint(First(args))
	runes := []rune(s)
	start, end := 0, len(runes)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(runes) {
		return Err(fmt.Sprintf("string-copy: invalid range [%d, %d) for string of length %d", start, end, len(runes)), args)
	}
	return string(runes[start:end])
}

// stringCompare creates a variadic string comparison primitive.
func stringCompare(pred func(string, string) bool) func(Pair) interface{} {
	return func(args Pair) interface{} {
		current := stringConstraint(First(args))
		args = RestPair(args)
		for args != nil {
			next := stringConstraint(First(args))
			if !pred(current, next) {
				return false
			}
			current = next
			args = RestPair(args)
		}
		return true
	}
}

// foldString applies Unicode simple case folding to a string.
// R7RS specifies simple case folding which is equivalent to strings.ToLower
// for character-by-character folding.
func foldString(s string) string {
	return strings.ToLower(s)
}

// stringCompareCaseInsensitive creates a variadic case-insensitive string comparison primitive.
func stringCompareCaseInsensitive(pred func(string, string) bool) func(Pair) interface{} {
	return stringCompare(func(x, y string) bool {
		return pred(foldString(x), foldString(y))
	})
}

var primitiveStringEqual = stringCompare(func(x, y string) bool { return x == y })
var primitiveStringLessThan = stringCompare(func(x, y string) bool { return x < y })
var primitiveStringGreaterThan = stringCompare(func(x, y string) bool { return x > y })
var primitiveStringLessThanOrEqual = stringCompare(func(x, y string) bool { return x <= y })
var primitiveStringGreaterThanOrEqual = stringCompare(func(x, y string) bool { return x >= y })

var primitiveStringCIEqual = stringCompareCaseInsensitive(func(x, y string) bool { return x == y })
var primitiveStringCILessThan = stringCompareCaseInsensitive(func(x, y string) bool { return x < y })
var primitiveStringCIGreaterThan = stringCompareCaseInsensitive(func(x, y string) bool { return x > y })
var primitiveStringCILessThanOrEqual = stringCompareCaseInsensitive(func(x, y string) bool { return x <= y })
var primitiveStringCIGreaterThanOrEqual = stringCompareCaseInsensitive(func(x, y string) bool { return x >= y })

// primitiveStringUpcase returns an uppercased copy of the string.
func primitiveStringUpcase(args Pair) interface{} {
	return strings.ToUpper(stringConstraint(First(args)))
}

// primitiveStringDowncase returns a lowercased copy of the string.
func primitiveStringDowncase(args Pair) interface{} {
	return strings.ToLower(stringConstraint(First(args)))
}

// primitiveStringFoldcase returns a case-folded copy of the string.
func primitiveStringFoldcase(args Pair) interface{} {
	return foldString(stringConstraint(First(args)))
}

// primitiveStringAppend concatenates all string arguments.
func primitiveStringAppend(args Pair) interface{} {
	var builder strings.Builder
	for args != nil {
		builder.WriteString(stringConstraint(First(args)))
		args = RestPair(args)
	}
	return builder.String()
}

// primitiveNumberToString converts a number to its string representation with optional radix.
func primitiveNumberToString(args Pair) interface{} {
	num := First(args)
	radix := 10
	if Second(args) != nil {
		radix = indexConstraint(Second(args))
	}
	if v, ok := num.(int64); ok {
		return strconv.FormatInt(v, radix)
	}
	if v, ok := num.(*big.Int); ok {
		return v.Text(radix)
	}
	if r, ok := num.(*big.Rat); ok {
		if radix == 10 {
			return r.RatString()
		}
		if r.IsInt() {
			return r.Num().Text(radix)
		}
		return r.RatString()
	}
	if radix == 10 {
		return fmt.Sprint(num)
	}
	n := int64(Num(num))
	return strconv.FormatInt(n, radix)
}

// primitiveStringToList converts a string to a list of characters, with optional start and end indices.
func primitiveStringToList(args Pair) interface{} {
	s := stringConstraint(First(args))
	runes := []rune(s)
	start, end := 0, len(runes)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(runes) {
		return Err(fmt.Sprintf("string->list: invalid range [%d, %d) for string of length %d", start, end, len(runes)), args)
	}
	runes = runes[start:end]
	elements := make([]interface{}, len(runes))
	for i, r := range runes {
		elements[i] = r
	}
	return List(elements...)
}

// primitiveListToString converts a list of characters to a string.
func primitiveListToString(args Pair) interface{} {
	list := First(args)
	var builder strings.Builder
	for list != nil {
		listPair, ok := list.(Pair)
		if !ok {
			return Err("list->string: expected a proper list", args)
		}
		builder.WriteRune(characterConstraint(First(listPair)))
		list = listPair.Rest()
	}
	return builder.String()
}

// primitiveStringToNumber parses a number from a string with optional radix.
func primitiveStringToNumber(args Pair) interface{} {
	s := stringConstraint(First(args))
	radix := 10
	if Second(args) != nil {
		radix = indexConstraint(Second(args))
	}
	if radix == 10 {
		// Try integer first
		if !strings.ContainsAny(s, ".eE") {
			if n, err := strconv.ParseInt(s, 10, 64); err == nil {
				return n
			}
			if bi, ok := new(big.Int).SetString(s, 10); ok {
				return bi
			}
		}
		f, err := strconv.ParseFloat(s, 64)
		if err != nil {
			return false
		}
		return f
	}
	n, err := strconv.ParseInt(s, radix, 64)
	if err != nil {
		if bi, ok := new(big.Int).SetString(s, radix); ok {
			return bi
		}
		return false
	}
	return n
}

// primitiveStringMap applies a procedure to corresponding characters of one or more strings.
func primitiveStringMap(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("string-map: first argument must be a procedure", List(First(args)))
	}
	rest := RestPair(args)
	var runeSlices [][]rune
	minLen := int(^uint(0) >> 1) // max int
	for rest != nil {
		s := stringConstraint(First(rest))
		runes := []rune(s)
		runeSlices = append(runeSlices, runes)
		if len(runes) < minLen {
			minLen = len(runes)
		}
		rest = RestPair(rest)
	}
	var builder strings.Builder
	for i := 0; i < minLen; i++ {
		var quotedArgs Pair
		var tail Pair
		for _, runes := range runeSlices {
			quoted := List(Symbol("quote"), runes[i])
			newPair := NewPair(quoted, nil)
			if quotedArgs == nil {
				quotedArgs = newPair
				tail = quotedArgs
			} else {
				tail.SetRest(newPair)
				tail = newPair
			}
		}
		result := proc.Apply(interpreter, quotedArgs, environment)
		r, ok := result.(rune)
		if !ok {
			return Err("string-map: procedure must return a character", List(result))
		}
		builder.WriteRune(r)
	}
	return builder.String()
}

// primitiveStringForEach applies a procedure to corresponding characters of one or more strings for side effects.
func primitiveStringForEach(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("string-for-each: first argument must be a procedure", List(First(args)))
	}
	rest := RestPair(args)
	var runeSlices [][]rune
	minLen := int(^uint(0) >> 1) // max int
	for rest != nil {
		s := stringConstraint(First(rest))
		runes := []rune(s)
		runeSlices = append(runeSlices, runes)
		if len(runes) < minLen {
			minLen = len(runes)
		}
		rest = RestPair(rest)
	}
	for i := 0; i < minLen; i++ {
		var quotedArgs Pair
		var tail Pair
		for _, runes := range runeSlices {
			quoted := List(Symbol("quote"), runes[i])
			newPair := NewPair(quoted, nil)
			if quotedArgs == nil {
				quotedArgs = newPair
				tail = quotedArgs
			} else {
				tail.SetRest(newPair)
				tail = newPair
			}
		}
		proc.Apply(interpreter, quotedArgs, environment)
	}
	return nil
}
