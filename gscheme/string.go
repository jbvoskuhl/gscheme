package gscheme

import (
	"fmt"
	"math/big"
	"strconv"
	"strings"
)

// Stringify is the Scheme mechanism to turn any object into a printable string.
func Stringify(object interface{}) string {
	if object == nil {
		return "()"
	}
	switch value := object.(type) {
	case bool:
		if value {
			return "#t"
		} else {
			return "#f"
		}
	case rune:
		return stringifyCharacter(value)
	case []byte:
		return stringifyByteVector(value)
	case []interface{}:
		return stringifyVector(value)
	case string:
		return fmt.Sprintf("\"%s\"", value)
	case Symbol:
		return string(value)
	case int64:
		return strconv.FormatInt(value, 10)
	case *big.Int:
		return value.String()
	case *big.Rat:
		if value.IsInt() {
			return value.Num().String()
		}
		return value.RatString()
	case complex128:
		return stringifyComplex(value)
	case fmt.Stringer:
		return value.String()
	default:
		// Fall back to Go's stringify mechanism which works for most types.
		return fmt.Sprint(object)
	}
}

// reverseNamedCharacters maps rune values to their R7RS character names.
var reverseNamedCharacters = map[rune]string{
	'\x07': "alarm",
	'\x08': "backspace",
	'\x7F': "delete",
	'\x1B': "escape",
	'\n':   "newline",
	'\x00': "null",
	'\r':   "return",
	' ':    "space",
	'\t':   "tab",
}

// stringifyCharacter formats a rune as a Scheme character literal.
func stringifyCharacter(ch rune) string {
	if name, ok := reverseNamedCharacters[ch]; ok {
		return "#\\" + name
	}
	if ch >= '!' && ch <= '~' {
		// Printable ASCII
		return "#\\" + string(ch)
	}
	// Non-printable or non-ASCII: use hex scalar value
	return fmt.Sprintf("#\\x%X", ch)
}

// stringifyByteVector takes a bytevector (which we model as a slice of uint8) and turns it into a string.
func stringifyByteVector(vector []uint8) string {
	items := make([]string, len(vector))
	for index, item := range vector {
		items[index] = Stringify(item)
	}
	return fmt.Sprint("#u8(", strings.Join(items, " "), ")")
}

// stringifyVector takes a vector (which we model as a slice of interfaces) and turns it into a string.
func stringifyVector(vector []interface{}) string {
	items := make([]string, len(vector))
	for index, item := range vector {
		items[index] = Stringify(item)
	}
	return fmt.Sprint("#(", strings.Join(items, " "), ")")
}

// stringifyComplex formats a complex number in Scheme notation.
func stringifyComplex(c complex128) string {
	r, i := real(c), imag(c)
	if i == 0 {
		return fmt.Sprint(r)
	}
	if r == 0 {
		if i == 1 {
			return "+i"
		} else if i == -1 {
			return "-i"
		}
		if i > 0 {
			return fmt.Sprintf("+%vi", i)
		}
		return fmt.Sprintf("%vi", i)
	}
	if i == 1 {
		return fmt.Sprintf("%v+i", r)
	} else if i == -1 {
		return fmt.Sprintf("%v-i", r)
	} else if i > 0 {
		return fmt.Sprintf("%v+%vi", r, i)
	}
	return fmt.Sprintf("%v%vi", r, i)
}
