package gscheme

import (
	"fmt"
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
	case []byte:
		return stringifyByteVector(value)
	case []interface{}:
		return stringifyVector(value)
	case string:
		return fmt.Sprintf("\"%s\"", value)
	case Symbol:
		return string(value)
	case complex128:
		return stringifyComplex(value)
	case fmt.Stringer:
		return value.String()
	default:
		// Fall back to Go's stringify mechanism which works for most types.
		return fmt.Sprint(object)
	}
}

// stringifyVector takes a bytevector (which we model as a slice of uint8) and turns it into a string.
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
