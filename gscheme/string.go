package gscheme

import "fmt"

// Stringify is the Scheme mechanism to turn any object into a printable string.
func Stringify(object interface{}) string {
	if object == nil {
		return "()"
	}
	switch value := object.(type) {
	case string:
		return fmt.Sprintf("\"%s\"", value)
	case fmt.Stringer:
		return value.String()
	default:
		return fmt.Sprint(object)
	}
}
