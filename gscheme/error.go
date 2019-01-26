package gscheme

import "fmt"

// Error is the interface for all Scheme errors.
// TODO(jbvoskuhl): Include the type of error so we can tell user/file/read errors apart.
type Error interface {
	fmt.Stringer
	error
	GetMessage() string
	GetIrritants() Pair
}

// schemeError is the concrete type behind the Error interface.
type schemeError struct {
	message   string
	irritants Pair
}

// NewError creates a new Error object, but does not raise a panic.
func NewError(message string, irritants Pair) Error {
	return &schemeError{message, irritants}
}

// GetMessage gives back the message provided when error was called.
func (e schemeError) GetMessage() string {
	return e.message
}

// GetIrritants returns the extra objects provided when error was called.
func (e schemeError) GetIrritants() Pair {
	return e.irritants
}

// Convert the error to a string including the message and irritants.
func (e schemeError) String() string {
	return e.message
}

// Convert the error to a string including the message and irritants.
func (e schemeError) Error() string {
	return e.message
}

// Err is used in gscheme to package up an error and raise it using the panic functionality.
func Err(message string, irritants Pair) interface{} {
	panic(NewError(message, irritants))
}
