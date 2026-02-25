package gscheme

import "fmt"

// errorKind distinguishes categories of errors per R7RS.
type errorKind int

const (
	errorKindGeneral errorKind = iota
	errorKindRead
	errorKindFile
)

// Error is the interface for all Scheme errors.
type Error interface {
	fmt.Stringer
	error
	GetMessage() string
	GetIrritants() Pair
	IsReadError() bool
	IsFileError() bool
}

// schemeError is the concrete type behind the Error interface.
type schemeError struct {
	message   string
	irritants Pair
	kind      errorKind
}

// NewError creates a new Error object, but does not raise a panic.
func NewError(message string, irritants Pair) Error {
	return &schemeError{message: message, irritants: irritants}
}

// NewReadError creates a new read-category Error object.
func NewReadError(message string, irritants Pair) Error {
	return &schemeError{message: message, irritants: irritants, kind: errorKindRead}
}

// NewFileError creates a new file-category Error object.
func NewFileError(message string, irritants Pair) Error {
	return &schemeError{message: message, irritants: irritants, kind: errorKindFile}
}

// GetMessage gives back the message provided when error was called.
func (e schemeError) GetMessage() string {
	return e.message
}

// GetIrritants returns the extra objects provided when error was called.
func (e schemeError) GetIrritants() Pair {
	return e.irritants
}

// String returns the error message followed by any irritant values.
func (e schemeError) String() string {
	if e.irritants == nil {
		return e.message
	}
	s := e.message
	for cursor := interface{}(e.irritants); cursor != nil; {
		if p, ok := cursor.(Pair); ok {
			if s != "" && s[len(s)-1] != ' ' {
				s += " "
			}
			s += Stringify(First(p))
			cursor = Rest(p)
		} else {
			break
		}
	}
	return s
}

// Error implements the error interface.
func (e schemeError) Error() string {
	return e.String()
}

// IsReadError returns true if this error is a read error.
func (e schemeError) IsReadError() bool { return e.kind == errorKindRead }

// IsFileError returns true if this error is a file error.
func (e schemeError) IsFileError() bool { return e.kind == errorKindFile }

// raisedError wraps an arbitrary Scheme value so raise can panic with non-Error objects
// and have them caught by Eval's defer/recover.
type raisedError struct {
	value interface{}
}

func (r *raisedError) String() string     { return Stringify(r.value) }
func (r *raisedError) Error() string      { return Stringify(r.value) }
func (r *raisedError) GetMessage() string { return Stringify(r.value) }
func (r *raisedError) GetIrritants() Pair { return nil }
func (r *raisedError) IsReadError() bool  { return false }
func (r *raisedError) IsFileError() bool  { return false }

// unwrapRaisedValue extracts the original value from a raisedError wrapper,
// or returns the Error as-is if it was raised via error/Err.
func unwrapRaisedValue(err Error) interface{} {
	if raised, ok := err.(*raisedError); ok {
		return raised.value
	}
	return err
}

// Err is used in gscheme to package up an error and raise it using the panic functionality.
func Err(message string, irritants Pair) interface{} {
	panic(NewError(message, irritants))
}

// ReadErr packages up a read error and raises it using panic.
func ReadErr(message string, irritants Pair) interface{} {
	panic(NewReadError(message, irritants))
}

// FileErr packages up a file error and raises it using panic.
func FileErr(message string, irritants Pair) interface{} {
	panic(NewFileError(message, irritants))
}
