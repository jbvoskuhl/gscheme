package gscheme

import "fmt"

// booleanConstraint is used to enforce boolean type constraints within primitives.
func booleanConstraint(object interface{}) bool {
	result, ok := object.(bool)
	if !ok {
		Err("Expected boolean, but instead got: ", List(object))
	}
	return result
}

// stringConstraint is used to enforce string type constraints within primitives.
func stringConstraint(object interface{}) string {
	result, ok := object.(string)
	if !ok {
		Err("Expected string, but instead got: ", List(object))
	}
	return result
}

// symbolConstraint is used to enforce symbol type constraints within primitives.
func symbolConstraint(object interface{}) Symbol {
	result, ok := object.(Symbol)
	if !ok {
		Err("Expected symbol, but instead got: ", List(object))
	}
	return result
}

// characterConstraint is used to enforce character type constraints within primitives.
func characterConstraint(object interface{}) rune {
	result, ok := object.(rune)
	if !ok {
		Err("Expected character, but instead got: ", List(object))
	}
	return result
}

// uint64Constraint is used to coerce integers into uint64 within primitives.
func uint64Constraint(object interface{}) uint64 {
	switch value := object.(type) {
	case uint:
		return uint64(value)
	case uint8:
		return uint64(value)
	case uint16:
		return uint64(value)
	case uint32:
		return uint64(value)
	case uint64:
		return uint64(value)
	case int:
		return uint64(value)
	case int8:
		return uint64(value)
	case int16:
		return uint64(value)
	case int32:
		return uint64(value)
	case int64:
		return uint64(value)
	default:
		Err(fmt.Sprintf("Expected integer type, but instead got: %T.", value), List(value))
	}
	return 0 // Should be uncalled since Err will raise an exception.
}

// vectorConstraint is used to enforce vector ([]interface{}) type constraints within primitives.
func vectorConstraint(object interface{}) []interface{} {
	result, ok := object.([]interface{})
	if !ok {
		Err("Expected vector, but instead got: ", List(object))
	}
	return result
}

// bytevectorConstraint is used to enforce bytevector ([]uint8) type constraints within primitives.
func bytevectorConstraint(object interface{}) []uint8 {
	result, ok := object.([]uint8)
	if !ok {
		Err("Expected bytevector, but instead got: ", List(object))
	}
	return result
}

// byteConstraint validates that a float64 value is in the 0-255 range and returns it as uint8.
func byteConstraint(object interface{}) uint8 {
	f, ok := object.(float64)
	if !ok || f < 0 || f > 255 || f != float64(int(f)) {
		Err("Expected exact integer in range 0-255, but instead got: ", List(object))
	}
	return uint8(f)
}

// integerConstraint is used to enforce integer type constraints within primitives.
func integerConstraint(object interface{}) interface{} {
	switch value := object.(type) {
	case uint:
		return value
	case uint8:
		return value
	case uint16:
		return value
	case uint32:
		return value
	case uint64:
		return value
	case int:
		return value
	case int8:
		return value
	case int16:
		return value
	case int32:
		return value
	case int64:
		return value
	default:
		return Err(fmt.Sprintf("Expected integer type, but instead got: %T.", value), List(value))
	}
}
