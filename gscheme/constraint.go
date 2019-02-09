package gscheme

import "fmt"

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
