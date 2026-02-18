package gscheme

import (
	"fmt"
	"math/big"
)

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
	case *big.Int:
		if value.IsUint64() {
			return value.Uint64()
		}
		Err(fmt.Sprintf("Expected integer in uint64 range, but instead got: %s.", value.String()), List(value))
	case *big.Rat:
		if value.IsInt() {
			n := value.Num()
			if n.IsUint64() {
				return n.Uint64()
			}
		}
		Err(fmt.Sprintf("Expected integer type, but instead got rational: %s.", value.RatString()), List(value))
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

// byteConstraint validates that a value is in the 0-255 range and returns it as uint8.
func byteConstraint(object interface{}) uint8 {
	switch v := object.(type) {
	case int64:
		if v < 0 || v > 255 {
			Err("Expected exact integer in range 0-255, but instead got: ", List(object))
		}
		return uint8(v)
	case *big.Int:
		if v.IsInt64() {
			n := v.Int64()
			if n >= 0 && n <= 255 {
				return uint8(n)
			}
		}
		Err("Expected exact integer in range 0-255, but instead got: ", List(object))
		return 0
	case *big.Rat:
		if v.IsInt() {
			n := v.Num().Int64()
			if n >= 0 && n <= 255 {
				return uint8(n)
			}
		}
		Err("Expected exact integer in range 0-255, but instead got: ", List(object))
		return 0
	case float64:
		if v < 0 || v > 255 || v != float64(int(v)) {
			Err("Expected exact integer in range 0-255, but instead got: ", List(object))
		}
		return uint8(v)
	default:
		Err("Expected exact integer in range 0-255, but instead got: ", List(object))
		return 0
	}
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
	case *big.Int:
		return SimplifyBigInt(value)
	case *big.Rat:
		if value.IsInt() {
			return SimplifyBigInt(value.Num())
		}
		return Err(fmt.Sprintf("Expected integer type, but instead got rational: %s.", value.RatString()), List(value))
	default:
		return Err(fmt.Sprintf("Expected integer type, but instead got: %T.", value), List(value))
	}
}
