package gscheme

import "math"

// Truth converts Scheme objects to boolean.  Only #f is false, others are true.
func Truth(x interface{}) bool {
	result, ok := x.(bool)
	return !ok || result
}

// ToFloat64 converts a Scheme object to a float64, or calls error.
func ToFloat64(x interface{}) float64 {
	switch value := x.(type) {
	case int64:
		return float64(value)
	case float64:
		return value
	case float32:
		return float64(value)
	case int32:
		return float64(value)
	case int16:
		return float64(value)
	case int8:
		return float64(value)
	case uint64:
		return float64(value)
	case uint32:
		return float64(value)
	case uint16:
		return float64(value)
	case uint8:
		return float64(value)
	default:
		return ToFloat64(Err("Expected a number, instead got:", List(x)))
	}
}

// Num converts a Scheme object to a float64. Alias for ToFloat64.
func Num(x interface{}) float64 {
	return ToFloat64(x)
}

// IsInt64 returns true if x is an int64.
func IsInt64(x interface{}) bool {
	_, ok := x.(int64)
	return ok
}

// IsFloat64 returns true if x is a float64.
func IsFloat64(x interface{}) bool {
	_, ok := x.(float64)
	return ok
}

// IsWholeNumber returns true if x is an int64 or a whole-number float64.
func IsWholeNumber(x interface{}) bool {
	switch v := x.(type) {
	case int64:
		return true
	case float64:
		return v == math.Trunc(v) && !math.IsInf(v, 0) && !math.IsNaN(v)
	default:
		return false
	}
}

// Len returns the length of a list.  Non-list objects have length zero.
func Len(list interface{}) int {
	result := 0
	pair, ok := list.(Pair)
	for ok {
		result++
		list = pair.Rest()
		pair, ok = list.(Pair)
	}
	return result
}
