package gscheme

// Truth converts Scheme objects to boolean.  Only #f is false, others are true.
func Truth(x interface{}) bool {
	result, ok := x.(bool)
	return !ok || result
}

// Num converts a Scheme object to a double, or calls error.  For now, numbers are coerced to float64
// inside of gscheme.
func Num(x interface{}) float64 {
	switch value := x.(type) {
	case float64:
		return value
	case float32:
		return float64(value)
	case int64:
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
		return Num(Err("Expected a number, instead got:", List(x)))
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
