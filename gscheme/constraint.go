package gscheme

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
