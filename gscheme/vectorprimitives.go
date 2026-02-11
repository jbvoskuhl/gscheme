package gscheme

import "fmt"

func installVectorPrimitives(environment Environment) {
	// Vector construction & access
	environment.DefineName(NewPrimitive("make-vector", 1, 2, primitiveMakeVector))
	environment.DefineName(NewPrimitive("vector", 0, maxArgs, primitiveVector))
	environment.DefineName(NewPrimitive("vector-length", 1, 1, primitiveVectorLength))
	environment.DefineName(NewPrimitive("vector-ref", 2, 2, primitiveVectorRef))
	environment.DefineName(NewPrimitive("vector-set!", 3, 3, primitiveVectorSet))
	environment.DefineName(NewPrimitive("vector-fill!", 2, 2, primitiveVectorFill))
	environment.DefineName(NewPrimitive("vector->list", 1, 1, primitiveVectorToList))
	environment.DefineName(NewPrimitive("list->vector", 1, 1, primitiveListToVector))
	environment.DefineName(NewPrimitive("vector-copy", 1, 3, primitiveVectorCopy))
	environment.DefineName(NewPrimitive("vector-append", 0, maxArgs, primitiveVectorAppend))
	environment.DefineName(NewPrimitive("vector->string", 1, 1, primitiveVectorToString))
	environment.DefineName(NewPrimitive("string->vector", 1, 1, primitiveStringToVector))

	// Vector higher-order
	environment.DefineName(NewHigherOrderPrimitive("vector-map", 2, 2, primitiveVectorMap))
	environment.DefineName(NewHigherOrderPrimitive("vector-for-each", 2, 2, primitiveVectorForEach))

	// Bytevector construction & access
	environment.DefineName(NewPrimitive("make-bytevector", 1, 2, primitiveMakeBytevector))
	environment.DefineName(NewPrimitive("bytevector", 0, maxArgs, primitiveBytevector))
	environment.DefineName(NewPrimitive("bytevector-length", 1, 1, primitiveBytevectorLength))
	environment.DefineName(NewPrimitive("bytevector-u8-ref", 2, 2, primitiveBytevectorU8Ref))
	environment.DefineName(NewPrimitive("bytevector-u8-set!", 3, 3, primitiveBytevectorU8Set))
	environment.DefineName(NewPrimitive("bytevector-copy", 1, 3, primitiveBytevectorCopy))
	environment.DefineName(NewPrimitive("bytevector-append", 0, maxArgs, primitiveBytevectorAppend))
	environment.DefineName(NewPrimitive("bytevector-copy!", 3, 5, primitiveBytevectorCopyTo))
	environment.DefineName(NewPrimitive("utf8->string", 1, 3, primitiveUtf8ToString))
	environment.DefineName(NewPrimitive("string->utf8", 1, 3, primitiveStringToUtf8))
}

// --- Vector Primitives ---

// primitiveMakeVector creates a vector of k elements, optionally filled with a value.
func primitiveMakeVector(args Pair) interface{} {
	k := indexConstraint(First(args))
	if k < 0 {
		return Err("make-vector: length must be non-negative", args)
	}
	vec := make([]interface{}, k)
	if fill := Second(args); fill != nil {
		for i := range vec {
			vec[i] = fill
		}
	}
	return vec
}

// primitiveVector builds a vector from its arguments.
func primitiveVector(args Pair) interface{} {
	var elems []interface{}
	for args != nil {
		elems = append(elems, First(args))
		args = RestPair(args)
	}
	if elems == nil {
		return []interface{}{}
	}
	return elems
}

// primitiveVectorLength returns the length of a vector as float64.
func primitiveVectorLength(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	return float64(len(vec))
}

// primitiveVectorRef returns the element at index k.
func primitiveVectorRef(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	k := indexConstraint(Second(args))
	if k < 0 || k >= len(vec) {
		return Err(fmt.Sprintf("vector-ref: index %d out of range for vector of length %d", k, len(vec)), args)
	}
	return vec[k]
}

// primitiveVectorSet sets the element at index k.
func primitiveVectorSet(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	k := indexConstraint(Second(args))
	if k < 0 || k >= len(vec) {
		return Err(fmt.Sprintf("vector-set!: index %d out of range for vector of length %d", k, len(vec)), args)
	}
	vec[k] = Third(args)
	return nil
}

// primitiveVectorFill fills all elements of a vector with a value.
func primitiveVectorFill(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	fill := Second(args)
	for i := range vec {
		vec[i] = fill
	}
	return nil
}

// primitiveVectorToList converts a vector to a list.
func primitiveVectorToList(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	if len(vec) == 0 {
		return nil
	}
	return List(vec...)
}

// primitiveListToVector converts a list to a vector.
func primitiveListToVector(args Pair) interface{} {
	list := First(args)
	var elems []interface{}
	for list != nil {
		listPair, ok := list.(Pair)
		if !ok {
			return Err("list->vector: expected a proper list", args)
		}
		elems = append(elems, First(listPair))
		list = listPair.Rest()
	}
	if elems == nil {
		return []interface{}{}
	}
	return elems
}

// primitiveVectorCopy copies a vector, with optional start and end indices.
func primitiveVectorCopy(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	start := 0
	end := len(vec)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(vec) {
		return Err(fmt.Sprintf("vector-copy: invalid range [%d, %d) for vector of length %d", start, end, len(vec)), args)
	}
	result := make([]interface{}, end-start)
	copy(result, vec[start:end])
	return result
}

// primitiveVectorAppend concatenates zero or more vectors.
func primitiveVectorAppend(args Pair) interface{} {
	var result []interface{}
	for args != nil {
		vec := vectorConstraint(First(args))
		result = append(result, vec...)
		args = RestPair(args)
	}
	if result == nil {
		return []interface{}{}
	}
	return result
}

// primitiveVectorToString converts a vector of characters to a string.
func primitiveVectorToString(args Pair) interface{} {
	vec := vectorConstraint(First(args))
	runes := make([]rune, len(vec))
	for i, elem := range vec {
		runes[i] = characterConstraint(elem)
	}
	return string(runes)
}

// primitiveStringToVector converts a string to a vector of characters.
func primitiveStringToVector(args Pair) interface{} {
	s := stringConstraint(First(args))
	runes := []rune(s)
	vec := make([]interface{}, len(runes))
	for i, r := range runes {
		vec[i] = r
	}
	return vec
}

// primitiveVectorMap applies a procedure to each element and returns a new vector.
func primitiveVectorMap(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("vector-map: first argument must be a procedure", List(First(args)))
	}
	vec := vectorConstraint(Second(args))
	result := make([]interface{}, len(vec))
	for i, elem := range vec {
		quotedArg := List(List(Symbol("quote"), elem))
		result[i] = proc.Apply(interpreter, quotedArg, environment)
	}
	return result
}

// primitiveVectorForEach applies a procedure to each element for side effects.
func primitiveVectorForEach(interpreter Scheme, args Pair, environment Environment) interface{} {
	proc, ok := First(args).(Applyer)
	if !ok {
		return Err("vector-for-each: first argument must be a procedure", List(First(args)))
	}
	vec := vectorConstraint(Second(args))
	for _, elem := range vec {
		quotedArg := List(List(Symbol("quote"), elem))
		proc.Apply(interpreter, quotedArg, environment)
	}
	return nil
}

// --- Bytevector Primitives ---

// primitiveMakeBytevector creates a bytevector of k bytes, optionally filled with a byte value.
func primitiveMakeBytevector(args Pair) interface{} {
	k := indexConstraint(First(args))
	if k < 0 {
		return Err("make-bytevector: length must be non-negative", args)
	}
	bv := make([]uint8, k)
	if Second(args) != nil {
		fill := byteConstraint(Second(args))
		for i := range bv {
			bv[i] = fill
		}
	}
	return bv
}

// primitiveBytevector builds a bytevector from byte arguments.
func primitiveBytevector(args Pair) interface{} {
	var bytes []uint8
	for args != nil {
		bytes = append(bytes, byteConstraint(First(args)))
		args = RestPair(args)
	}
	if bytes == nil {
		return []uint8{}
	}
	return bytes
}

// primitiveBytevectorLength returns the length of a bytevector as float64.
func primitiveBytevectorLength(args Pair) interface{} {
	bv := bytevectorConstraint(First(args))
	return float64(len(bv))
}

// primitiveBytevectorU8Ref returns the byte at index as float64.
func primitiveBytevectorU8Ref(args Pair) interface{} {
	bv := bytevectorConstraint(First(args))
	k := indexConstraint(Second(args))
	if k < 0 || k >= len(bv) {
		return Err(fmt.Sprintf("bytevector-u8-ref: index %d out of range for bytevector of length %d", k, len(bv)), args)
	}
	return float64(bv[k])
}

// primitiveBytevectorU8Set sets the byte at index.
func primitiveBytevectorU8Set(args Pair) interface{} {
	bv := bytevectorConstraint(First(args))
	k := indexConstraint(Second(args))
	if k < 0 || k >= len(bv) {
		return Err(fmt.Sprintf("bytevector-u8-set!: index %d out of range for bytevector of length %d", k, len(bv)), args)
	}
	bv[k] = byteConstraint(Third(args))
	return nil
}

// primitiveBytevectorCopy copies a bytevector, with optional start and end indices.
func primitiveBytevectorCopy(args Pair) interface{} {
	bv := bytevectorConstraint(First(args))
	start := 0
	end := len(bv)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(bv) {
		return Err(fmt.Sprintf("bytevector-copy: invalid range [%d, %d) for bytevector of length %d", start, end, len(bv)), args)
	}
	result := make([]uint8, end-start)
	copy(result, bv[start:end])
	return result
}

// primitiveBytevectorAppend concatenates zero or more bytevectors.
func primitiveBytevectorAppend(args Pair) interface{} {
	var result []uint8
	for args != nil {
		bv := bytevectorConstraint(First(args))
		result = append(result, bv...)
		args = RestPair(args)
	}
	if result == nil {
		return []uint8{}
	}
	return result
}

// primitiveBytevectorCopyTo implements bytevector-copy! (to at from [start [end]]).
func primitiveBytevectorCopyTo(args Pair) interface{} {
	to := bytevectorConstraint(First(args))
	at := indexConstraint(Second(args))
	from := bytevectorConstraint(Third(args))
	start := 0
	end := len(from)
	// 4th arg: start
	fourth := First(Rest(Rest(Rest(args))))
	if fourth != nil {
		start = indexConstraint(fourth)
	}
	// 5th arg: end
	fifth := First(Rest(Rest(Rest(Rest(args)))))
	if fifth != nil {
		end = indexConstraint(fifth)
	}
	if start < 0 || end < start || end > len(from) {
		return Err(fmt.Sprintf("bytevector-copy!: invalid source range [%d, %d) for bytevector of length %d", start, end, len(from)), args)
	}
	count := end - start
	if at < 0 || at+count > len(to) {
		return Err(fmt.Sprintf("bytevector-copy!: destination range [%d, %d) out of range for bytevector of length %d", at, at+count, len(to)), args)
	}
	copy(to[at:], from[start:end])
	return nil
}

// primitiveUtf8ToString decodes a UTF-8 bytevector to a string, with optional start and end.
func primitiveUtf8ToString(args Pair) interface{} {
	bv := bytevectorConstraint(First(args))
	start := 0
	end := len(bv)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(bv) {
		return Err(fmt.Sprintf("utf8->string: invalid range [%d, %d) for bytevector of length %d", start, end, len(bv)), args)
	}
	return string(bv[start:end])
}

// primitiveStringToUtf8 encodes a string to a UTF-8 bytevector, with optional start and end (in characters).
func primitiveStringToUtf8(args Pair) interface{} {
	s := stringConstraint(First(args))
	runes := []rune(s)
	start := 0
	end := len(runes)
	if Second(args) != nil {
		start = indexConstraint(Second(args))
	}
	if Third(args) != nil {
		end = indexConstraint(Third(args))
	}
	if start < 0 || end < start || end > len(runes) {
		return Err(fmt.Sprintf("string->utf8: invalid range [%d, %d) for string of length %d", start, end, len(runes)), args)
	}
	return []uint8(string(runes[start:end]))
}
