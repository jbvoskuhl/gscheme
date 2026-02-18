package gscheme

import "math/big"

// NewRat creates a new *big.Rat from numerator and denominator.
func NewRat(a, b int64) *big.Rat {
	return new(big.Rat).SetFrac64(a, b)
}

// ToRat converts an int64 or *big.Rat to *big.Rat. Panics on other types.
func ToRat(x interface{}) *big.Rat {
	switch v := x.(type) {
	case *big.Rat:
		return v
	case int64:
		return new(big.Rat).SetInt64(v)
	default:
		return ToRat(Err("Expected exact number, instead got:", List(x)))
	}
}

// SimplifyRat returns int64 if the denominator is 1, otherwise returns the *big.Rat.
func SimplifyRat(r *big.Rat) interface{} {
	if r.IsInt() {
		return r.Num().Int64()
	}
	return r
}

// IsRat returns true if x is a *big.Rat.
func IsRat(x interface{}) bool {
	_, ok := x.(*big.Rat)
	return ok
}

// isExact returns true if x is an exact number (int64 or *big.Rat).
func isExact(x interface{}) bool {
	switch x.(type) {
	case int64, *big.Rat:
		return true
	default:
		return false
	}
}
