package gscheme

import (
	"math"
	"math/big"
	"math/bits"
)

// ToBigInt converts an int64 or *big.Int to *big.Int.
func ToBigInt(x interface{}) *big.Int {
	switch v := x.(type) {
	case *big.Int:
		return v
	case int64:
		return big.NewInt(v)
	default:
		return ToBigInt(Err("Expected exact integer, instead got:", List(x)))
	}
}

// SimplifyBigInt returns int64 if z fits in int64 range, otherwise returns *big.Int.
func SimplifyBigInt(z *big.Int) interface{} {
	if z.IsInt64() {
		return z.Int64()
	}
	return z
}

// IsBigInt returns true if x is a *big.Int.
func IsBigInt(x interface{}) bool {
	_, ok := x.(*big.Int)
	return ok
}

// isExactInt returns true if x is an exact integer (int64 or *big.Int).
func isExactInt(x interface{}) bool {
	switch x.(type) {
	case int64, *big.Int:
		return true
	default:
		return false
	}
}

// addInt64 adds two int64 values, returning int64 on no overflow or *big.Int on overflow.
func addInt64(a, b int64) interface{} {
	sum, carry := bits.Add64(uint64(a), uint64(b), 0)
	// Overflow occurs when the sign bits of a and b are the same but differ from sum's sign bit.
	// Equivalently: carry into MSB != carry out of MSB.
	signA := uint64(a) >> 63
	signB := uint64(b) >> 63
	signSum := sum >> 63
	if (signA == signB) && (signA != signSum) {
		_ = carry
		result := new(big.Int).Add(big.NewInt(a), big.NewInt(b))
		return result
	}
	return int64(sum)
}

// subInt64 subtracts two int64 values, returning int64 on no overflow or *big.Int on overflow.
func subInt64(a, b int64) interface{} {
	diff := uint64(a) - uint64(b)
	// Overflow occurs when signs of a and b differ and result sign differs from a's sign.
	signA := uint64(a) >> 63
	signB := uint64(b) >> 63
	signDiff := diff >> 63
	if (signA != signB) && (signA != signDiff) {
		result := new(big.Int).Sub(big.NewInt(a), big.NewInt(b))
		return result
	}
	return int64(diff)
}

// mulInt64 multiplies two int64 values, returning int64 on no overflow or *big.Int on overflow.
func mulInt64(a, b int64) interface{} {
	// Handle special cases
	if a == 0 || b == 0 {
		return int64(0)
	}
	if a == 1 {
		return b
	}
	if b == 1 {
		return a
	}
	// Check for MinInt64 * -1 overflow
	if a == math.MinInt64 && b == -1 || a == -1 && b == math.MinInt64 {
		result := new(big.Int).Mul(big.NewInt(a), big.NewInt(b))
		return result
	}
	product := a * b
	// Verify: if product/a != b, overflow occurred
	if product/a != b {
		result := new(big.Int).Mul(big.NewInt(a), big.NewInt(b))
		return result
	}
	return product
}

// negInt64 negates an int64, returning int64 on no overflow or *big.Int on overflow.
func negInt64(a int64) interface{} {
	if a == math.MinInt64 {
		result := new(big.Int).Neg(big.NewInt(a))
		return result
	}
	return -a
}
