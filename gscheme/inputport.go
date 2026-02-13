package gscheme

import (
	"bufio"
	"io"
	"strconv"
	"strings"
	"unicode"
)

// namedCharacters maps R7RS-small character names to their rune values.
var namedCharacters = map[string]rune{
	"alarm":     '\x07',
	"backspace": '\x08',
	"delete":    '\x7F',
	"escape":    '\x1B',
	"newline":   '\n',
	"null":      '\x00',
	"return":    '\r',
	"space":     ' ',
	"tab":       '\t',
}

// EOF is the special object returned when end-of-file is reached.
var EOF = Symbol("#!EOF")

// InputPort is to Scheme as Reader is to Go.
// It provides tokenizing and parsing of Scheme expressions from an input stream.
type InputPort struct {
	reader        io.Reader
	in            *bufio.Reader
	isPushedToken bool
	isPushedChar  bool
	pushedToken   interface{}
	pushedChar    rune
	buff          strings.Builder
}

// NewInputPort creates an InputPort from an io.Reader.
func NewInputPort(r io.Reader) *InputPort {
	return &InputPort{
		reader: r,
		in:     bufio.NewReader(r),
	}
}

// NewInputPortFromString creates an InputPort from a string.
func NewInputPortFromString(s string) *InputPort {
	return NewInputPort(strings.NewReader(s))
}

// ReadChar reads and returns a Scheme character or EOF.
func (p *InputPort) ReadChar() interface{} {
	if p.isPushedChar {
		p.isPushedChar = false
		if p.pushedChar == -1 {
			return EOF
		}
		return p.pushedChar
	}
	ch, _, err := p.in.ReadRune()
	if err != nil {
		return EOF
	}
	return ch
}

// PeekChar peeks at and returns the next Scheme character (or EOF)
// without consuming it.
func (p *InputPort) PeekChar() interface{} {
	ch := p.peekCh()
	if ch == -1 {
		return EOF
	}
	return ch
}

// pushChar pushes a character back to be re-used later.
func (p *InputPort) pushChar(ch rune) rune {
	p.isPushedChar = true
	p.pushedChar = ch
	return ch
}

// popChar pops off the previously pushed character.
func (p *InputPort) popChar() rune {
	p.isPushedChar = false
	return p.pushedChar
}

// peekCh peeks at and returns the next character as a rune, -1 for EOF.
// Does not consume the character.
func (p *InputPort) peekCh() rune {
	if p.isPushedChar {
		return p.pushedChar
	}
	ch, _, err := p.in.ReadRune()
	if err != nil {
		return p.pushChar(-1)
	}
	return p.pushChar(ch)
}

// parseHexScalar parses a string of hex digits as a Unicode code point.
func parseHexScalar(hexDigits string) (rune, bool) {
	n, err := strconv.ParseInt(hexDigits, 16, 32)
	if err != nil {
		return 0, false
	}
	return rune(n), true
}

// readStringHexEscape reads hex digits until ';' for a \xHHHH; string escape.
func (p *InputPort) readStringHexEscape() rune {
	var hexBuf strings.Builder
	for {
		r, _, err := p.in.ReadRune()
		if err != nil {
			warn("EOF inside hex escape in string.")
			break
		}
		if r == ';' {
			break
		}
		hexBuf.WriteRune(r)
	}
	if ch, ok := parseHexScalar(hexBuf.String()); ok {
		return ch
	}
	warn("Invalid hex escape in string: \\x" + hexBuf.String() + ";")
	return unicode.ReplacementChar
}

// Read reads and returns a Scheme expression, or EOF.
func (p *InputPort) Read() interface{} {
	token := p.nextToken()
	switch token {
	case Symbol("("):
		return p.readTail(false)
	case Symbol(")"):
		warn("Extra ) ignored.")
		return p.Read()
	case Symbol("."):
		warn("Extra . ignored.")
		return p.Read()
	case Symbol("'"):
		return List(Symbol("quote"), p.Read())
	case Symbol("`"):
		return List(Symbol("quasiquote"), p.Read())
	case Symbol(","):
		return List(Symbol("unquote"), p.Read())
	case Symbol(",@"):
		return List(Symbol("unquote-splicing"), p.Read())
	default:
		return token
	}
}

// Close closes the input port.
func (p *InputPort) Close() error {
	if closer, ok := p.reader.(io.Closer); ok {
		return closer.Close()
	}
	return nil
}

// IsEOF checks if the argument is the EOF object.
func IsEOF(x interface{}) bool {
	return x == EOF
}

// readTail recursively builds list structures.
func (p *InputPort) readTail(dotOK bool) interface{} {
	token := p.nextToken()
	if token == EOF {
		return Err("EOF during read.", nil)
	}
	if token == Symbol(")") {
		return nil
	}
	if token == Symbol(".") {
		result := p.Read()
		token = p.nextToken()
		if token != Symbol(")") {
			warn("Where's the ')'? Got " + Stringify(token) + " after .")
		}
		return result
	}
	// Push back the token and read the expression
	p.isPushedToken = true
	p.pushedToken = token
	return Cons(p.Read(), p.readTail(true))
}

// nextToken is the tokenizer that returns the next token from the input.
func (p *InputPort) nextToken() interface{} {
	var ch rune

	// See if we should re-use a pushed char or token
	if p.isPushedToken {
		p.isPushedToken = false
		return p.pushedToken
	} else if p.isPushedChar {
		ch = p.popChar()
	} else {
		r, _, err := p.in.ReadRune()
		if err != nil {
			return EOF
		}
		ch = r
	}

	// Skip whitespace
	for unicode.IsSpace(ch) {
		r, _, err := p.in.ReadRune()
		if err != nil {
			return EOF
		}
		ch = r
	}

	// See what kind of non-white character we got
	switch ch {
	case -1:
		return EOF
	case '(':
		return Symbol("(")
	case ')':
		return Symbol(")")
	case '\'':
		return Symbol("'")
	case '`':
		return Symbol("`")
	case ',':
		r, _, err := p.in.ReadRune()
		if err != nil {
			return Symbol(",")
		}
		if r == '@' {
			return Symbol(",@")
		}
		p.pushChar(r)
		return Symbol(",")
	case ';':
		// Comment: skip to end of line and then read next token
		for ch != -1 && ch != '\n' && ch != '\r' {
			r, _, err := p.in.ReadRune()
			if err != nil {
				return EOF
			}
			ch = r
		}
		return p.nextToken()
	case '"':
		// Strings are represented as Go strings
		p.buff.Reset()
		for {
			r, _, err := p.in.ReadRune()
			if err != nil || r == '"' {
				if err != nil {
					warn("EOF inside of a string.")
				}
				return p.buff.String()
			}
			if r == '\\' {
				// Handle escape sequence
				escaped, _, err := p.in.ReadRune()
				if err != nil {
					warn("EOF inside of a string.")
					return p.buff.String()
				}
				switch escaped {
				case 'n':
					p.buff.WriteRune('\n')
				case 't':
					p.buff.WriteRune('\t')
				case 'r':
					p.buff.WriteRune('\r')
				case 'a':
					p.buff.WriteRune('\x07')
				case 'b':
					p.buff.WriteRune('\x08')
				case '\\':
					p.buff.WriteRune('\\')
				case '"':
					p.buff.WriteRune('"')
				case '|':
					p.buff.WriteRune('|')
				case 'x':
					ch := p.readStringHexEscape()
					p.buff.WriteRune(ch)
				default:
					p.buff.WriteRune(escaped)
				}
			} else {
				p.buff.WriteRune(r)
			}
		}
	case '#':
		r, _, err := p.in.ReadRune()
		if err != nil {
			return EOF
		}
		switch r {
		case 't', 'T':
			return true
		case 'f', 'F':
			return false
		case '(':
			p.pushChar('(')
			return listToVector(p.Read())
		case '\\':
			// Character literal
			r, _, err := p.in.ReadRune()
			if err != nil {
				return EOF
			}
			if unicode.IsLetter(r) {
				// Accumulate a full word
				p.buff.Reset()
				p.buff.WriteRune(r)
				for {
					next := p.peekCh()
					if next == -1 || unicode.IsSpace(next) || next == '(' || next == ')' ||
						next == '\'' || next == ';' || next == '"' || next == ',' || next == '`' {
						break
					}
					p.popChar()
					p.buff.WriteRune(next)
				}
				word := p.buff.String()
				if len(word) == 1 {
					return r
				}
				lower := strings.ToLower(word)
				if ch, ok := namedCharacters[lower]; ok {
					return ch
				}
				if lower[0] == 'x' && len(lower) > 1 {
					if ch, ok := parseHexScalar(lower[1:]); ok {
						return ch
					}
				}
				warn("Unknown character name: " + word)
				return r
			}
			return r
		case 'e', 'i', 'd':
			// Exact/inexact/decimal prefix - ignore and read next token
			return p.nextToken()
		case 'b', 'o', 'x':
			warn("#" + string(r) + " not implemented, ignored.")
			return p.nextToken()
		default:
			warn("#" + string(r) + " not recognized, ignored.")
			return p.nextToken()
		}
	default:
		// Symbol or number
		p.buff.Reset()
		c := ch
		for {
			p.buff.WriteRune(ch)
			r, _, err := p.in.ReadRune()
			if err != nil {
				ch = -1
				break
			}
			ch = r
			if unicode.IsSpace(ch) || ch == '(' || ch == ')' || ch == '\'' ||
				ch == ';' || ch == '"' || ch == ',' || ch == '`' {
				break
			}
		}
		p.pushChar(ch)
		s := p.buff.String()

		// Try to parse as a number if it starts with a digit, +, -, or .
		if c == '.' || c == '+' || c == '-' || (c >= '0' && c <= '9') {
			if num := parseNumber(s); num != nil {
				return num
			}
			// Starts with a digit but isn't a valid number — error per R7RS
			if c >= '0' && c <= '9' {
				return NewError("Invalid number literal: "+s, nil)
			}
		}
		// R7RS: identifiers are case-insensitive, fold to lowercase.
		return Symbol(strings.ToLower(s))
	}
}

// listToVector converts a list to a vector (Go slice).
func listToVector(list interface{}) []interface{} {
	var result []interface{}
	for pair, ok := list.(Pair); ok; pair, ok = list.(Pair) {
		result = append(result, pair.First())
		list = pair.Rest()
	}
	return result
}

// parseNumber attempts to parse a string as a number (real or complex).
// Returns nil if the string is not a valid number.
// Per R7RS, complex literals require an explicit sign before the imaginary part:
// +i, -i, +<num>i, -<num>i, <real>+<num>i, <real>-<num>i.
func parseNumber(s string) interface{} {
	s = strings.ToLower(s)

	// Check for pure imaginary: +i, -i (bare "i" is a symbol per R7RS)
	if s == "+i" {
		return complex(0, 1)
	}
	if s == "-i" {
		return complex(0, -1)
	}

	// Check if it ends with 'i' (complex number)
	if strings.HasSuffix(s, "i") {
		s = s[:len(s)-1] // Remove trailing 'i'

		// Find the last + or - that's not at the start and not part of exponent.
		// R7RS requires this sign — without it the imaginary part is invalid.
		splitIdx := -1
		for i := len(s) - 1; i > 0; i-- {
			if (s[i] == '+' || s[i] == '-') && s[i-1] != 'e' && s[i-1] != 'E' {
				splitIdx = i
				break
			}
		}

		if splitIdx == -1 {
			// No sign found separating real from imaginary.
			// Only valid forms here are "+<num>i" or "-<num>i" (pure imaginary with sign).
			if s == "+" {
				return complex(0, 1)
			}
			if s == "-" {
				return complex(0, -1)
			}
			// Must start with + or - to be a valid pure imaginary
			if len(s) > 0 && (s[0] == '+' || s[0] == '-') {
				if imag, err := strconv.ParseFloat(s, 64); err == nil {
					return complex(0, imag)
				}
			}
			return nil
		}

		// Complex with real and imaginary parts like "3+4i" or "3-4i"
		realStr := s[:splitIdx]
		imagStr := s[splitIdx:]

		realPart, err := strconv.ParseFloat(realStr, 64)
		if err != nil {
			return nil
		}

		var imagPart float64
		if imagStr == "+" {
			imagPart = 1
		} else if imagStr == "-" {
			imagPart = -1
		} else {
			imagPart, err = strconv.ParseFloat(imagStr, 64)
			if err != nil {
				return nil
			}
		}

		return complex(realPart, imagPart)
	}

	// Try to parse as an integer first (no '.', 'e', or 'E')
	if !strings.ContainsAny(s, ".eE") {
		if n, err := strconv.ParseInt(s, 10, 64); err == nil {
			return n
		}
	}

	// Try to parse as a regular float
	if f, err := strconv.ParseFloat(s, 64); err == nil {
		return f
	}

	return nil
}

// warn prints a warning message. In a full implementation, this would
// go to an error port.
func warn(message string) {
	// For now, just ignore warnings. In a full implementation,
	// this would print to stderr or an error port.
}
