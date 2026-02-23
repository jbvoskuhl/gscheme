package gscheme

import (
	"errors"
	"io"
	"os"
	"strconv"
	"time"

	"golang.org/x/term"
)

// ErrInterrupt is returned by ReadLine when the user presses Ctrl-C.
var ErrInterrupt = errors.New("interrupt")

// Readline provides line editing with history for interactive terminal input.
type Readline struct {
	fd           int
	history      []string
	historyIndex int
}

// NewReadline returns a Readline if stdin is a terminal, nil otherwise.
func NewReadline() *Readline {
	fd := int(os.Stdin.Fd())
	if !term.IsTerminal(fd) {
		return nil
	}
	return &Readline{fd: fd}
}

// ReadLine reads a line from the terminal with editing and history support.
func (rl *Readline) ReadLine(prompt string) (string, error) {
	oldState, err := term.MakeRaw(rl.fd)
	if err != nil {
		return "", err
	}
	defer term.Restore(rl.fd, oldState)

	line := []rune{}
	pos := 0
	rl.historyIndex = len(rl.history)
	savedLine := ""

	writeStr := func(s string) {
		os.Stdout.WriteString(s)
	}

	redraw := func() {
		writeStr("\r" + prompt + string(line) + "\x1b[K")
		if pos < len(line) {
			writeStr("\x1b[" + strconv.Itoa(len(line)-pos) + "D")
		}
	}

	// flashMatch briefly highlights the matching ( when ) is typed.
	flashMatch := func() {
		matchPos := findMatchingParen(line, pos-1)
		if matchPos < 0 {
			return
		}
		// Redraw with the matching paren in reverse video
		writeStr("\r" + prompt)
		writeStr(string(line[:matchPos]))
		writeStr("\x1b[7m" + string(line[matchPos]) + "\x1b[0m")
		writeStr(string(line[matchPos+1:]) + "\x1b[K")
		if pos < len(line) {
			writeStr("\x1b[" + strconv.Itoa(len(line)-pos) + "D")
		}
		time.Sleep(200 * time.Millisecond)
		redraw()
	}

	writeStr(prompt)

	buf := make([]byte, 1)
	for {
		n, err := os.Stdin.Read(buf)
		if n == 0 || err != nil {
			return "", io.EOF
		}
		b := buf[0]

		switch {
		case b == 0x0D || b == 0x0A: // Enter
			writeStr("\r\n")
			result := string(line)
			if len(result) > 0 && (len(rl.history) == 0 || rl.history[len(rl.history)-1] != result) {
				rl.history = append(rl.history, result)
			}
			return result, nil

		case b == 0x04: // Ctrl-D
			if len(line) == 0 {
				writeStr("\r\n")
				return "", io.EOF
			}

		case b == 0x03: // Ctrl-C
			writeStr("^C\r\n")
			return "", ErrInterrupt

		case b == 0x7F || b == 0x08: // Backspace
			if pos > 0 {
				line = append(line[:pos-1], line[pos:]...)
				pos--
				redraw()
			}

		case b == 0x01: // Ctrl-A
			pos = 0
			redraw()

		case b == 0x05: // Ctrl-E
			pos = len(line)
			redraw()

		case b == 0x15: // Ctrl-U
			line = []rune{}
			pos = 0
			redraw()

		case b == 0x0B: // Ctrl-K
			line = line[:pos]
			redraw()

		case b == 0x1B: // Escape sequence
			var seq [2]byte
			os.Stdin.Read(seq[:1])
			if seq[0] != '[' {
				continue
			}
			os.Stdin.Read(seq[1:])
			switch seq[1] {
			case 'A': // Up arrow
				if rl.historyIndex > 0 {
					if rl.historyIndex == len(rl.history) {
						savedLine = string(line)
					}
					rl.historyIndex--
					line = []rune(rl.history[rl.historyIndex])
					pos = len(line)
					redraw()
				}
			case 'B': // Down arrow
				if rl.historyIndex < len(rl.history) {
					rl.historyIndex++
					if rl.historyIndex == len(rl.history) {
						line = []rune(savedLine)
					} else {
						line = []rune(rl.history[rl.historyIndex])
					}
					pos = len(line)
					redraw()
				}
			case 'C': // Right arrow
				if pos < len(line) {
					pos++
					writeStr("\x1b[C")
				}
			case 'D': // Left arrow
				if pos > 0 {
					pos--
					writeStr("\x1b[D")
				}
			case 'H': // Home
				pos = 0
				redraw()
			case 'F': // End
				pos = len(line)
				redraw()
			}

		default:
			if b >= 0x20 && b < 0x7F { // Printable ASCII
				ch := rune(b)
				if pos == len(line) {
					line = append(line, ch)
				} else {
					line = append(line[:pos+1], line[pos:]...)
					line[pos] = ch
				}
				pos++
				redraw()
				if ch == ')' {
					flashMatch()
				}
			}
		}
	}
}

// findMatchingParen scans backwards from closePos to find the matching '('.
// It respects nesting and skips characters inside string literals.
// Returns the index of the matching '(' or -1 if not found.
func findMatchingParen(line []rune, closePos int) int {
	depth := 1
	inString := false
	for i := closePos - 1; i >= 0; i-- {
		ch := line[i]
		if ch == '"' && (i == 0 || line[i-1] != '\\') {
			inString = !inString
			continue
		}
		if inString {
			continue
		}
		if ch == ')' {
			depth++
		} else if ch == '(' {
			depth--
			if depth == 0 {
				return i
			}
		}
	}
	return -1
}
