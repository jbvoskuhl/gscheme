package gscheme

import (
	"errors"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"
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
	Completer    func(prefix string) []string
}

// NewReadline returns a Readline if stdin is a terminal, nil otherwise.
func NewReadline() *Readline {
	fd := int(os.Stdin.Fd())
	if !term.IsTerminal(fd) {
		return nil
	}
	return &Readline{fd: fd}
}

// multilineBuffer manages a multi-line editing buffer.
type multilineBuffer struct {
	lines [][]rune
	row   int
	col   int
}

func newMultilineBuffer() *multilineBuffer {
	return &multilineBuffer{
		lines: [][]rune{{}},
	}
}

func (mb *multilineBuffer) currentLine() []rune {
	return mb.lines[mb.row]
}

// joinedText returns the full text with newlines between lines.
func (mb *multilineBuffer) joinedText() string {
	parts := make([]string, len(mb.lines))
	for i, line := range mb.lines {
		parts[i] = string(line)
	}
	return strings.Join(parts, "\n")
}

// flatten concatenates all lines with newlines for paren matching.
func (mb *multilineBuffer) flatten() []rune {
	var result []rune
	for i, line := range mb.lines {
		if i > 0 {
			result = append(result, '\n')
		}
		result = append(result, line...)
	}
	return result
}

// flatIndex returns the flat index corresponding to the current (row, col).
func (mb *multilineBuffer) flatIndex() int {
	idx := 0
	for i := 0; i < mb.row; i++ {
		idx += len(mb.lines[i]) + 1 // +1 for newline
	}
	idx += mb.col
	return idx
}

// flatToRowCol converts a flat index back to (row, col).
func (mb *multilineBuffer) flatToRowCol(flatIdx int) (int, int) {
	idx := 0
	for i, line := range mb.lines {
		lineLen := len(line)
		if i < len(mb.lines)-1 {
			lineLen++ // account for newline
		}
		if idx+lineLen > flatIdx {
			return i, flatIdx - idx
		}
		idx += lineLen
	}
	// Past end — return last position
	last := len(mb.lines) - 1
	return last, len(mb.lines[last])
}

// insertNewline splits the current line at the cursor, creating a new line below.
func (mb *multilineBuffer) insertNewline() {
	cur := mb.lines[mb.row]
	before := make([]rune, mb.col)
	copy(before, cur[:mb.col])
	after := make([]rune, len(cur)-mb.col)
	copy(after, cur[mb.col:])

	// Replace current line with before, insert after as new line
	mb.lines[mb.row] = before
	newLines := make([][]rune, len(mb.lines)+1)
	copy(newLines, mb.lines[:mb.row+1])
	newLines[mb.row+1] = after
	copy(newLines[mb.row+2:], mb.lines[mb.row+1:])
	mb.lines = newLines
	mb.row++
	mb.col = 0
}

// loadText replaces the buffer contents with the given text (may contain newlines).
func (mb *multilineBuffer) loadText(text string) {
	parts := strings.Split(text, "\n")
	mb.lines = make([][]rune, len(parts))
	for i, p := range parts {
		mb.lines[i] = []rune(p)
	}
	mb.row = len(mb.lines) - 1
	mb.col = len(mb.lines[mb.row])
}

// prompt returns the appropriate prompt for the given line index.
func prompt(primary, continuation string, lineIdx int) string {
	if lineIdx == 0 {
		return primary
	}
	return continuation
}

// ReadLine reads a line from the terminal with multi-line editing and history support.
// When accumulated is non-empty, the buffer is pre-loaded for continued editing.
func (rl *Readline) ReadLine(primaryPrompt, continuationPrompt, accumulated string) (string, error) {
	oldState, err := term.MakeRaw(rl.fd)
	if err != nil {
		return "", err
	}
	defer term.Restore(rl.fd, oldState)

	buf := newMultilineBuffer()
	if accumulated != "" {
		buf.loadText(accumulated)
	}

	rl.historyIndex = len(rl.history)
	savedText := ""
	displayRows := 0 // how many rows we've drawn (0-indexed from first line)

	writeStr := func(s string) {
		os.Stdout.WriteString(s)
	}

	// redrawAll clears all displayed lines and redraws the entire buffer.
	redrawAll := func() {
		// Move cursor up to first displayed line
		if displayRows > 0 {
			writeStr("\x1b[" + strconv.Itoa(displayRows) + "A")
		}

		// Redraw each line
		for i, line := range buf.lines {
			p := prompt(primaryPrompt, continuationPrompt, i)
			writeStr("\r" + p + string(line) + "\x1b[K")
			if i < len(buf.lines)-1 {
				writeStr("\r\n")
			}
		}

		displayRows = len(buf.lines) - 1

		// Move cursor to the correct row
		if buf.row < len(buf.lines)-1 {
			up := len(buf.lines) - 1 - buf.row
			writeStr("\x1b[" + strconv.Itoa(up) + "A")
		}

		// Move cursor to the correct column
		p := prompt(primaryPrompt, continuationPrompt, buf.row)
		cursorCol := len(p) + buf.col
		writeStr("\r")
		if cursorCol > 0 {
			writeStr("\x1b[" + strconv.Itoa(cursorCol) + "C")
		}
	}

	// flashMatch briefly highlights the matching ( when ) is typed.
	flashMatch := func() {
		flat := buf.flatten()
		flatIdx := buf.flatIndex() - 1 // position of the ) just typed
		matchIdx := findMatchingParen(flat, flatIdx)
		if matchIdx < 0 {
			return
		}
		matchRow, matchCol := buf.flatToRowCol(matchIdx)

		// Move cursor to the matching paren's row
		if matchRow != buf.row {
			if matchRow < buf.row {
				writeStr("\x1b[" + strconv.Itoa(buf.row-matchRow) + "A")
			} else {
				writeStr("\x1b[" + strconv.Itoa(matchRow-buf.row) + "B")
			}
		}

		// Redraw that line with highlight
		matchP := prompt(primaryPrompt, continuationPrompt, matchRow)
		matchLine := buf.lines[matchRow]
		writeStr("\r" + matchP)
		writeStr(string(matchLine[:matchCol]))
		writeStr("\x1b[7m" + string(matchLine[matchCol]) + "\x1b[0m")
		writeStr(string(matchLine[matchCol+1:]) + "\x1b[K")

		// Move cursor back to where it was (current row, current col)
		if matchRow != buf.row {
			if matchRow < buf.row {
				writeStr("\x1b[" + strconv.Itoa(buf.row-matchRow) + "B")
			} else {
				writeStr("\x1b[" + strconv.Itoa(matchRow-buf.row) + "A")
			}
		}
		p := prompt(primaryPrompt, continuationPrompt, buf.row)
		cursorCol := len(p) + buf.col
		writeStr("\r")
		if cursorCol > 0 {
			writeStr("\x1b[" + strconv.Itoa(cursorCol) + "C")
		}

		time.Sleep(200 * time.Millisecond)
		redrawAll()
	}

	// Draw initial state
	redrawAll()

	rawBuf := make([]byte, 1)
	for {
		n, err := os.Stdin.Read(rawBuf)
		if n == 0 || err != nil {
			return "", io.EOF
		}
		b := rawBuf[0]

		switch {
		case b == 0x0D: // Enter — submit
			// Move cursor to end of last line, then newline
			if buf.row < len(buf.lines)-1 {
				writeStr("\x1b[" + strconv.Itoa(len(buf.lines)-1-buf.row) + "B")
			}
			writeStr("\r\n")
			result := buf.joinedText()
			if len(result) > 0 && (len(rl.history) == 0 || rl.history[len(rl.history)-1] != result) {
				rl.history = append(rl.history, result)
			}
			return result, nil

		case b == 0x0A: // Ctrl-J — insert newline
			buf.insertNewline()
			redrawAll()

		case b == 0x04: // Ctrl-D
			if len(buf.lines) == 1 && len(buf.lines[0]) == 0 {
				writeStr("\r\n")
				return "", io.EOF
			}

		case b == 0x03: // Ctrl-C
			// Move to end of display before printing ^C
			if buf.row < len(buf.lines)-1 {
				writeStr("\x1b[" + strconv.Itoa(len(buf.lines)-1-buf.row) + "B")
			}
			writeStr("^C\r\n")
			return "", ErrInterrupt

		case b == 0x7F || b == 0x08: // Backspace
			if buf.col > 0 {
				line := buf.lines[buf.row]
				buf.lines[buf.row] = append(line[:buf.col-1], line[buf.col:]...)
				buf.col--
				redrawAll()
			} else if buf.row > 0 {
				// Join current line onto end of previous line
				prev := buf.lines[buf.row-1]
				cur := buf.lines[buf.row]
				newCol := len(prev)
				buf.lines[buf.row-1] = append(prev, cur...)
				buf.lines = append(buf.lines[:buf.row], buf.lines[buf.row+1:]...)
				buf.row--
				buf.col = newCol
				redrawAll()
			}

		case b == 0x01: // Ctrl-A
			buf.col = 0
			redrawAll()

		case b == 0x05: // Ctrl-E
			buf.col = len(buf.currentLine())
			redrawAll()

		case b == 0x15: // Ctrl-U — kill line before cursor
			buf.lines[buf.row] = buf.lines[buf.row][buf.col:]
			buf.col = 0
			redrawAll()

		case b == 0x0B: // Ctrl-K — kill line after cursor
			buf.lines[buf.row] = buf.lines[buf.row][:buf.col]
			redrawAll()

		case b == 0x0C: // Ctrl-L — clear screen
			writeStr("\x1b[2J\x1b[H")
			redrawAll()

		case b == 0x09: // Tab — completion
			if rl.Completer == nil {
				continue
			}
			// Extract prefix: scan backwards from cursor to a delimiter
			line := buf.currentLine()
			start := buf.col
			for start > 0 && !isSchemeDelimiter(line[start-1]) {
				start--
			}
			if start == buf.col {
				continue // nothing to complete
			}
			prefix := string(line[start:buf.col])
			candidates := rl.Completer(prefix)
			if len(candidates) == 0 {
				continue
			}
			if len(candidates) == 1 {
				// Insert remaining characters
				suffix := []rune(candidates[0][len(prefix):])
				newLine := make([]rune, 0, len(line)+len(suffix))
				newLine = append(newLine, line[:buf.col]...)
				newLine = append(newLine, suffix...)
				newLine = append(newLine, line[buf.col:]...)
				buf.lines[buf.row] = newLine
				buf.col += len(suffix)
				redrawAll()
			} else {
				// Insert longest common prefix
				lcp := longestCommonPrefix(candidates)
				if len(lcp) > len(prefix) {
					suffix := []rune(lcp[len(prefix):])
					newLine := make([]rune, 0, len(line)+len(suffix))
					newLine = append(newLine, line[:buf.col]...)
					newLine = append(newLine, suffix...)
					newLine = append(newLine, line[buf.col:]...)
					buf.lines[buf.row] = newLine
					buf.col += len(suffix)
				}
				// Display candidates below input
				// Move to end of display
				if buf.row < len(buf.lines)-1 {
					writeStr("\x1b[" + strconv.Itoa(len(buf.lines)-1-buf.row) + "B")
				}
				writeStr("\r\n" + strings.Join(candidates, "  ") + "\r\n")
				displayRows = 0
				redrawAll()
			}

		case b == 0x1B: // Escape sequence
			var seq [2]byte
			os.Stdin.Read(seq[:1])
			if seq[0] != '[' {
				// Alt-key combos: ESC + letter
				switch seq[0] {
				case 'f', 'F': // Alt-F — word forward
					line := buf.currentLine()
					p := buf.col
					// Skip non-delimiters (current word)
					for p < len(line) && !isSchemeDelimiter(line[p]) {
						p++
					}
					// Skip delimiters
					for p < len(line) && isSchemeDelimiter(line[p]) {
						p++
					}
					buf.col = p
					redrawAll()
				case 'b', 'B': // Alt-B — word backward
					line := buf.currentLine()
					p := buf.col
					// Skip delimiters
					for p > 0 && isSchemeDelimiter(line[p-1]) {
						p--
					}
					// Skip non-delimiters (current word)
					for p > 0 && !isSchemeDelimiter(line[p-1]) {
						p--
					}
					buf.col = p
					redrawAll()
				}
				continue
			}
			os.Stdin.Read(seq[1:])
			switch seq[1] {
			case 'A': // Up arrow
				if buf.row > 0 {
					buf.row--
					if buf.col > len(buf.lines[buf.row]) {
						buf.col = len(buf.lines[buf.row])
					}
					redrawAll()
				} else {
					// History navigation
					if rl.historyIndex > 0 {
						if rl.historyIndex == len(rl.history) {
							savedText = buf.joinedText()
						}
						rl.historyIndex--
						buf.loadText(rl.history[rl.historyIndex])
						redrawAll()
					}
				}
			case 'B': // Down arrow
				if buf.row < len(buf.lines)-1 {
					buf.row++
					if buf.col > len(buf.lines[buf.row]) {
						buf.col = len(buf.lines[buf.row])
					}
					redrawAll()
				} else {
					// History navigation
					if rl.historyIndex < len(rl.history) {
						rl.historyIndex++
						if rl.historyIndex == len(rl.history) {
							buf.loadText(savedText)
						} else {
							buf.loadText(rl.history[rl.historyIndex])
						}
						redrawAll()
					}
				}
			case 'C': // Right arrow
				if buf.col < len(buf.currentLine()) {
					buf.col++
					redrawAll()
				}
			case 'D': // Left arrow
				if buf.col > 0 {
					buf.col--
					redrawAll()
				}
			case 'H': // Home
				buf.col = 0
				redrawAll()
			case 'F': // End
				buf.col = len(buf.currentLine())
				redrawAll()
			}

		default:
			if b >= 0x20 && b < 0x7F { // Printable ASCII
				ch := rune(b)
				line := buf.currentLine()
				if buf.col == len(line) {
					buf.lines[buf.row] = append(line, ch)
				} else {
					newLine := make([]rune, len(line)+1)
					copy(newLine, line[:buf.col])
					newLine[buf.col] = ch
					copy(newLine[buf.col+1:], line[buf.col:])
					buf.lines[buf.row] = newLine
				}
				buf.col++
				redrawAll()
				if ch == ')' {
					flashMatch()
				}
			}
		}
	}
}

// isSchemeDelimiter returns true for characters that separate tokens in Scheme.
func isSchemeDelimiter(ch rune) bool {
	switch ch {
	case ' ', '\t', '(', ')', '\'', ';', '"', ',', '`':
		return true
	}
	return false
}

// wordForward returns the position after skipping to the next word boundary.
func wordForward(line []rune, pos int) int {
	// Skip non-delimiters
	for pos < len(line) && !isSchemeDelimiter(line[pos]) {
		pos++
	}
	// Skip delimiters
	for pos < len(line) && isSchemeDelimiter(line[pos]) {
		pos++
	}
	return pos
}

// wordBackward returns the position after skipping to the previous word boundary.
func wordBackward(line []rune, pos int) int {
	// Skip delimiters
	for pos > 0 && isSchemeDelimiter(line[pos-1]) {
		pos--
	}
	// Skip non-delimiters
	for pos > 0 && !isSchemeDelimiter(line[pos-1]) {
		pos--
	}
	return pos
}

// longestCommonPrefix finds the longest common prefix of the given strings.
func longestCommonPrefix(strs []string) string {
	if len(strs) == 0 {
		return ""
	}
	prefix := strs[0]
	for _, s := range strs[1:] {
		for i := 0; i < len(prefix); i++ {
			if i >= len(s) || prefix[i] != s[i] {
				prefix = prefix[:i]
				break
			}
		}
	}
	return prefix
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

// MakeSymbolCompleter creates a completion function from an environment's symbols.
func MakeSymbolCompleter(env Environment) func(string) []string {
	return func(prefix string) []string {
		symbols := env.Symbols()
		var matches []string
		for _, sym := range symbols {
			name := string(sym)
			if strings.HasPrefix(name, prefix) {
				matches = append(matches, name)
			}
		}
		sort.Strings(matches)
		return matches
	}
}
