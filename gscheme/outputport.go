package gscheme

import (
	"bufio"
	"io"
	"strings"
)

// OutputPort is to Scheme as Writer is to Go.
// It provides buffered writing of Scheme output to an underlying stream.
type OutputPort struct {
	writer io.Writer
	out    *bufio.Writer
}

// NewOutputPort creates an OutputPort from an io.Writer.
func NewOutputPort(w io.Writer) *OutputPort {
	return &OutputPort{
		writer: w,
		out:    bufio.NewWriter(w),
	}
}

// NewOutputPortFromBuilder creates an OutputPort backed by a strings.Builder for string ports.
func NewOutputPortFromBuilder(b *strings.Builder) *OutputPort {
	return &OutputPort{
		writer: b,
		out:    bufio.NewWriter(b),
	}
}

// WriteString writes a raw string to the output port.
func (p *OutputPort) WriteString(s string) {
	p.out.WriteString(s)
}

// WriteRune writes a single character to the output port.
func (p *OutputPort) WriteRune(ch rune) {
	p.out.WriteRune(ch)
}

// Newline writes a newline character to the output port.
func (p *OutputPort) Newline() {
	p.out.WriteRune('\n')
}

// Flush flushes the buffered output.
func (p *OutputPort) Flush() {
	p.out.Flush()
}

// Close flushes the buffer and closes the underlying writer if it implements io.Closer.
func (p *OutputPort) Close() error {
	p.out.Flush()
	if closer, ok := p.writer.(io.Closer); ok {
		return closer.Close()
	}
	return nil
}

// GetString returns the accumulated string if backed by a *strings.Builder.
// Returns ("", false) if the underlying writer is not a *strings.Builder.
func (p *OutputPort) GetString() (string, bool) {
	p.out.Flush()
	if b, ok := p.writer.(*strings.Builder); ok {
		return b.String(), true
	}
	return "", false
}

// String returns a human-readable representation of the output port.
func (p *OutputPort) String() string {
	return "#<output-port>"
}
