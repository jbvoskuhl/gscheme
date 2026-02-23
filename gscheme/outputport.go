package gscheme

import (
	"bufio"
	"bytes"
	"io"
	"strings"
)

// OutputPort is to Scheme as Writer is to Go.
// It provides buffered writing of Scheme output to an underlying stream.
type OutputPort struct {
	writer io.Writer
	out    *bufio.Writer
	closed bool
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

// WriteByte writes a single byte to the output port.
func (p *OutputPort) WriteByte(b byte) {
	p.out.WriteByte(b)
}

// Close flushes the buffer and closes the underlying writer if it implements io.Closer.
func (p *OutputPort) Close() error {
	p.closed = true
	p.out.Flush()
	if closer, ok := p.writer.(io.Closer); ok {
		return closer.Close()
	}
	return nil
}

// IsOpen returns true if the output port has not been closed.
func (p *OutputPort) IsOpen() bool {
	return !p.closed
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

// GetBytes returns the accumulated bytes if backed by a *bytes.Buffer.
// Returns (nil, false) if the underlying writer is not a *bytes.Buffer.
func (p *OutputPort) GetBytes() ([]uint8, bool) {
	p.out.Flush()
	if b, ok := p.writer.(*bytes.Buffer); ok {
		// Return a copy so the caller owns the slice
		result := make([]uint8, b.Len())
		copy(result, b.Bytes())
		return result, true
	}
	return nil, false
}

// NewOutputPortFromBuffer creates an OutputPort backed by a bytes.Buffer for bytevector ports.
func NewOutputPortFromBuffer(b *bytes.Buffer) *OutputPort {
	return &OutputPort{
		writer: b,
		out:    bufio.NewWriter(b),
	}
}

// String returns a human-readable representation of the output port.
func (p *OutputPort) String() string {
	return "#<output-port>"
}
