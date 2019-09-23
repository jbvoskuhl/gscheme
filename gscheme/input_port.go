package gscheme

import (
	"github.com/antlr/antlr4/runtime/Go/antlr"
	"github.com/jbvoskuhl/gscheme/gscheme/parser"
)

type InputPort interface {
	read() interface{}
}

type inputPort struct {
	*parser.GSchemeParser
}

func NewInputPortString(source string) InputPort {
	input := antlr.NewInputStream(source)
	lexer := parser.NewGSchemeLexer(input)
	stream := antlr.NewCommonTokenStream(lexer, 0)
	return &inputPort{parser.NewGSchemeParser(stream)}
}

func (input *inputPort) read() interface{} {
	// input.AddParseListener(input)
	return translate(input.Expression())
}

func translate(expression antlr.Tree) interface{} {
	if expression == nil {
		return nil
	}
	switch rule := expression.(type) {
	case parser.IBooleanContext: return translateBoolean(rule)
	case parser.ICharacterContext: return translateCharacter(rule)
	case parser.IListContext: return translateList(rule)
	case *parser.ListExpressionContext: return translateList(rule.List())
	default: panic("GScheme parser bug: Found an unknown expression.")
	}
}

func translateBoolean(expression parser.IBooleanContext) bool {
	switch expression.GetText() {
	case "#t", "#true": return true
	case "#f", "#false": return false
	default: panic("GScheme parser bug: Found a boolean without proper text.")
	}
}

func translateCharacter(expression parser.ICharacterContext) rune {
	text := expression.GetText()
	text = text[2:]
	if len(text) == 1 {
		return rune(text[0])
	} else if text == "alarm" {
		return '\a'
	} else {
		return '1'
	}
}

func translateList(expression parser.IListContext) Pair {
	children := expression.GetChildren()
	elements := make([]interface{}, 0, len(children))
	for _, child := range children {
		_, ok := child.(parser.IExpressionContext)
		if ok {
			elements = append(elements, translate(child))
		}
	}
	return List(elements ...)
}

func (input *inputPort) EnterEveryRule(ctx antlr.ParserRuleContext) {
}

func (input *inputPort) ExitEveryRule(ctx antlr.ParserRuleContext) {
}

func (input *inputPort)  VisitErrorNode(node antlr.ErrorNode) {
}

func (input *inputPort) VisitTerminal(node antlr.TerminalNode) {
}

