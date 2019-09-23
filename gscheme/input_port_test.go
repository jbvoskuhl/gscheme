package gscheme

import (
     "testing"
)

func TestBoolean(t *testing.T) {
     port := NewInputPortString("#t #true #f #false")
     if port.read() != true {
          t.Errorf("The text #t did not parse as true.")
     }
     if port.read() != true {
          t.Errorf("The text #true did not parse as true.")
     }
     if port.read() != false {
          t.Errorf("The text #f did not parse as false.")
     }
     if port.read() != false {
          t.Errorf("The text #false did not parse as false.")
     }
     /*
     if port.read() != parser.GSchemeParserEOF {
          t.Errorf("The final expression was not an EOF object.")
     }
     */
}

func TestCharacter(t *testing.T) {
     port := NewInputPortString("#\\a")
     if port.read() != 'a' {
          t.Errorf("The text #\a did not parse as 'a'.")
     }
}

func TestList(t *testing.T) {
     port := NewInputPortString("(() () ())")
     list := port.read()
     if Len(list) != 3 {
          t.Errorf("The length of (() () ()) is not 3.")
     }
}