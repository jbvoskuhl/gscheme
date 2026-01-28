package main

import (
	"fmt"
	"os"

	"gscheme/gscheme"
)

func main() {
	s := gscheme.New()
	s.LoadFiles(os.Args[1:])
	fmt.Println("gscheme - a lightweight Scheme interpreter")
	s.ReadEvalPrintLoop()
}
