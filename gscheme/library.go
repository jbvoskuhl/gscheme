package gscheme

import (
	"fmt"
	"strings"
)

// LibraryName is an ordered sequence of symbols and integers that uniquely
// identifies a library, e.g. ["scheme" "base"] or ["my" "app" 1].
type LibraryName []interface{}

func (n LibraryName) String() string {
	parts := make([]string, len(n))
	for i, p := range n {
		parts[i] = fmt.Sprint(p)
	}
	return "(" + strings.Join(parts, " ") + ")"
}

// libraryNameKey returns a canonical string key for use in the registry map.
func libraryNameKey(name LibraryName) string {
	parts := make([]string, len(name))
	for i, p := range name {
		parts[i] = fmt.Sprint(p)
	}
	return strings.Join(parts, "/")
}

// parseLibraryName converts a Scheme pair like (scheme base) to a LibraryName.
// Returns (nil, false) if the pair contains anything other than symbols or integers.
func parseLibraryName(x interface{}) (LibraryName, bool) {
	p, ok := x.(Pair)
	if !ok || p == nil {
		return nil, false
	}
	var parts []interface{}
	for p != nil {
		el := First(p)
		switch v := el.(type) {
		case Symbol:
			parts = append(parts, v)
		case int64:
			parts = append(parts, v)
		default:
			return nil, false
		}
		p = RestPair(p)
	}
	if len(parts) == 0 {
		return nil, false
	}
	return LibraryName(parts), true
}

// Library is a defined module with an isolated environment and a declared set
// of exported names. Only exported names are visible to importers.
type Library struct {
	name    LibraryName
	env     Environment       // private library environment
	exports map[Symbol]Symbol // external-name → internal-name
}

// Name returns the library's name.
func (l *Library) Name() LibraryName { return l.name }

// exportedBindings returns a snapshot map of external-name → value for all
// exports whose internal name is bound in the library environment.
func (l *Library) exportedBindings() map[Symbol]interface{} {
	result := make(map[Symbol]interface{})
	for externalName, internalName := range l.exports {
		if val, ok := l.env.Lookup(internalName); ok {
			result[externalName] = val
		}
	}
	return result
}

// LibraryRegistry maps canonical library name keys to Library objects.
// Each Scheme interpreter instance holds its own registry.
type LibraryRegistry map[string]*Library

// Register adds (or replaces) a library in the registry.
func (r LibraryRegistry) Register(lib *Library) {
	r[libraryNameKey(lib.name)] = lib
}

// Lookup retrieves a library by name, returning (nil, false) if not found.
func (r LibraryRegistry) Lookup(name LibraryName) (*Library, bool) {
	lib, ok := r[libraryNameKey(name)]
	return lib, ok
}
