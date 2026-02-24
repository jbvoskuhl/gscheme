package gscheme

import (
	"path/filepath"
	"testing"
)

func TestSchemeTestSuite(t *testing.T) {
	files, err := filepath.Glob("testdata/*.scm")
	if err != nil {
		t.Fatalf("Failed to glob test files: %v", err)
	}
	for _, f := range files {
		t.Run(filepath.Base(f), func(t *testing.T) {
			s := New().(*scheme)
			s.LoadSRFI64()
			s.LoadFile(f)
			failCount := evalScheme(s, `%test-fail-count`)
			passCount := evalScheme(s, `%test-pass-count`)
			if n, ok := failCount.(int64); ok && n > 0 {
				t.Errorf("%d test(s) failed", n)
			}
			if n, ok := passCount.(int64); ok {
				t.Logf("%d test(s) passed", n)
			}
		})
	}
}
