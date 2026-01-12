# CL Mode Implementation Response

**Date:** 2026-01-04

## Summary

Successfully implemented CL Mode for Smelter, adding pure Common Lisp execution alongside the existing Coalton mode. This doubles the addressable market without significant maintenance overhead.

## Changes Made

### 1. Core Implementation (`src/cli.lisp`)
Added ~100 lines implementing:
- `print-cl-help()` - CL-specific help message
- `run-cl-script(filepath)` - Execute .lisp files in CL-USER package
- `eval-cl-expr(expr-string)` - Evaluate CL expressions
- `run-cl-repl()` - Interactive CL REPL with "cl>" prompt
- `handle-cl-mode(args)` - Route CL subcommands

### 2. Updated Main Help
Changed tagline from:
- "Industrial-strength typed scripting with Coalton"

To:
- "Lisp scripts that just work. Types optional."

Added CL mode section to help output.

### 3. Test Suite (`test/test-cl-mode.sh`)
Created 11 comprehensive tests:
1. CL help output
2. Basic arithmetic eval
3. String formatting
4. List operations (reduce)
5. Lambda functions
6. Script execution with defun
7. Shebang stripping
8. Let macro
9. List primitives (cons/car/cdr)
10. Error handling (missing file)
11. Loop construct

All tests pass.

### 4. Documentation
- Created `docs/cl-mode-implementation.md`
- Updated `CLAUDE.md` with CL mode commands and architecture

### 5. Example Script
Created `examples/hello.lisp` demonstrating CL script with shebang.

## Commands

```bash
# CL Mode
smt cl run script.lisp    # Run CL script
smt cl eval '(+ 1 2)'     # Eval CL expression
smt cl repl               # CL REPL
smt cl --help             # CL help

# Coalton Mode (unchanged)
smt run script.coal       # Type-safe Coalton
smt eval '(+ 1 2)'        # Coalton expression
smt repl                  # Coalton REPL
```

## Verification

1. **Build:** Successful
2. **Smoke tests:** All pass (15/15)
3. **CL mode tests:** All pass (11/11)
4. **Gemini verification:** Confirmed complete

## Key Design Decisions

1. **Explicit mode** - Users must use `smt cl` for CL (no auto-detection)
2. **CL-USER package** - Standard CL environment for maximum compatibility
3. **Shared utilities** - Reuses `strip-shebang`, `read-file-content`
4. **No CL stdlib** - CL users know the language; value is in distribution

## Future Considerations (Phase 2, if requested)

1. Multiple return values in REPL
2. Script arguments helper
3. CL stdlib additions (only if users ask)

## File Changes

```
src/cli.lisp                        # +100 lines (CL mode + smt-cl detection)
test/test-cl-mode.sh               # New (11 tests)
examples/hello.lisp                # New example (shebang: #!/usr/bin/env smt-cl)
docs/cl-mode-implementation.md     # New documentation
CLAUDE.md                          # Updated
README.md                          # Updated with CL mode commands
Makefile                           # Updated (creates smt-cl symlink on build/install)
```

## Pre-Release Fixes

### Shebang Issue
- **Problem:** `#!/usr/bin/env smt cl run` doesn't work because `env` treats "smt cl run" as single command
- **Solution:** Added `smt-cl` symlink detection in `main()` function
- **Usage:** `#!/usr/bin/env smt-cl` works correctly

### Binary Size
- **Uncompressed:** 20MB (unchanged - CL mode adds only ~100 lines)
- **Compressed (UPX):** ~9.3MB (as documented)
- No new dependencies added
