# CL Mode Implementation

**Date:** 2026-01-04
**Status:** Implemented and tested

## Overview

CL Mode adds pure Common Lisp execution to Smelter alongside the existing Coalton mode. This doubles Smelter's addressable market without significant maintenance overhead.

## Positioning Update

**Before:** "Type-safe scripts that just work"
**After:** "Lisp scripts that just work. Types optional."

## Commands

### `smt cl run <file.lisp>`
Run a Common Lisp script without Coalton translation.

```bash
smt cl run script.lisp
```

### `smt cl eval <expr>`
Evaluate a Common Lisp expression.

```bash
smt cl eval '(+ 1 2 3)'
# => 6

smt cl eval '(format nil "Hello ~A" "World")'
# => Hello World

smt cl eval '(mapcar (lambda (x) (* x x)) (list 1 2 3))'
# => (1 4 9)
```

### `smt cl repl`
Start an interactive CL REPL.

```bash
smt cl repl
# cl> (defun square (x) (* x x))
# SQUARE
# cl> (square 7)
# 49
```

### `smt cl --help`
Show CL-specific help.

## File Conventions

| Extension | Mode | Command |
|-----------|------|---------|
| `.coal` | Coalton (type-safe) | `smt run` |
| `.lisp` | Common Lisp | `smt cl run` |

## Shebang Support

CL scripts can use shebangs for direct execution via `smt-cl`:

```lisp
#!/usr/bin/env smt-cl

(defun greet (name)
  (format t "Hello, ~A!~%" name))

(greet "World")
```

**Note:** Use `smt-cl` (not `smt cl run`) in shebangs because most systems don't support multiple arguments after `/usr/bin/env`. The `smt-cl` symlink is created automatically during build and install.

## Implementation Details

### Code Changes (~100 lines in cli.lisp)

1. **`print-cl-help`** - CL-specific help message
2. **`run-cl-script`** - Execute .lisp files without Coalton translation
3. **`eval-cl-expr`** - Evaluate CL expressions
4. **`run-cl-repl`** - Interactive CL REPL with prompt "cl>"
5. **`handle-cl-mode`** - Route CL subcommands

### Execution Environment

CL scripts run in the `CL-USER` package with full SBCL capabilities:
- All standard CL functions (`format`, `loop`, `defun`, etc.)
- Access to SBCL extensions
- Same binary, same instant startup

### Key Design Decisions

1. **Explicit mode** - Users must use `smt cl` for CL mode (no auto-detection)
2. **Standard environment** - Uses `CL-USER` package for maximum compatibility
3. **Shebang support** - Same pattern as Coalton mode
4. **No CL stdlib** - CL users know the language; value is in distribution

## Testing

CL mode has 11 dedicated tests in `test/test-cl-mode.sh`:

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

Run tests:
```bash
./test/test-cl-mode.sh
```

## Future Considerations

### Phase 2: CL Stdlib (post-launch, if requested)
Only add if users ask. CL users already know `format`, `with-open-file`, etc.

### Phase 3: Auto-detect (probably never)
Could detect mode from file extension, but explicit is better. Avoid magic.

## Files Modified

- `src/cli.lisp` - Added CL mode functions (~100 lines)
- `test/test-cl-mode.sh` - CL mode test suite (11 tests)
- `examples/hello.lisp` - Example CL script
- `CLAUDE.md` - Updated documentation
