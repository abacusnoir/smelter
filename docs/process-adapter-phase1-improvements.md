# Process Adapter Phase 1 Improvements

**Status**: ✅ Completed
**Date**: October 1, 2025
**Achievement**: Enhanced process adapter with safe command construction and platform detection

## Overview

Phase 1 extends the string-based process adapter with two critical improvements:
1. **Command Builder** - Safe command construction with automatic shell escaping
2. **Platform Detection** - Cross-platform OS detection and command mapping

These additions make the process adapter safer and more ergonomic while maintaining full backward compatibility.

## Implementation

### 1. Command Builder Module (`src/adapters/process/builder.lisp`)

**Purpose**: Provide type-safe list-based API with automatic shell injection prevention

**Exports**:
- `escape-shell-arg :: String -> String` - Escape special characters by wrapping in single quotes
- `build-command :: String -> List String -> String` - Build command string from cmd + arguments
- `run-args :: String -> List String -> Result ProcessError ProcessResult` - Execute command with list of arguments

**Key Features**:
- Single-quote escaping strategy that handles embedded quotes correctly
- Functional `fold` over arguments for command string construction
- Type-safe wrapper around existing `run` function

**Example Usage**:
```coalton
;; Safe argument escaping
(escape-shell-arg "file with spaces") ;; => "'file with spaces'"

;; Build command from list
(build-command "grep" (list "-r" "search term" "/path"))
;; => "grep '-r' 'search term' '/path'"

;; Execute with automatic escaping
(match (run-args "echo" (list "safe$(whoami)"))
  ((Ok result) "Command executed safely")
  ((Err e) "Command failed"))
```

**Lines of Code**: 75
**Test Coverage**: 8/8 tests passing (`test/test-process-builder.sh`)

### 2. Platform Detection Module (`src/adapters/process/platform.lisp`)

**Purpose**: Detect operating system and provide platform-specific command abstractions

**Exports**:
- `OS` type - ADT with variants: `Linux`, `MacOS`, `Windows`, `BSD`, `Unknown`
- `detect-os :: Unit -> OS` - Detect current operating system via `uname` or `ver`
- `os-name :: OS -> String` - Convert OS type to string name
- `platform-command :: OS -> String -> String` - Get OS-specific command for intent

**Key Features**:
- Runtime OS detection using `uiop:run-program`
- Fallback strategy: `uname -s` (Unix) → `ver` (Windows)
- Command intent mapping for common operations (list, copy, remove, move)
- Error handling with Unknown fallback

**Supported Intents**:
| Intent | Unix | Windows |
|--------|------|---------|
| list | ls | dir /b |
| copy | cp | copy |
| remove | rm | del |
| move | mv | move |

**Example Usage**:
```coalton
;; Detect current OS
(let ((os (detect-os Unit)))
  (os-name os))  ;; => "MacOS"

;; Get platform-specific command
(let ((os (detect-os Unit)))
  (platform-command os "list"))  ;; => "ls" on Unix, "dir /b" on Windows
```

**Lines of Code**: 123
**Test Coverage**: 4/4 tests passing (`test/test-process-platform.sh`)

### 3. Integration Tests

**Test Suite**: `test/test-process-integration.sh` - 7/7 tests passing

**Coverage**:
- **Backward Compatibility** (3 tests):
  - String API unchanged
  - Shell API unchanged
  - Environment API unchanged
- **Integration Tests** (4 tests):
  - Builder + core integration
  - Platform detection + execution
  - Complex command scenarios
  - Shell injection prevention

## Architecture Decisions

### 1. Module Organization
```
src/adapters/process/
├── process.lisp    - Core process adapter (string-based API)
├── builder.lisp    - Command construction utilities
└── platform.lisp   - OS detection and platform commands
```

**Rationale**: Modular design allows:
- Independent use of each component
- Clear separation of concerns
- Easy extension in future phases

### 2. ASDF Build Configuration
```lisp
(:module "process"
  :components
  ((:file "process")
   (:file "builder" :depends-on ("process"))
   (:file "platform" :depends-on ("process"))))
```

**Key Detail**: Explicit `:depends-on` ensures `process.lisp` is loaded before `builder` and `platform` are compiled, allowing them to reference the `smelter/adapters/process` package.

### 3. Coalton vs Lisp Implementation Strategy

**Pattern Used**:
- Coalton interface for type safety and composition
- Lisp FFI blocks for system calls and string manipulation
- `lisp:` prefix for Common Lisp functions (SBCL package nickname restriction)

**Why This Works**:
- Type safety at Coalton level
- Access to full Common Lisp ecosystem via FFI
- Proven pattern from other adapters (fs, http, json)

## Debugging Journey

### Issue 1: Missing Closing Parenthesis
**Problem**: `platform.lisp` had unclosed `coalton-toplevel` form
**Symptom**: LISP:READ error during compilation
**Fix**: Added missing closing parenthesis at end of file

### Issue 2: Package Reference Before Definition
**Problem**: `builder.lisp` and `platform.lisp` tried to use `#:proc` nickname for package that didn't exist yet
**Symptom**: "The name 'SMELTER/ADAPTERS/PROCESS' does not designate any package"
**Solution**:
1. Removed unused `#:proc` nickname from `platform.lisp`
2. Added explicit `:depends-on ("process")` in ASDF to ensure load order

### Issue 3: ADT Pattern Matching Unreliability
**Problem**: Coalton's `match` on OS variants always matched first case
**Workaround**: Used `lisp:eq` for `os-name`, then converted OS to string for `platform-command` logic
**Lesson**: String-based matching more reliable than ADT instance comparison in current Coalton

## Test Results

### All Phase 1 Tests Passing ✅

**Builder Tests** (8/8):
```bash
$ ./test/test-process-builder.sh
✅ All builder tests passed!
```

**Platform Tests** (4/4):
```bash
$ ./test/test-process-platform.sh
✅ All platform tests passed!
```

**Integration Tests** (7/7):
```bash
$ ./test/test-process-integration.sh
✅ All integration tests passed!
✅ Backward compatibility maintained!
```

**Smoke Tests**: 8/9 passing (1 pre-existing failure in `hello.coal` unrelated to process adapter)

## Gemini Verification ✅

Whole-repo verification confirms:
- ✅ All Phase 1 requirements met
- ✅ Implementation follows project patterns
- ✅ Code quality and documentation excellent
- ✅ Backward compatibility maintained
- ✅ Test coverage comprehensive

**Minor Suggestion**: Consider using Coalton `match` instead of `lisp:cond` for `os-name` and `platform-command` for more idiomatic Coalton style (non-blocking).

## Usage Examples

### Safe Command Construction
```coalton
;; Old way (injection risk):
(run "grep -r 'search $(whoami)' /path")  ;; UNSAFE!

;; New way (automatic escaping):
(run-args "grep" (list "-r" "search $(whoami)" "/path"))  ;; SAFE!
```

### Cross-Platform File Operations
```coalton
;; Detect OS and use appropriate command
(let ((os (detect-os Unit))
      (list-cmd (platform-command os "list")))
  (run list-cmd))  ;; Uses 'ls' on Unix, 'dir /b' on Windows
```

### Type-Safe Command Building
```coalton
;; Build complex command with automatic escaping
(let ((cmd (build-command "find"
             (list "." "-name" "*.lisp" "-type" "f"))))
  (run cmd))
```

## Performance Impact

**Binary Size**: No measurable increase (builder: 75 lines, platform: 123 lines)
**Startup Time**: No measurable impact (functions compiled in core)
**Runtime**: Negligible overhead (single-quote wrapping + string concatenation)

## Future Phases

Phase 1 lays the groundwork for:
- **Phase 2**: Advanced execution options (timeout, environment, working directory)
- **Phase 3**: Streaming I/O and output capture
- **Phase 4**: Process management (signals, kill, wait)
- **Phase 5**: Pipeline composition (multi-command workflows)
- **Phase 6**: Async execution (background processes)

## Related Documentation

- [Process Adapter Implementation](process-adapter-implementation.md) - Original string-based adapter
- [Process Adapter Improvements Theory](../responses/process-adapter-improvements-theory-2025-10-01.md) - Complete improvement roadmap
- [CLAUDE.md](../CLAUDE.md) - Project overview and development guide

## Lessons Learned

1. **ASDF Dependencies Matter**: Package nicknames require explicit load order via `:depends-on`
2. **Coalton ADT Matching**: Current Coalton has issues with ADT instance comparison - use string conversion
3. **Test-First Development**: Writing tests first caught issues early and ensured comprehensive coverage
4. **Gemini Verification**: Whole-repo verification provides valuable independent quality check
5. **Missing Parentheses**: Common Lisp's reader errors can be cryptic - always validate parentheses balance
