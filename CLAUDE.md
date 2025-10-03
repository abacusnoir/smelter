# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Smelter is a self-contained CLI runner for Coalton (statically-typed Lisp) that embeds SBCL and Coalton into a single executable. It provides zero-dependency scripting with type safety and fast startup times.

## Build System

The project uses Make with the following key commands:

- `make build` - Build the smt executable (main development command)
- `make test` - Run comprehensive smoke tests
- `make deps` - Install dependencies (Quicklisp and Coalton)
- `make clean` - Clean build artifacts
- `make dev` - Quick rebuild cycle (clean + build + basic tests)
- `make install` - Install to /usr/local/bin
- `make compress` - Compress binary with UPX (optional)
- `make release` - Create release archive

## Architecture

### Two-Stage Build Process

1. **Core Creation** (`build/create-image.lisp`): Creates SBCL core with pre-loaded Coalton for fast startup
2. **Executable Generation** (`src/cli.lisp`): Wraps core with CLI interface using `sb-ext:save-lisp-and-die`

### Key Components

- `src/cli.lisp` - Main CLI application with command parsing, REPL, script execution
- `build/create-image.lisp` - SBCL core builder that embeds Coalton
- `test/smoke-test.sh` - Comprehensive test suite covering all CLI commands
- `examples/` - Sample Coalton scripts demonstrating language features

### CLI Commands Structure

The smelter package exports:
- `main` - Entry point handling argument parsing
- `run-script` - Execute .coal files with shebang support
- `start-repl` - Interactive Coalton REPL
- `eval-expression` - Evaluate single expressions
- `save-executable` - Create the final binary

### Script Execution Model

Scripts run in `smelter-user` package with:
- Automatic shebang stripping
- Coalton environment setup
- Auto-execution of `main` function if present
- Proper error handling and exit codes

### Script Syntax (Clean Coalton)

Smelter uses **clean Coalton syntax** without boilerplate. Scripts look like this:

```lisp
#!/usr/bin/env smt run

(declare add (Integer -> Integer -> Integer))
(define (add x y) (+ x y))

(define main
  (println (show (add 2 3))))
```

**No `coalton-toplevel` wrappers needed!** The translator automatically handles the conversion.

**For backward compatibility**, the old `(coalton-toplevel ...)` syntax still works, but clean syntax is preferred for better UX.

## Testing

Smelter has comprehensive test coverage with **112+ test cases** across 7 test suites:

### Quick Testing
```bash
make test                # Basic tests (smoke + eval regression)
make test-comprehensive  # Comprehensive suite (74 tests) - recommended
make test-all            # ALL test suites (112+ tests)
```

### Test Suites
- **Comprehensive Suite** (`make test-comprehensive`): 74 tests covering basic functionality, edge cases, pattern matching, file operations, clean syntax, performance, error recovery, stdlib, launch examples, and CLI
- **Stress Tests** (`make test-stress`): 14 tests for performance under load, large inputs, rapid execution, recursion depth, and memory stability
- **Cross-Platform Tests** (`make test-cross-platform`): 24 tests for shell compatibility, locales, file permissions, path handling, exit codes, and concurrent execution
- **Smoke Tests**: Basic CLI functionality
- **Eval Regression Tests**: Pattern matching validation
- **JSON Regression Tests**: JSON parsing and generation
- **Launch Verification Tests**: HN launch readiness (11 tests)

### Individual Test Commands
```bash
./test/comprehensive-test-suite.sh  # 74 tests
./test/stress-test.sh               # 14 tests
./test/cross-platform-test.sh       # 24 tests
./test/smoke-test.sh                # Smoke tests
./test/eval-regression.sh           # Eval tests
```

See **[Comprehensive Test Coverage Achievement](docs/comprehensive-test-coverage-achievement.md)** for detailed test documentation.

## Common Development Tasks

### Adding New CLI Commands

1. Add command parsing logic in `parse-arguments` function
2. Implement command handler following existing patterns
3. Add help text in `print-help`
4. Add tests in `test/smoke-test.sh`

### Modifying Core Image

Edit `build/create-image.lisp` to:
- Load additional libraries at build time
- Modify optimization settings
- Add pre-compiled functionality

### Creating Examples

Add .coal files to `examples/` with:
- Shebang: `#!/usr/bin/env smt run`
- Clean Coalton syntax (no `coalton-toplevel` wrapper)
- Type declarations using `(declare name Type)`
- Function definitions using `(define (name args) body)`
- `main` function for script execution

Example structure:
```lisp
#!/usr/bin/env smt run
;; Comments describing the example

(declare my-function (Type -> Type))
(define (my-function arg)
  body)

(define main
  (println "Hello from Smelter!"))
```

### Showcase Examples

The `examples/showcase/` directory contains **6 production-ready demos** (19-33 lines each) demonstrating real-world type-safe scripting:

- **config-validator.coal** (19 lines) - Type-safe configuration validation
- **error-handling.coal** (27 lines) - Result types for guaranteed error handling
- **type-safety.coal** (29 lines) - Compile-time type checking examples
- **rosetta.coal** (31 lines) - Expressive code with type safety
- **build-pipeline.coal** (33 lines) - Type-safe build orchestration
- **data-transform.coal** (33 lines) - Type-safe data processing pipelines

Run demos:
```bash
./smt run examples/showcase/config-validator.coal
./test/verify-demos.sh  # Verify all showcase demos work
```

See `examples/showcase/README.md` for detailed descriptions and use cases.

## Dependencies

- SBCL (Steel Bank Common Lisp)
- Quicklisp (automatically installed by `make deps`)
- Coalton (loaded via Quicklisp)
- Make for build orchestration

The final binary is self-contained with no runtime dependencies.

## Feature Documentation

### Implemented Features
- **[Showcase Demos Achievement](docs/showcase-demos-achievement.md)** - 6 production-ready demos (19-33 lines) showcasing real-world type-safe scripting use cases - config validation, error handling, build pipelines, data transforms - all verified and HN-ready
- **[Comprehensive Test Coverage Achievement](docs/comprehensive-test-coverage-achievement.md)** - Production-ready test coverage with 112+ tests across comprehensive, stress, and cross-platform suites - ensuring "smt scripts just work" for HN launch
- **[Launch-Ready Achievement](docs/launch-ready-achievement.md)** - Show functions (show-int, show-bool) + 5 working launch examples + verification script - Complete HN launch readiness with 11/11 tests passing
- **[Standard Library I/O Implementation](docs/stdlib-io-implementation.md)** - Core I/O package (`smelter.stdlib.io`) with print, println, and read-line functions - enables batteries-included I/O for clean Coalton scripts
- **[Process Adapter Phase 1 Improvements](docs/process-adapter-phase1-improvements.md)** - Safe command construction with shell escaping and cross-platform OS detection
- **[Process Adapter Implementation](docs/process-adapter-implementation.md)** - Simplified string-based process execution adapter (5 of 6 adapters now working)
- **[Simplified Coalton-Toplevel Implementation](docs/coalton-toplevel-implementation.md)** - Pure Coalton support using native Coalton contexts instead of preprocessing
- **[Official Coalton Migration Patterns](docs/coalton-migration-patterns.md)** - Migrated stdlib to use official Coalton library binding patterns
- **[HTTP and JSON Adapters](docs/http-json-adapters.md)** - Network and JSON capabilities for API interaction and data processing
- **[Performance Optimization Achievement](docs/performance-optimization-achievement.md)** - 51.6% startup improvement (88ms → 42.6ms) with lazy loading architecture
- **[End-to-End Testing Achievement](docs/e2e-testing-achievement.md)** - Comprehensive E2E integration test suite completing the testing pyramid
- **[CI Integration Achievement](docs/ci-integration-achievement.md)** - Complete CI/CD pipeline with automated testing and code coverage
- **[Smelter CSV Library](docs/smelter-csv-library.md)** - Type-safe CSV parsing and generation for data processing and report generation
- **[Smelter Adapter Implementation](docs/smelter-adapter-implementation.md)** - Complete I/O adapter system: HTTP, JSON, File System, Process, and CLI adapters
- **Enhanced JSON Support** - JSON parsing with nested structure handling for objects and arrays
- **Release Automation** - Complete release pipeline with performance metrics and packaging

### Performance Metrics (v0.1.0)
- **Binary Size**: 9.3MB optimized (was 18MB), self-contained, no runtime dependencies  
- **Startup Time**: ~42.6ms average (was 88ms) - **51.6% improvement**
- **Core Features**: Ultra-fast arithmetic, basic evaluation, JSON primitives, file I/O foundations
- **Competitive Position**: Faster than Ruby (62ms), competitive with Python (29ms) and Node.js (35ms)

### Recent Major Fixes
- **[Package System Fix](docs/package-system-fix.md)** - Critical fix resolving package lock violations that prevented Result/Tuple usage in user scripts, enabling modern Coalton functional patterns

### Known Limitations
- **[SBCL Runtime Limitations](docs/sbcl-runtime-limitations.md)** - CLI argument handling limitations inherited from SBCL
- **Unicode in string literals**: Parser fails with Unicode characters (✓, ✗) in string literals - use ASCII only for now
- **Integer division**: No `div` function available yet (workaround: use multiplication/subtraction)
- **Floating point**: No Float type or show-float function (integers only for now)
- **Advanced show functions**: Only show-int and show-bool available (no show for lists, tuples, custom types)
- **Datetime stdlib disabled**: `smelter-datetime.lisp` has compilation errors and is temporarily disabled from build
- **String Operations**: Some string functions need refinement for full compatibility

## Documentation Maintenance

**IMPORTANT**: Always keep this documentation up-to-date after making significant changes:

- **Architecture changes**: Update the "Architecture" and "Key Components" sections
- **New CLI commands**: Update "CLI Commands Structure" and "Common Development Tasks" 
- **New build targets**: Update "Build System" section
- **New dependencies**: Update "Dependencies" section
- **Major refactoring**: Review entire document for accuracy
- **New features**: Add documentation in `docs/` and link from "Feature Documentation" section

When this CLAUDE.md becomes too large (>200 lines), consider splitting into:
- `docs/architecture.md` - Detailed system architecture
- `docs/development.md` - Development workflows and patterns
- `docs/testing.md` - Testing strategies and procedures

Always update documentation immediately after implementing changes, not as a separate task.
- Add a docs/ entry for each achievement by you in terms of implementing a new feature, and link to it from CLAUDE.md
- Each of these responses you give me at the end, should be logged somewhere. I suggest pasting them in `responses/`. Don't link to them from anywhere, but keep them all there.