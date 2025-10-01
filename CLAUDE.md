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

## Testing

Run `make test` to execute smoke tests covering:
- Basic CLI commands (--version, --help)
- Expression evaluation (`smt eval`)
- Script execution (`smt run`)
- REPL functionality
- Shebang script execution
- Error conditions and edge cases

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
- Proper Coalton toplevel declarations
- `main` function for script execution

## Dependencies

- SBCL (Steel Bank Common Lisp)
- Quicklisp (automatically installed by `make deps`)
- Coalton (loaded via Quicklisp)
- Make for build orchestration

The final binary is self-contained with no runtime dependencies.

## Feature Documentation

### Implemented Features
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
- **Limited Standard Library**: JSON and HTTP functions not yet exposed to user scripts (available internally)
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