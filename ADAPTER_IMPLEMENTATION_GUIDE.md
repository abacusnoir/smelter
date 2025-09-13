# Smelter Adapters Implementation Guide

**For ClaudeCode: This guide contains everything needed to implement Smelter's 5 essential adapters**

## 🎯 Project Context

Smelter is a self-contained CLI runner for Coalton (statically-typed Lisp) that enables typed scripting with instant startup. We currently have:
- 16MB self-contained binary with 66ms startup ✅
- Pure Coalton syntax working (all operators: +, -, *, /, ==, >, <, etc.) ✅  
- REPL and script execution functioning perfectly ✅
- Package context fixed (coalton-user) ✅

## 🏗️ Current Build System

The project uses this Makefile structure:
```makefile
SBCL := sbcl --non-interactive --no-userinit
COALTON_VERSION := 0.8.0
SMELTER_VERSION := 0.1.0

build-core:
	$(SBCL) --load build/prepare-image.lisp
	
build-binary: build-core
	$(SBCL) --core smelter.core \
	        --eval "(sb-ext:save-lisp-and-die \"smt\" :executable t)"
```

## 🎯 Mission: Implement 5 Essential Adapters

### Required Directory Structure
```
src/adapters/
├── http.lisp      # HTTP client with drakma
├── json.lisp      # JSON parsing with st-json  
├── fs.lisp        # File system operations
├── process.lisp   # Process/shell execution
└── cli.lisp       # CLI argument parsing

test/
└── adapter-tests.coal  # Comprehensive test suite

examples/
└── github-stats.coal   # Showcase all adapters working together

build/
└── build-adapters.lisp # Build integration script
```

## 📋 Implementation Requirements

### 1. Core Principles
- **Type Safety First**: Use Result/Optional types, no exceptions
- **Pure Coalton Interfaces**: Hide all Common Lisp implementation details
- **Developer Ergonomics**: Simple functions for common cases
- **Comprehensive Error Handling**: Descriptive error variants

### 2. Dependencies to Install
```lisp
;; Required quicklisp dependencies
(ql:quickload '(:drakma :st-json :split-sequence :cl-ppcre))
```

### 3. Package Structure
Each adapter should be in the `smelter/adapters` package and export its public API.

## 🔧 Adapter Specifications

### 1. HTTP Adapter (src/adapters/http.lisp)
**Purpose**: Type-safe HTTP client with comprehensive error handling

**Key Types**:
```coalton
(define-type Method GET POST PUT DELETE PATCH)
(define-type Request (Request String Method Headers (Optional String)))
(define-type Response (Response Integer Headers String))  
(define-type HttpError (NetworkError String) (TimeoutError String) (ParseError String))
```

**Core Functions**:
- `http-request` - Full control HTTP request
- `http-get` - Simple GET request  
- `http-post` - Simple POST with body
- `http-get-json` - GET expecting JSON response
- `make-headers`, `response-status`, `response-body`, `get-header`

**Implementation Notes**:
- Use `drakma` for HTTP operations
- 30-second timeout default
- Automatic JSON content-type detection
- Clean Result-based error handling

### 2. JSON Adapter (src/adapters/json.lisp) 
**Purpose**: Parse and generate JSON with complete type safety

**Key Types**:
```coalton
(define-type JSONValue JSONNull (JSONBool Boolean) (JSONNumber Double-Float) 
                       (JSONString String) (JSONArray (List JSONValue))
                       (JSONObject (List (Tuple String JSONValue))))
(define-type JSONError (ParseError String) (KeyError String) (TypeError String String))
```

**Core Functions**:
- `parse-json` - Parse JSON string to JSONValue
- `stringify-json` - Convert JSONValue to string
- `json-get` - Path-based access ("user.name")
- `json-get-string`, `json-get-number`, `json-get-bool` - Type-safe getters
- `json-object`, `json-array` - Constructor helpers

**Implementation Notes**:
- Use `st-json` for parsing performance
- Support dot-notation paths for nested access
- Pretty printing support
- Comprehensive error messages

### 3. File System Adapter (src/adapters/fs.lisp)
**Purpose**: Robust file and directory operations

**Key Types**:
```coalton  
(define-type FileInfo (FileInfo Integer Integer Boolean String))
(define-type FSError (FileNotFound String) (PermissionDenied String) (IOError String))
```

**Core Functions**:
- `read-file`, `write-file`, `append-file`, `delete-file`
- `file-exists?`, `directory-exists?`, `get-file-info`
- `list-directory`, `create-directory`, `remove-directory`  
- `join-paths`, `absolute-path`, `path-filename`, `path-extension`
- `read-lines`, `write-lines`

**Implementation Notes**:
- Use SBCL's native file operations
- Cross-platform path handling
- Atomic operations where possible
- Proper permissions and metadata

### 4. Process Adapter (src/adapters/process.lisp)
**Purpose**: Execute external commands and processes safely

**Key Types**:
```coalton
(define-type ProcessResult (ProcessResult Integer String String))
(define-type ProcessError (CommandNotFound String) (ExecutionError String))
(define-type ProcessConfig (ProcessConfig (Optional String) (Optional (List String)) (Optional Integer)))
```

**Core Functions**:
- `run-process` - Execute command and capture output
- `run-process-with-input` - Execute with stdin
- `shell` - Run command in shell
- `spawn-process` - Async execution
- `capture-command` - Simple stdout capture
- `command-exists?`, `run-with-sudo`

**Implementation Notes**:
- Use `sb-ext:run-program` for process management
- Shell detection for cross-platform compatibility  
- Timeout support
- Proper stream handling

### 5. CLI Adapter (src/adapters/cli.lisp)
**Purpose**: Type-safe command-line argument parsing

**Key Types**:
```coalton
(define-type ArgType StringArg IntegerArg BooleanArg (ListArg ArgType))
(define-type ArgSpec (ArgSpec String String ArgType Boolean (Optional String)))
(define-type Args (Args (List (Tuple String String))))
(define-type CLIError (MissingRequired String) (InvalidType String String String))
```

**Core Functions**:
- `parse-args` - Parse according to specifications
- `get-arg`, `get-arg-string`, `get-arg-integer`, `get-arg-boolean`
- `string-arg`, `integer-arg`, `boolean-arg` - Spec builders
- `generate-help` - Automatic help generation

**Implementation Notes**:
- Hand-rolled parser for full control
- GNU-style arguments (--long, -short)
- Type conversion with validation
- Automatic help text generation

## 🧪 Testing Requirements

### Test File: test/adapter-tests.coal
Comprehensive test suite covering:
- HTTP requests to real endpoints (httpbin.org)
- JSON parsing edge cases
- File system operations with temp files
- Process execution scenarios  
- CLI argument parsing variations

**Test Framework Pattern**:
```coalton
(define-type TestResult (TestPass String) (TestFail String String))
(declare run-test (String -> (() -> Boolean) -> TestResult))
(declare run-test-suite ((List (() -> TestResult)) -> Unit))
```

### Integration Example: examples/github-stats.coal
Comprehensive example demonstrating all 5 adapters:
1. **CLI** - Parse username, output file, verbose flag
2. **HTTP** - Fetch GitHub user and repo data  
3. **JSON** - Parse API responses
4. **Process** - Get timestamp via shell
5. **FS** - Save formatted report to file

**Usage**: `smt run examples/github-stats.coal --username torvalds --verbose`

## 🔨 Build Integration

### Makefile Updates
Add these targets:
```makefile
install-deps:
	$(SBCL) --eval "(ql:quickload '(drakma st-json split-sequence cl-ppcre))" --eval "(quit)"

build-adapters: install-deps  
	$(SBCL) --load build/build-adapters.lisp --eval "(build-adapters-image)" --eval "(quit)"

test-adapters: build-adapters
	./smt run test/adapter-tests.coal

test-example: build-adapters
	./smt run examples/github-stats.coal --username torvalds --verbose --output /tmp/test-stats.txt
```

### Build Script: build/build-adapters.lisp
Integration script that:
- Loads required dependencies (drakma, st-json, etc.)
- Loads all adapter source files in order
- Verifies adapter integration
- Saves enhanced SBCL image
- Creates final executable with adapters included

## 🎯 Success Criteria

**After implementation, these should work:**

1. **HTTP Test**: `smt eval '(use smelter/http) (match (http-get "https://httpbin.org/get") ((Ok resp) (println (show (response-status resp)))) ((Err e) (println (show e))))'`

2. **JSON Test**: `smt eval '(use smelter/json) (match (parse-json "{\"test\": true}") ((Ok json) (println (stringify-json json))) ((Err e) (println (show e))))'`

3. **File Test**: `smt eval '(use smelter/fs) (match (write-file "/tmp/smelter-test" "hello") ((Ok _) (println "File written")) ((Err e) (println (show e))))'`

4. **Process Test**: `smt eval '(use smelter/process) (match (run-process "echo test") ((Ok result) (println (process-stdout result))) ((Err e) (println (show e))))'`

5. **CLI Test**: `smt eval '(use smelter/cli) (let ((specs (list (string-arg "test" "Test" False)))) (match (parse-args specs (list "--test" "value")) ((Ok args) (println (get-arg args "test"))) ((Err e) (println (show e)))))'`

6. **Full Example**: `smt run examples/github-stats.coal --username torvalds --output /tmp/stats.txt --verbose`

## 🚀 Implementation Steps for ClaudeCode

1. **Create directory structure** (`src/adapters/`, `test/`, `examples/`, ensure `build/` exists)

2. **Implement adapters in order**:
   - Start with JSON (no external deps)
   - Then HTTP (depends on drakma)
   - Then FS (uses SBCL internals)
   - Then Process (uses sb-ext)
   - Finally CLI (pure Coalton)

3. **Create test suite** that validates each adapter

4. **Create GitHub stats example** showing all adapters working together

5. **Update build system** with new Makefile targets and build script

6. **Create README update** documenting the new adapters

7. **Test integration** by running the success criteria commands

## 🔍 Key Implementation Notes

- **Coalton Syntax**: Use `coalton-toplevel` to wrap all definitions
- **Error Handling**: Every function that can fail returns `Result`
- **Type Exports**: Export all public types and functions  
- **Documentation**: Include docstrings for all public functions
- **Testing**: Test both success and failure paths
- **Performance**: Startup impact should be minimal (<10ms additional)

## 📚 Reference Files

All the complete implementation code for each adapter has been provided in the previous artifacts. Use those as the authoritative source for:
- Complete function signatures
- Type definitions  
- Error handling patterns
- Implementation details
- Test cases

The artifacts contain production-ready code that follows all the principles and requirements outlined in this guide.

---

**Ready to make Smelter truly powerful for real-world scripting!** 🔥
