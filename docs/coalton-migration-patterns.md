# Coalton Migration Patterns for Smelter

This document outlines the official Coalton patterns used in Smelter stdlib and adapters, following the official Coalton library binding patterns.

## Overview

Smelter has been migrated to use official Coalton patterns instead of custom adapter macros. This ensures compatibility with current and future Coalton versions and follows established best practices.

## Key Migration Changes

### Old Pattern → New Pattern

**Before (Custom adapters)**:
```lisp
(define-adapter file-adapter
  (defcoalton read-file ...)
  ...)
```

**After (Official Coalton)**:
```lisp
(coalton-toplevel
  (declare read-file (String -> (Result FileError String)))
  (define (read-file path)
    (lisp (Result FileError String) (path)
      ...)))
```

## Package Structure

### Standard Package Definition
```lisp
(defpackage #:smelter.stdlib.module-name
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; Exported functions and types
   #:function-name
   #:ErrorType #:ErrorVariant))

(in-package #:smelter.stdlib.module-name)
```

### For Adapters (External Libraries)
```lisp
(defpackage #:smelter/adapters/library-name
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:cp #:coalton-prelude))
  (:export ...))
```

## Type Definitions

### Error Types
Always use ADT (Algebraic Data Types) for structured error handling:

```lisp
(coalton-toplevel
  (define-type FileError
    (FileNotFound String)
    (PermissionDenied String)  
    (IOError String)))
```

### Data Types  
```lisp
(coalton-toplevel
  (define-type JSONValue
    JSONNull
    (JSONBool Boolean)
    (JSONString String)
    (JSONArray (List JSONValue))))
```

## Function Patterns

### 1. Pure Coalton Functions
For functions that don't need CL interop:
```lisp
(coalton-toplevel
  (declare pure-function (String -> String))
  (define (pure-function input)
    "Pure Coalton implementation"
    (append input " processed")))
```

### 2. CL Interop with `lisp` Forms
For functions that call Common Lisp code:

```lisp
(coalton-toplevel
  (declare read-file (String -> (Result FileError String)))
  (define (read-file path)
    "Read entire contents of a file as a string"
    (lisp (Result FileError String) (path)
      (cl:handler-case
          (Ok (alexandria:read-file-into-string path))
        (cl:file-error (e)
          (Err (FileNotFound (cl:format cl:nil "File not found: ~A" path))))
        (cl:error (e)
          (Err (IOError (cl:format cl:nil "IO error: ~A" e))))))))
```

### 3. Simple CL Wrappers
For simple CL function wrappers:
```lisp
(coalton-toplevel
  (declare file-exists-p (String -> Boolean))
  (define (file-exists-p path)
    "Check if a file exists"
    (lisp Boolean (path)
      (cl:not (cl:null (cl:probe-file path))))))
```

## Error Handling Patterns

### Always Use Result Types
```lisp
(coalton-toplevel
  (declare risky-operation (String -> (Result ErrorType SuccessType)))
  (define (risky-operation input)
    (lisp (Result ErrorType SuccessType) (input)
      (cl:handler-case
          (Ok (some-cl-function input))
        (cl:specific-error (e)
          (Err (SpecificError "specific error message")))
        (cl:error (e)
          (Err (GeneralError (cl:format cl:nil "~A" e))))))))
```

### Error Type Hierarchy
Structure error types hierarchically:
```lisp
(define-type NetworkError
  (ConnectionFailed String)
  (Timeout String)
  (InvalidResponse String))

(define-type AppError  
  (NetworkErr NetworkError)
  (FileErr FileError)
  (ValidationErr String))
```

## Performance Optimizations

### Use `(inline)` for Simple Wrappers
```lisp
(coalton-toplevel
  (inline)
  (declare simple-wrapper (String -> String))
  (define (simple-wrapper input)
    (lisp String (input)
      (simple-cl-function input))))
```

### Declare Types Explicitly
Always provide explicit type declarations:
```lisp
(coalton-toplevel
  (declare process-list ((List String) -> (List String)))
  (define (process-list items)
    (map (fn (item) (append item " processed")) items)))
```

## Build System Integration

### Adding New Modules
1. Create the module file in appropriate directory (`src/stdlib/` or `src/adapters/`)
2. Add to `build/create-image.lisp`:
   ```lisp
   (load (merge-pathnames "src/stdlib/new-module.lisp" cwd))
   ```
3. Add package verification:
   ```lisp
   (unless (find-package :smelter.stdlib.new-module)
     (error "New module package not found"))
   ```

## Testing Patterns

### Manual Testing
```bash
# Test functions via eval command
./smt eval '(module:function-name "test-input")'

# Test in REPL  
./smt repl
> (module:function-name "test-input")
```

### Expected Output Formats
- Success: `#.(ok result-value)`
- Error: `#.(err #.(error-type "error message"))`
- Boolean: `t` or `nil`

## Common Gotchas

### 1. Package References
- Use full package names: `smelter.stdlib.module:function`
- Ensure packages are loaded in build system

### 2. CL Symbol Prefixing  
Always prefix CL symbols with `cl:`:
```lisp
(cl:handler-case
  (cl:with-open-file (stream path ...)
    (cl:write-string content stream)))
```

### 3. Result Type Construction
```lisp
;; Correct
(Ok value)
(Err error-value)

;; Incorrect (will cause compilation errors)  
(coalton:ok value)
```

### 4. String Handling
Coalton strings are CL strings, but be explicit about conversions when needed.

## Migration Checklist

When adding a new module or migrating existing code:

- [ ] Use `coalton-toplevel` wrapper
- [ ] Define proper error types with ADTs
- [ ] Use `Result` types for fallible operations  
- [ ] Provide explicit type declarations with `declare`
- [ ] Use `(lisp ReturnType (args) ...)` for CL interop
- [ ] Add proper error handling with `handler-case`
- [ ] Export all public symbols in defpackage
- [ ] Add module to build system
- [ ] Test functionality via eval/REPL
- [ ] Document any specific usage patterns

## Further Reading

- [Official Coalton Documentation](https://coalton-lang.github.io/)
- [Coalton Library Binding Patterns](https://coalton-lang.github.io/blog/2024/01/coalton-library-bindings/)
- [Smelter Architecture Documentation](architecture.md)