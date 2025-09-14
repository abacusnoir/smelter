# Smelter Adapter Pattern Template

**Status**: Established - JSON Adapter Working ✅  
**Date**: September 13, 2025

## 🎯 Working Pattern Discovered

After solving the core integration challenges, we've established the correct pattern for Smelter adapters.

## 📋 File Structure Template

```lisp
;; src/adapters/[name].lisp
(defpackage #:smelter/adapters/[name]
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:cp #:coalton-prelude))
  (:export 
   ;; Export all public types and functions
   #:TypeName
   #:ErrorType
   #:main-function))

(in-package #:smelter/adapters/[name])

;; CRITICAL: Single coalton-toplevel block containing everything
(coalton-toplevel
  ;; 1. Type definitions first
  (define-type MainType
    Constructor1
    (Constructor2 Type))

  (define-type ErrorType
    (ErrorCase1 String)
    (ErrorCase2 String String))

  ;; 2. Function declarations and definitions together
  (declare function-name (InputType -> (Result ErrorType OutputType)))
  (define (function-name input)
    "Documentation string"
    ;; Implementation using both Ok and Err paths
    (if condition
        (Ok result)
        (Err (ErrorCase1 "message"))))

  ;; 3. More functions follow same pattern
  (declare another-function (Type1 -> Type2 -> Type3))
  (define (another-function arg1 arg2)
    "Another function"
    ;; Implementation
    ))
```

## 🔑 Critical Success Factors

### 1. Result Type Parameter Order
**CRITICAL**: Coalton uses `Result<Error, Success>` order:
```lisp
;; CORRECT
(declare parse-json (String -> (Result JSONError JSONValue)))

;; WRONG - will cause type mismatch errors
(declare parse-json (String -> (Result JSONValue JSONError)))
```

### 2. Single coalton-toplevel Block
- **NEVER** have multiple `coalton-toplevel` blocks
- Put ALL `declare` and `define` statements inside ONE block
- Order: Types first, then function pairs (declare + define)

### 3. Package Structure
```lisp
;; Use standard defpackage format
(defpackage #:smelter/adapters/[name]
  (:use #:coalton #:coalton-prelude)
  (:export #:all-public-symbols))

(in-package #:smelter/adapters/[name])
```

### 4. Function Implementation Patterns
- Always provide both `Ok` and `Err` code paths when returning Result types
- Avoid stub implementations that only return `Err` - causes type inference issues
- Use concrete implementations even if simplified

## 🧪 Testing Pattern

### Build Integration
Add to `build/create-image.lisp`:
```lisp
;; Load adapter
(load (merge-pathnames "src/adapters/[name].lisp" cwd))

;; Verify package
(unless (find-package :smelter/adapters/[name])
  (error "Adapter package not found"))
```

### Function Testing
```bash
# Test function accessibility
./smt eval 'smelter/adapters/[name]:function-name'

# Test basic functionality
./smt eval '(smelter/adapters/[name]:function-name "test-input")'
```

## 📊 JSON Adapter Success Evidence

The JSON adapter successfully demonstrates this pattern:

✅ **Build Success**: `Built: smt ( 18M)` - No fatal errors  
✅ **Loading Success**: `Loading Smelter JSON adapter...` - Package loads  
✅ **Function Access**: Functions accessible via package name  
✅ **Functionality**: Parse operations return correct Result types:
- `parse-json "null"` → `Ok(JSONNull)`
- `parse-json "true"` → `Ok(JSONBool(True))`  
- `parse-json "false"` → `Ok(JSONBool(False))`
- `parse-json "invalid"` → `Err(ParseError("Complex JSON not yet supported"))`

## 🚀 Next Adapters Application Order

1. **CLI Adapter** - Pure Coalton logic, no external deps
2. **File System Adapter** - Uses SBCL built-ins
3. **Process Adapter** - Uses sb-ext, moderate complexity  
4. **HTTP Adapter** - Most complex (drakma + networking)

## 🔧 Common Issues & Solutions

### Issue: "coalton:declare is only valid in a Coalton toplevel"
**Solution**: Move all `declare` statements inside the `coalton-toplevel` block

### Issue: Result type parameter mismatch  
**Solution**: Use `(Result ErrorType SuccessType)` order, not the reverse

### Issue: Type inference problems with stub functions
**Solution**: Implement both success and error paths in functions

### Issue: Unmatched parentheses
**Solution**: Verify single closing paren for `coalton-toplevel` block

## 📝 Template Checklist

For each new adapter:
- [ ] Package definition with correct exports
- [ ] Single `coalton-toplevel` block
- [ ] Types defined first
- [ ] Result types use correct parameter order  
- [ ] All functions have both declare and define
- [ ] Functions implement both success and error paths
- [ ] Added to build system loading
- [ ] Package verification added
- [ ] Basic functionality tested

This pattern is proven to work and should be applied systematically to all remaining adapters.