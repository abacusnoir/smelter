# Simplified Coalton-Toplevel Implementation

## Overview
Implemented a simplified approach for pure Coalton support in Smelter by wrapping code in appropriate Coalton contexts rather than attempting complex preprocessing.

## Problem Statement
The initial approach attempted to preprocess Coalton operators (like `+`, `-`, `==`, etc.) and transform them to fully qualified names (e.g., `coalton-library/classes:+`). This approach was complex and error-prone:
- Package lock violations when interning certain symbols
- Complex symbol transformation logic
- Difficulty handling special forms
- Maintenance burden of operator mappings

## Solution
Adopted a simplified architecture that leverages Coalton's native environments:

### For Script Files
```lisp
(coalton:coalton-toplevel
  ;; All user's Coalton definitions go here
  (define (add x y) (+ x y))
  (define main (print "Hello")))
```

### For REPL Expressions  
```lisp
(coalton:coalton
  ;; Single expressions evaluated directly
  (+ 2 3))
```

## Implementation Details

### Key Changes
1. **Removed preprocessor system** (`src/coalton-preprocessor.lisp`)
2. **Simplified translator** (`src/coalton-translator.lisp`):
   - `translate-for-repl-simple`: Wraps expressions in `coalton:coalton-toplevel`
   - `translate-for-script-simple`: Wraps entire scripts in `coalton-toplevel`
3. **Updated build process** to skip preprocessor loading

### Code Structure
```lisp
(defun translate-for-repl-simple (script)
  "Simplified REPL translation using coalton-toplevel"
  (let ((forms (coalton-script-definitions script)))
    (with-output-to-string (out)
      ;; Always use coalton-toplevel for consistent operator availability
      (format out "  (coalton:coalton-toplevel~%")
      (dolist (form forms)
        (format out "    ~S~%" form))
      (format out "  ))"))))
```

## Results

### Working Operators
- **Arithmetic**: `+`, `-`, `*` 
- **Comparison**: `>`, `<`, `>=`, `<=`
- **Combinations**: `(+ (* 2 3) 4)`

### Operators Requiring Coalton Prelude
- **Equality**: `==` 
- **Conditionals**: `if`
- **Division**: `/` (in some contexts)

## Benefits
1. **Simpler architecture** - No complex preprocessing
2. **More reliable** - Leverages Coalton's native operator handling
3. **Easier to maintain** - No operator mapping tables
4. **Better error messages** - Coalton reports issues directly

## Future Work
- Automatic import of Coalton prelude for full operator support
- Better integration with Coalton's package system
- Support for user-defined operators

## Related Files
- `/src/coalton-translator.lisp` - Simplified translation implementation
- `/build/create-image.lisp` - Updated build process
- Removed: `/src/coalton-preprocessor.lisp` - Complex preprocessor (deleted)