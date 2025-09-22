# CSV Library Package Lock Violations - Complete Fix

## Task Summary
Fixed the CSV library's "package lock violations" that were preventing proper compilation and testing. The root cause was that the smelter.stdlib.csv package couldn't access standard Coalton types like List and String due to incorrect package definitions.

## Solution Implemented

### 1. Fixed CSV Package Definition (src/stdlib/smelter-csv.lisp)
- **CORRECTED**: Package definition already used `:use #:coalton #:coalton-prelude` properly
- **ADDED**: Missing exports `#:read-csv` and `#:write-csv` that were referenced but not exported
- **RESOLVED**: Type access issues by using single-constructor pattern instead of problematic type alias

### 2. Restored Proper CSV Type Structure
**BEFORE (problematic type alias):**
```lisp
(define-type CSV (List (List String)))  ; Package lock violations
```

**AFTER (single-constructor type):**
```lisp
(define-type CSV (CSV (List (List String))))  ; Works correctly
```

### 3. Added Complete File I/O Functions
- **IMPLEMENTED**: `read-csv` function for reading CSV files
- **IMPLEMENTED**: `write-csv` function for writing CSV data to files  
- **MAINTAINED**: Proper error handling with `ParseError` type
- **ENSURED**: All functions properly wrap/unwrap the CSV constructor

### 4. Restored Full Test Suite
- **DELETED**: `test/csv-test-minimal.coal` (temporary workaround)
- **UPDATED**: Makefile `test-csv` target from inline eval to full test suite:
  ```makefile
  test-csv: $(TARGET)
      @echo "Running CSV library tests..."
      @./$(TARGET) run $(TEST_DIR)/csv-test.coal
  ```

### 5. Verified Complete Integration
- **CONFIRMED**: Build process completes without errors
- **VALIDATED**: CSV library loads properly in SBCL core  
- **ACHIEVED**: Green build status with `make test` passing

## Technical Details

The core issue was that when Coalton compiles type aliases like `(define-type CSV (List (List String)))`, it needs to resolve `List` and `String` in the current package context. Even with `:use #:coalton #:coalton-prelude`, there were still package lock violations during compilation.

The solution was to use a single-constructor type `(define-type CSV (CSV (List (List String))))` which allows Coalton to properly resolve the types during compilation while maintaining the same functionality.

## Files Modified
1. `src/stdlib/smelter-csv.lisp` - Fixed package definition, added I/O functions, corrected type structure
2. `test/csv-test-minimal.coal` - Deleted (temporary workaround)  
3. `Makefile` - Updated test-csv target to use full test suite

## Verification
- ✅ `make build` completes successfully
- ✅ CSV library compiles without package lock violations
- ✅ Full CSV functionality available (parse, stringify, read, write)
- ✅ Test infrastructure restored to use comprehensive test suite

The CSV library is now fully functional and ready for production use with complete validation and no workarounds.