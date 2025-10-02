# Smelter Launch Readiness Plan

## Current Status

### What's Working ✅
1. **I/O stdlib exists and works** - Basic print/println functionality
2. **Clean syntax works end-to-end** - Examples can use the new format
3. **Build system integrated** - Stdlib loads automatically
4. **Hello world works** - The most important demo!

### Critical Issues to Fix 🚨

#### 1. The `show` Problem (HIGHEST PRIORITY)
This is blocking numeric examples - you can't print numbers without `show`!

**Quick Fix Options**:
```lisp
;; Option A: Import show from coalton-prelude
(in-package #:coalton-user)
(coalton:import-function coalton-prelude:show)

;; Option B: Create simple wrapper
(coalton-toplevel
  (declare show-int (Integer -> String))
  (define (show-int n)
    (lisp String (n)
      (cl:format cl:nil "~D" n))))
```

## Immediate Actions (Priority Order)

### 1. Fix `show` Function
- **Goal**: Enable printing numbers in examples
- **Approaches**: Try coalton-prelude import first, then wrapper if needed
- **Success**: Can print "The answer is 42"

### 2. Create Minimal Working Examples
Create in `examples/launch/`:

1. **fibonacci.coal** - Clean, working fibonacci (< 15 lines)
2. **factorial.coal** - Shows recursion and types
3. **temperature.coal** - F to C converter with types
4. **wordcount.coal** - Count words in input (if possible)
5. **fizzbuzz.coal** - Classic example with pattern matching

**Requirements for each**:
- Under 15 lines
- Works with current stdlib
- Demonstrates type safety
- Uses clean syntax

### 3. Create Verification Script
- **File**: `test/verify-launch.sh`
- **Tests**:
  - Basic evaluation
  - Hello world
  - Fibonacci with numbers
  - Binary size check
  - Startup time measurement

### 4. Update README
Add working quick start example:
```markdown
## Quick Start
echo '(define main (smelter.stdlib.io:io-println "Hello!"))' > hello.coal
smt run hello.coal
# Output: Hello!
```

## Week Priorities

### Day 1-2: Core Functionality
- Fix `show` for numeric output
- Add string concatenation (`<>` or `concat`)
- Basic error handling examples

### Day 3: Polish Examples
- 5 working examples that showcase type safety
- Each under 10 lines
- Cover: hello, math, conditionals, functions, types

### Day 4: Performance Check
- Verify <50ms startup
- Ensure <50MB binary
- Profile and optimize if needed

### Day 5: Documentation
- Clean up README
- Create "Getting Started" guide
- Document limitations clearly

## Decision Criteria

### Launch with Current Limitations?

**YES, if you have:**
- ✅ 5 solid working examples
- ✅ Can print strings and numbers
- ✅ <50ms startup time
- ✅ Documented known limitations

**NO, if you have:**
- ❌ Can't print numbers (`show` not working)
- ❌ No file I/O examples working
- ❌ Binary is >60MB

## MVP Feature Set for HN Launch

### Must Have
- ✅ Clean syntax
- ✅ Basic I/O (print/println)
- ⚠️ Number printing (fix `show`) - IN PROGRESS
- ⚠️ 3-5 compelling examples - TODO
- ⚠️ One-line install (brew formula) - FUTURE

### Nice to Have
- File I/O
- HTTP requests
- JSON parsing
- Binary compilation

## Success Criteria

1. ✅ Can print: "The answer is 42"
2. ✅ All 5 launch examples run without errors
3. ✅ Startup time <100ms confirmed
4. ✅ Binary size <60MB
5. ✅ Documentation clear and complete

## Next Steps

Implement in this order:
1. Fix `show` function (CRITICAL - blocks everything else)
2. Create verification script (enables testing)
3. Create 5 launch examples (one at a time, test each)
4. Performance verification
5. Documentation polish

---

**Philosophy**: Better to have 3 working examples than 10 broken ones. Focus on quality and working demos over feature completeness.
