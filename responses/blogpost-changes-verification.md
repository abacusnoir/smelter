# Blogpost Changes Verification

## Note
The draft and fixed versions are now identical because I edited the draft in place, then copied it to the fixed version. Here's a verification of the key changes made:

## Change 1: pure-computation.smt

### Original (from user's input):
```lisp
;; Helper to show boolean
(declare show-bool (Boolean -> String))
(define (show-bool b)
  (if b "True" "False"))  ← Was "True"/"False"

;; Main handles ALL the I/O in one place
(define main
  (progn
    (println "Processing numbers...")
    (let ((result (process 21)))
      (progn
        (println (show result))
        (println (lisp String ()           ← Used lisp block
          (cl:format nil "Is ~A even? ~A"  ← cl:format doesn't work
            (into result)                   ← into needs 2 args
            (if (is-even result) "yes" "no"))))))))  ← if in CL context
```

### Fixed (in blogpost-functions-fixed.md):
```lisp
;; Helper to show boolean
(declare show-bool (Boolean -> String))
(define (show-bool b)
  (if b "yes" "no"))  ✓ Now returns "yes"/"no"

;; Main handles ALL the I/O in one place
(define main
  (progn
    (println "Processing numbers...")
    (let ((result (process 21)))
      (progn
        (println (show result))
        (println (<> "Is " (<> (show result) (<> " even? " (show-bool (is-even result))))))))))
        ✓ Pure Coalton string concatenation with <>
```

**Why this fix?**
- `lisp` blocks create a CL context where Coalton variables aren't accessible
- `into` requires two arguments (value and target type)
- `if` inside lisp block is CL's if, not Coalton's
- Pure Coalton `<>` concatenation is cleaner and works correctly

---

## Change 2: composition.smt

### Original (from user's input):
```lisp
;; Break down the steps
(declare show-steps (Integer -> Unit))
(define (show-steps x)
  (progn
    (println (lisp String () (cl:format nil "Start with: ~A" (into x))))  ← lisp block
    (let ((step1 (add-one x)))  ← Nested lets
      (progn
        (println (lisp String () (cl:format nil "After add-one: ~A" (into step1))))
        (let ((step2 (double step1)))  ← More nesting
          (progn
            (println (lisp String () (cl:format nil "After double: ~A" (into step2))))
            (let ((step3 (square step2)))  ← Even more nesting
              (println (lisp String () (cl:format nil "After square: ~A" (into step3)))))))))))
```

### Fixed (in blogpost-functions-fixed.md):
```lisp
;; Break down the steps
(declare show-steps (Integer -> Unit))
(define (show-steps x)
  (let ((step1 (add-one x))  ✓ Single let with all bindings
        (step2 (double (add-one x)))
        (step3 (square (double (add-one x)))))
    (progn
      (println (<> "Start with: " (show x)))  ✓ Pure Coalton <>
      (println (<> "After add-one: " (show step1)))
      (println (<> "After double: " (show step2)))
      (println (<> "After square: " (show step3))))))
```

**Why this fix?**
- Same `lisp` block issues as above
- Deeply nested `let` expressions are harder to read
- Single `let` with multiple bindings is more idiomatic Coalton
- Pure Coalton string concatenation works correctly

---

## Changes 3 & 4: double.smt and exercises.smt

### Status: ✅ NO CHANGES NEEDED

Both examples already used native Coalton methods correctly:
- `println` instead of `smelter.stdlib.io:io-println`
- `show` instead of `smelter.stdlib.io:show-int`

---

## Verification: Code Matches Working Scripts

All blogpost code snippets now match the tested, working scripts in `examples/blogpost/`:

```bash
$ ./smt run examples/blogpost/pure-computation.smt
Processing numbers...
42
Is 42 even? yes  ✓

$ ./smt run examples/blogpost/composition.smt
=== Direct transformation ===
144

=== Step by step ===
Start with: 5
After add-one: 6
After double: 12
After square: 144  ✓
```

---

## Summary of Changes

1. **Replaced `lisp` blocks** with pure Coalton `<>` string concatenation (2 examples)
2. **Fixed show-bool return values** from "True"/"False" to "yes"/"no" (1 example)
3. **Simplified nested lets** to single let with multiple bindings (1 example)
4. **Verified invocations** already use `smt run` (no changes needed)
5. **Verified native methods** - `println` and `show` already used correctly (2 examples needed no changes)

**Total examples**: 4
**Examples changed**: 2 (pure-computation.smt, composition.smt)
**Examples unchanged**: 2 (double.smt, exercises.smt)
**All examples tested**: ✅ All working correctly
