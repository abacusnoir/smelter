---
description: Validate and fix code examples in a blogpost
---

You are tasked with validating and fixing all code examples in a blogpost about Smelter/Coalton.

## Workflow

Follow this systematic process:

### 1. Save and Plan
- Save the blogpost content to `blogpost-{topic}-draft.md` (derive topic from content)
- Create a TODO list with items for each code example found

### 2. Extract and Validate Examples
For each code example (`.smt` or `.coal` file mentioned):
- Extract the code into `examples/blogpost/{filename}`
- Verify it uses **native Coalton methods**:
  - `println` (NOT `smelter.stdlib.io:io-println`)
  - `show` (NOT `smelter.stdlib.io:show-int`)
  - These are auto-imported into `coalton-user` package
- Test with `./smt run examples/blogpost/{filename}`
- Fix any issues found

### 3. Common Issues to Fix

**lisp blocks don't work in clean Coalton:**
- ❌ `(lisp String () (cl:format nil "text ~A" (into x)))`
- ✅ `(<> "text " (show x))`

**Use pure Coalton string concatenation:**
- Operator: `<>`
- Example: `(<> "Hello " (<> name "!"))`

**Simplify nested lets:**
- ❌ Deeply nested `(let ((x ...)) (let ((y ...)) ...))`
- ✅ Single let: `(let ((x ...) (y ...)) ...)`

**Type inference works:**
- `declare` statements are optional (Coalton infers types)
- But keep them for clarity and better errors

### 4. Update and Verify
- Update the blogpost with all corrected code examples
- Save corrected version to `blogpost-{topic}-fixed.md`
- Run `diff -u blogpost-{topic}-draft.md blogpost-{topic}-fixed.md` to show changes
- Create verification summary in `responses/blogpost-{topic}-validation.md`

### 5. Sanity Checks
- Verify all examples in blogpost match the working scripts in `examples/blogpost/`
- Run all examples in sequence to ensure they work
- Check invocations use `./smt run` (NOT `smt eval`)
- Confirm output matches expected results in blogpost

### 6. Final Report
Create a summary documenting:
- What worked as-is
- What needed fixing and why
- Key learnings
- List of files created
- Test results

## Expected Behavior

- Use TodoWrite tool to track progress through all examples
- Test EVERY example before marking as complete
- Prefer pure Coalton over lisp blocks
- Fix issues incrementally (one example at a time)
- Always verify changes with actual test runs

## Output

At the end, provide:
1. List of all examples validated (✅ or ⚠️ status)
2. Summary of changes made
3. Verification that blogpost code matches working scripts
4. Path to verification document in `responses/`
