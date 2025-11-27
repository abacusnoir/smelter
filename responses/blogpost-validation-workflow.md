# Blogpost Validation Workflow

## Quick Start

To validate and fix code examples in a blogpost:

```
/validate-blogpost

[paste blogpost content here]
```

## What It Does

The workflow automatically:

1. **Saves** the blogpost to `blogpost-{topic}-draft.md`
2. **Creates TODOs** for each code example
3. **Extracts** each example to `examples/blogpost/{filename}`
4. **Tests** each example with `./smt run`
5. **Fixes** common issues:
   - Replaces `lisp` blocks with `<>` string concatenation
   - Uses native `println` and `show` functions
   - Simplifies nested `let` expressions
6. **Updates** blogpost with corrected examples
7. **Verifies** with diff and sanity checks
8. **Documents** all changes in `responses/`

## Common Fixes Applied

### ❌ Don't Use lisp Blocks
```lisp
(lisp String () (cl:format nil "text ~A" (into x)))
```

### ✅ Use Pure Coalton
```lisp
(<> "text " (show x))
```

### ❌ Don't Use Qualified Names
```lisp
(smelter.stdlib.io:io-println "hello")
(smelter.stdlib.io:show-int 42)
```

### ✅ Use Native Methods
```lisp
(println "hello")
(show 42)
```

## Why Native Methods Work

The `coalton-user` package auto-imports from `smelter.stdlib.clean`:
- `println` wraps `smelter.stdlib.io:io-println`
- `show` wraps `smelter.stdlib.io:show-int`

These are available without qualification in all scripts.

## Files Created

After running `/validate-blogpost`, you'll have:

```
blogpost-{topic}-draft.md          # Original content
blogpost-{topic}-fixed.md          # Corrected version
examples/blogpost/*.smt            # Working example scripts
responses/blogpost-{topic}-validation.md  # Summary
```

## Example Session

```
User: /validate-blogpost

[paste blogpost content]

Claude:
✅ double.smt - works as-is
⚠️ pure-computation.smt - fixed lisp block → <> concatenation
⚠️ composition.smt - fixed lisp blocks, simplified lets
✅ exercises.smt - works as-is

All examples tested and working!
See: responses/blogpost-{topic}-validation.md
```

## Manual Verification

Always sanity check with:

```bash
# View changes
diff -u blogpost-{topic}-draft.md blogpost-{topic}-fixed.md

# Test all examples
for f in examples/blogpost/*.smt; do ./smt run "$f"; done
```

## Integration with CLAUDE.md

This workflow follows the patterns in CLAUDE.md:
- Uses clean Coalton syntax (no `coalton-toplevel`)
- Tests before committing changes
- Documents achievements in `docs/` and responses in `responses/`
- Maintains example quality for HN launch readiness
