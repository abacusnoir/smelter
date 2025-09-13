# SBCL Runtime Limitations

## CLI Argument Handling

### Known Issue: `--help` and `--version` Flags

**Problem:** The `--help` and `--version` flags show SBCL help instead of Smelter help.

**Root Cause:** SBCL processes certain arguments (`--help`, `--version`, `--core`, etc.) at the runtime level before the user's `toplevel` function is called. This is inherent to how SBCL saves executables.

**Current Behavior:**
```bash
./smt --help      # Shows SBCL help (limitation)
./smt --version   # Shows SBCL version (limitation)
./smt             # Shows Smelter help ✅
./smt eval ...    # Works correctly ✅
./smt run ...     # Works correctly ✅
./smt repl        # Works correctly ✅
```

**Workarounds:**
1. Use `./smt` (no arguments) to see Smelter help
2. Use `./smt repl` then `:version` for version information
3. Use `./smt repl` then `:help` for REPL help

**Technical Details:**
- SBCL's `save-lisp-and-die` with `:save-runtime-options nil` doesn't prevent this
- Runtime options are processed before the `:toplevel` function is called
- This affects all SBCL-based CLI tools, not just Smelter

**Alternative Solutions Attempted:**
1. `:save-runtime-options nil` - Did not resolve the issue
2. Custom argument preprocessing - Would require C-level modifications
3. Wrapper scripts - Would break the "single executable" design goal

**Recommendation:** 
Document this limitation and train users to use `./smt` for help instead of `./smt --help`. The core CLI functionality works perfectly for all actual use cases.

## Impact Assessment

**Severity:** Low - cosmetic issue only
**User Impact:** Minor - users can easily access help and version through alternative means
**Functionality Impact:** None - all core Smelter features work correctly

The CLI entry point fix successfully resolved the main issue where the binary was showing SBCL behavior instead of Smelter behavior. This SBCL runtime argument handling is a separate, minor limitation.