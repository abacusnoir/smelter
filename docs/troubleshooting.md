# Troubleshooting Guide

This guide covers common issues and their solutions when working with Smelter.

## Pattern Matching Issues

### Problem: Pattern matching fails in eval mode

**Symptoms:**
- Commands like `./smt eval '(match (Ok 1) ((Ok x) x) ((Err _) 0))'` fail
- Error messages like "end of file on STRING-INPUT-STREAM"
- Pattern matching works in script mode but not eval mode

**Root Cause:**
Missing closing parenthesis in the `translate-for-repl` function caused malformed Lisp code generation.

**Solution:**
This issue was fixed in commit 9244378. If you encounter this:

1. **Verify your version:**
   ```bash
   ./smt --version
   ```
   Should show Smelter 0.1.0 or later.

2. **Test pattern matching:**
   ```bash
   ./smt eval '(match (Ok 42) ((Ok x) x) ((Err _) 0))'
   # Should output: 42
   ```

3. **Run regression tests:**
   ```bash
   make test-eval
   ```

**Prevention:**
The regression test suite (`test/eval-regression.sh`) now includes comprehensive pattern matching tests to prevent this issue from recurring.

## Build Issues

### Problem: Build fails with compilation errors

**Symptoms:**
- `make build` fails with SBCL compilation errors
- Missing dependencies errors
- Package not found errors

**Solutions:**

1. **Clean and rebuild:**
   ```bash
   make clean
   make deps
   make build
   ```

2. **Check dependencies:**
   Ensure you have SBCL installed:
   ```bash
   sbcl --version
   ```

3. **Reset Quicklisp cache:**
   ```bash
   make clean-all
   make deps
   make build
   ```

### Problem: Binary not executable

**Symptoms:**
- `./smt` command not found
- Permission denied errors

**Solution:**
```bash
chmod +x ./smt
```

## Runtime Issues

### Problem: Coalton package not found

**Symptoms:**
- "Coalton package not found in executable" error
- Binary crashes on startup

**Solution:**
The binary wasn't built correctly. Rebuild:
```bash
make clean
make build
```

### Problem: Script execution fails

**Symptoms:**
- `./smt run script.coal` fails
- Syntax errors in working scripts

**Common causes:**

1. **Shebang issues:**
   Ensure scripts start with:
   ```bash
   #!/usr/bin/env smt run
   ```

2. **Package context:**
   Scripts run in `coalton-user` package. Use fully qualified names if needed:
   ```lisp
   (cl:format cl:t "Hello~%")  ; instead of (format t "Hello~%")
   ```

3. **Main function:**
   For scripts with `main` functions, ensure proper definition:
   ```lisp
   (coalton-toplevel
     (declare main (Unit -> Unit))
     (define (main _)
       ;; your code here
       ))
   ```

## Performance Issues

### Problem: Slow startup times

**Expected behavior:**
- First run: ~100ms (loading and verification)
- Subsequent runs: ~50ms

**If significantly slower:**

1. **Check for debug output:**
   Look for excessive verification messages. This was removed in the cleanup.

2. **Verify binary compression:**
   ```bash
   make compress  # Optional UPX compression
   ```

3. **Check system resources:**
   Ensure adequate RAM and CPU available.

## Testing and Verification

### Run comprehensive tests

```bash
# Full test suite
make test

# Eval regression tests only
make test-eval

# Development cycle
make dev
```

### Manual verification

```bash
# Basic functionality
./smt eval '(+ 1 2)'

# Pattern matching
./smt eval '(match (Ok 42) ((Ok x) x) ((Err _) 0))'

# Help and version
./smt --help
./smt --version
```

## Getting Help

### Check logs and output

1. **Build logs:** Look for compilation errors during `make build`
2. **Runtime errors:** Check error messages from `./smt` commands
3. **Test results:** Run `make test` to identify specific failures

### Verify installation

```bash
# Check all components
make test

# Verify specific functionality
./smt eval '(match (Some 10) ((Some x) x) ((None) 0))'
```

### Report issues

When reporting issues, include:

1. **Version information:**
   ```bash
   ./smt --version
   sbcl --version
   uname -a
   ```

2. **Error messages:** Full output including stack traces

3. **Reproduction steps:** Minimal example that demonstrates the issue

4. **Test results:**
   ```bash
   make test-eval 2>&1 | tee test-results.log
   ```

## Common Error Messages

| Error | Cause | Solution |
|-------|-------|----------|
| `end of file on STRING-INPUT-STREAM` | Old version with missing parenthesis bug | Update to latest version |
| `Coalton package not found` | Build issue | `make clean && make build` |
| `coalton-user package not found` | Build issue | `make clean && make build` |
| `command not found: smt` | Binary not in PATH or not executable | `chmod +x ./smt` or `make install` |
| Pattern matching undefined function | Old version | Update and rebuild |

---

**Note:** This troubleshooting guide is for Smelter v0.1.0+. For earlier versions, please update first.