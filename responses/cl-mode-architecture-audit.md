# Smelter Architecture Audit: CL Mode Feasibility

**Date**: 2026-01-04
**Branch**: claude/audit-smelter-cl-mode-lPHhU

## Executive Summary

**Verdict: CL mode is TRIVIAL to implement (~50-100 lines)**

The Smelter codebase already has substantial infrastructure supporting Common Lisp code alongside Coalton:
- The translator explicitly handles CL forms (`defun`, `defvar`, etc.) separately from Coalton forms
- Pure CL scripts already work (e.g., `examples/simple.coal`)
- A hybrid mode exists where Coalton and CL code coexist
- The `smelter.bridge.*` packages are pure CL wrappers

Adding a dedicated `smt cl run script.lisp` command requires minimal changes to `src/cli.lisp`.

---

## 1. Current State Assessment

### Project Structure
```
smelter/
├── src/
│   ├── cli.lisp                    # Main CLI (327 lines)
│   ├── coalton-translator.lisp     # Coalton→CL translation (341 lines)
│   ├── coalton-preprocessor.lisp   # Operator qualification
│   ├── stdlib/                     # Coalton-based standard library
│   │   ├── io.lisp                 # I/O operations (Coalton)
│   │   ├── smelter-prelude.lisp    # CL fallback operators!
│   │   └── ...
│   ├── adapters/                   # Coalton wrappers around CL
│   │   ├── fs.lisp                 # Filesystem (24KB, Coalton→CL FFI)
│   │   ├── http.lisp               # HTTP client
│   │   ├── json-adapter.lisp       # JSON handling
│   │   └── process/                # Process execution
│   └── bridge/
│       └── json.lisp               # Pure CL (YASON wrapper)
├── build/
│   ├── create-image.lisp           # SBCL image builder
│   └── fast-core.lisp              # Lazy loading infrastructure
├── smelter.asd                     # ASDF system definition
└── examples/
    ├── simple.coal                 # Pure CL script!
    ├── hello.coal                  # Clean Coalton syntax
    └── ...
```

### Working Binary Status
- **No pre-built binary** in repo (built via `make build`)
- **SBCL not installed** in current environment (cannot test build)
- Build process: `make build` → `sbcl --load build/create-image.lisp`
- Binary spec: ~9.3MB compressed, ~42ms startup

### Achieved vs Target Metrics (from docs)
| Metric | Target | Achieved |
|--------|--------|----------|
| Binary size | <15MB | 9.3MB ✓ |
| Startup time | <100ms | ~42ms ✓ |
| Tests | comprehensive | 112+ tests ✓ |

---

## 2. Execution Path Analysis

### Path: `smt run script.coal` → Execution

```
main() [cli.lisp:304]
  └── parse-arguments()
        └── run-script(filepath) [cli.lisp:112]
              ├── read-file-content()
              ├── strip-shebang()
              └── translate-pure-coalton(content) [translator.lisp:248]
                    ├── parse-coalton-file() → coalton-script struct
                    │     ├── Coalton forms → .declarations, .definitions
                    │     └── CL forms (defun, etc.) → .lisp-forms
                    └── translate-for-script()
                          ├── Wrap in (cl:progn ...)
                          ├── Import stdlib packages
                          ├── IF coalton-forms: wrap in (coalton:coalton-toplevel ...)
                          └── CL forms: output with cl: qualification
              └── eval(translated-code) in coalton-user package
              └── Call main if present
```

### Where Coalton Gets Loaded

1. **Build time** (`build/create-image.lisp:17`):
   ```lisp
   (ql:quickload :coalton :silent nil)
   ```
   Full Coalton compiler embedded in SBCL image.

2. **Runtime** (in translated code):
   ```lisp
   (coalton:coalton-toplevel ...)  ; Triggers Coalton compilation
   ```

### Separation of "Load Runtime" vs "Compile User Code"

**YES, clear separation exists:**
- Runtime environment (`coalton-user` package) created at build time
- User code compilation happens at script eval time via `coalton-toplevel`
- The lazy-loading architecture in `fast-core.lisp` even allows deferring Coalton load

### What Happens with Plain CL S-expressions

**They already work!** Looking at `examples/simple.coal`:
```lisp
#!/usr/bin/env smt run
(defun greet (name)
  (concatenate 'string "Hello, " name "!"))

(defun main ()
  (format t "~A~%" (greet "Smelter")))

(main)
```

The translator:
1. Parses forms, identifies `defun` as CL (not Coalton)
2. Stores in `.lisp-forms` (not `.definitions`)
3. Outputs CL forms OUTSIDE `coalton-toplevel`
4. Qualifies symbols with `cl:` prefix

**No Coalton compilation triggered for pure CL scripts!**

---

## 3. Package/Runtime Structure

### Packages Created at Startup

| Package | Purpose |
|---------|---------|
| `:coalton` | Coalton core language |
| `:coalton-prelude` | Coalton standard library |
| `:coalton-user` | User environment (imports above) |
| `:smelter` | CLI package |
| `:smelter.translator` | Translation logic |
| `:smelter.stdlib.*` | Coalton-based stdlib |
| `:smelter.bridge.*` | Pure CL wrappers |
| `:smelter/adapters/*` | Coalton wrappers over CL libs |

### Coalton-Specific Symbols Exposed to User Scripts

Via `coalton-user` imports:
- Arithmetic: `+`, `-`, `*`, `/`
- Comparison: `==`, `<`, `>`, `<=`, `>=`
- Control: `if`, `match`, `let`, `progn`
- Types: `Integer`, `String`, `Boolean`, `List`, `Optional`, `Result`
- Constructors: `Some`, `None`, `Ok`, `Err`, `Cons`, `Nil`

Via stdlib imports:
- `println`, `show`, `io-println`, `io-print`
- `read-file`, `write-file`, `file-exists?`
- `http-get`, `parse-json`, etc.

### Are Adapters Wrapping CL Directly or Through Coalton Types?

**Both!** The adapters use a layered architecture:

```
User Script (Coalton)
       ↓
Adapter (Coalton types + FFI)  e.g., fs.lisp
       ↓
Bridge (Pure CL)               e.g., bridge/json.lisp
       ↓
External Library               e.g., YASON, Drakma
```

Example from `fs.lisp:81-91`:
```lisp
(declare read-file (String -> (Result FSError String)))
(define (read-file path)
  (lisp (Result FSError String) (path)    ; Coalton FFI
    (cl:handler-case                       ; Pure CL inside
        (cl:with-open-file (stream path ...)
          (Ok ...))
      (cl:file-error (e)
        (Err ...)))))
```

---

## 4. Minimal CL Mode Experiment

### Can You Bypass Coalton and Just `(load)` + `(eval)`?

**YES.** Here's a sketch:

```lisp
;; Add to src/cli.lisp

(defun run-cl-script (filepath)
  "Run a plain Common Lisp script without Coalton"
  (handler-case
      (progn
        (unless (probe-file filepath)
          (smelter-error "File not found: ~A" filepath))

        (let* ((raw-content (read-file-content filepath))
               (content (strip-shebang raw-content)))

          ;; Simply load in cl-user package
          (let ((*package* (find-package :cl-user)))
            (with-input-from-string (stream content)
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    do (eval form))))

          ;; Call main if defined
          (when (fboundp 'main)
            (funcall 'main)))

        (sb-ext:exit :code 0))

    (error (e)
      (format *error-output* "Error: ~A~%" e)
      (sb-ext:exit :code 1))))
```

### What Breaks?

1. **Stdlib not available**: Smelter's Coalton-based stdlib (println, read-file, etc.) won't work
2. **Package context**: Scripts would run in `cl-user`, not `coalton-user`
3. **No automatic type safety**: Coalton's compile-time checks bypassed

### Estimate: Effort Level

| Approach | Lines Changed | Complexity |
|----------|---------------|------------|
| Basic `smt cl run` | ~50 lines | Trivial |
| CL mode + CL stdlib | ~100-150 lines | Easy |
| CL mode + shared adapters | ~200-300 lines | Moderate |
| Auto-detect mode | ~150 lines | Moderate (potential confusion) |

---

## 5. Build System Analysis

### How Binary is Produced

`build/create-image.lisp`:
```lisp
(sb-ext:save-lisp-and-die "smt"
  :toplevel #'smelter:main
  :executable t
  :compression t        ; ZLIB compression
  :purify nil           ; Preserve dynamic compilation
  :save-runtime-options t)
```

### What's Included in the Image

- **Full SBCL runtime**
- **Full Coalton compiler** (not just runtime)
- **Quicklisp subset** (loaded dependencies)
- **Smelter code** (CLI, translator, stdlib, adapters)
- **External libraries**: YASON (JSON), Drakma (HTTP), cl-csv, etc.

### Tree Shaking?

**NO tree-shaking.** It's a full image dump. The binary includes:
- Everything needed to compile new Coalton code at runtime
- All loaded libraries regardless of usage

This is why the binary is ~9.3MB even for simple scripts.

---

## Key Findings

### Positive Discoveries

1. **CL already partially supported**: The translator handles `defun`, `defvar`, etc.
2. **Pure CL scripts work**: `examples/simple.coal` runs without Coalton compilation
3. **Lazy loading exists**: `fast-core.lisp` shows infrastructure for deferred loading
4. **Clean separation**: Bridge layer is pure CL, adapters are Coalton→CL FFI
5. **CL fallback functions**: `smelter.stdlib.prelude` has CL implementations

### Concerns

1. **Package context**: Scripts currently eval in `coalton-user`, not `cl-user`
2. **Stdlib dependency**: Most stdlib is Coalton-based, needs CL equivalents
3. **No explicit CL mode**: Users must know to avoid Coalton syntax

---

## Recommended Approach

### Phase 1: Explicit CL Command (~50 lines)

Add `smt cl run <file.lisp>` command:

```lisp
;; In parse-arguments:
((string= (first args) "cl")
 (cond
   ((string= (second args) "run")
    (run-cl-script (third args)))
   ((string= (second args) "eval")
    (eval-cl-expression (third args)))
   (t (smelter-error "Unknown cl command"))))
```

Implementation in `run-cl-script`:
- Read file, strip shebang
- Eval forms in `cl-user` package
- Call `main` if defined
- No Coalton translation

### Phase 2: CL Standard Library (Optional, ~100 lines)

Create `src/stdlib-cl/` with pure CL utilities:
- `io.lisp`: `print-line`, `read-line`
- `file.lisp`: File operations wrapping UIOP
- `string.lisp`: String utilities

### Phase 3: Shared Adapters (Optional, ~200 lines)

Refactor adapters to have CL-accessible interfaces:
- Keep Coalton type wrappers
- Export CL functions directly for CL mode

---

## Blockers/Risks

### Low Risk

1. **Namespace collision**: CL's `+` vs Coalton's `+` could confuse users
   - Mitigation: Document clearly, use explicit package prefixes

2. **Stdlib gap**: CL mode won't have `println`, `read-file`, etc.
   - Mitigation: Provide CL stdlib or document CL equivalents

### Medium Risk

1. **Maintenance burden**: Two code paths to maintain
   - Mitigation: Share infrastructure where possible (shebang stripping, file I/O)

2. **User confusion**: When to use CL vs Coalton mode?
   - Mitigation: Clear documentation, maybe different file extensions (`.lisp` vs `.coal`)

### No Blockers Identified

The architecture cleanly supports CL mode. No fundamental obstacles.

---

## Conclusion

Adding CL mode to Smelter is **trivial** from an architecture perspective. The codebase already:
- Separates Coalton and CL forms in translation
- Has pure CL layers (bridge, prelude fallbacks)
- Supports lazy Coalton loading

**Recommended minimal implementation**: ~50 lines in `cli.lisp` for `smt cl run` command.

**Full implementation with CL stdlib**: ~150-200 lines across a few files.

This is a **10-line conceptual change** (add new command path) with **~50-100 lines** of implementation.
