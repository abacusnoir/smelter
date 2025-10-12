# 🔥 Smelter - Type-safe scripting that just works

**The only scripting language with ML-style type inference, zero dependencies, and ~43ms startup.**

[![Build Status](https://github.com/abacusnoir/smelter/workflows/Build/badge.svg)](https://github.com/abacusnoir/smelter/actions)
[![Release](https://img.shields.io/github/v/release/abacusnoir/smelter)](https://github.com/abacusnoir/smelter/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Smelter (`smt`) is a self-contained CLI runner for [Coalton](https://coalton-lang.github.io/) that brings type-safe scripting to your workflow. Think Babashka for typed Lisp, with ML-style type inference and zero runtime dependencies.

```bash
# Quick install
curl -fsSL https://smelter-landing.pages.dev/install.sh | bash

# Run typed scripts
smt run script.coal

# Interactive REPL
smt repl

# Evaluate expressions
smt eval '(+ 2 3)'
```

## ⚡ Why Smelter?

- **⚡ ~43ms startup** - Faster than Ruby (62ms), competitive with Python (29ms) and Node.js (35ms)
- **🧠 ML-style type inference** - Catch errors at compile time with Hindley-Milner type system
- **📦 9.3MB binary** - Self-contained, optimized, no dependencies to install
- **🎯 Zero dependencies** - Single binary, works everywhere
- **🚀 Shebang support** - Run `.coal` files directly with `#!/usr/bin/env smt run`
- **💻 Interactive REPL** - Explore and test code interactively
- **🌍 Cross platform** - macOS (Intel/ARM) and Linux support
- **📚 Batteries included** - HTTP, JSON, file I/O, process execution built-in
- **✅ 112+ tests** - Comprehensive test coverage ensures reliability

## 🚀 Quick Start

### Installation

**Quick Install (Recommended):**
```bash
curl -fsSL https://smelter-landing.pages.dev/install.sh | bash
```

**Manual Installation:**
```bash
# Download latest release for your platform
# macOS (Apple Silicon):
curl -L https://github.com/abacusnoir/smelter/releases/latest/download/smelter-macos-arm64.tar.gz | tar xz

# macOS (Intel):
curl -L https://github.com/abacusnoir/smelter/releases/latest/download/smelter-macos-x64.tar.gz | tar xz

# Linux (x86_64):
curl -L https://github.com/abacusnoir/smelter/releases/latest/download/smelter-linux-x64.tar.gz | tar xz

# Install to system
sudo mv smelter-* /usr/local/bin/smt
chmod +x /usr/local/bin/smt
```

### Your First Script

Create `hello.coal`:
```lisp
#!/usr/bin/env smt run

(declare greet (String -> String))
(define (greet name)
  (concatenate "Hello, " name "!"))

(define main
  (println (greet "World")))
```

Run it:
```bash
chmod +x hello.coal
./hello.coal
# Output: Hello, World!
```

### Showcase Examples

Check out `examples/showcase/` for production-ready demos:
- **config-validator.coal** - Type-safe configuration validation
- **error-handling.coal** - Result types for guaranteed error handling
- **type-safety.coal** - Compile-time type checking examples
- **build-pipeline.coal** - Type-safe build orchestration
- **data-transform.coal** - Type-safe data processing pipelines

```bash
./smt run examples/showcase/config-validator.coal
```

## 📖 Usage

### Commands

```bash
smt run <file.coal>      # Run a Coalton script
smt eval <expression>    # Evaluate a Coalton expression
smt repl                 # Start interactive REPL
smt --version            # Show version information
smt --help               # Show help
```

### Examples

**Quick arithmetic:**
```bash
smt eval '(+ 2 3)'
# Output: 5
```

**Type-safe factorial:**
```lisp
#!/usr/bin/env smt run

(declare factorial (Integer -> Integer))
(define (factorial n)
  (if (== n 0) 1
      (* n (factorial (- n 1)))))

(define main
  (println (show-int (factorial 10))))
```

**Interactive REPL:**
```bash
$ smt repl
Smelter 0.1.0 - Coalton REPL

smt> (+ 2 3)
5
smt> :quit
Goodbye!
```

## 🏗️ Building from Source

### Prerequisites

- SBCL (Steel Bank Common Lisp)
- Make
- curl (for installing dependencies)

### Build Steps

```bash
# Clone the repository
git clone https://github.com/abacusnoir/smelter.git
cd smelter

# Install dependencies (automatically installs Quicklisp and Coalton)
make deps

# Build the binary
make build

# Run comprehensive tests (112+ test cases)
make test-all

# Install locally
make install
```

### Development

```bash
# Quick development cycle
make dev

# Build with compression
make compress

# Create release archive
make release
```

## 🔧 How It Works

Smelter embeds SBCL and Coalton into a single executable using `sb-ext:save-lisp-and-die`. The build process:

1. **Load Dependencies** - Quicklisp loads Coalton and dependencies
2. **Create Core** - SBCL saves a core image with Coalton pre-compiled  
3. **Build Binary** - The CLI layer wraps the core into an executable
4. **Optimize** - Tree-shaking and compression reduce size

This approach gives you:
- ✅ **Fast startup** - No compilation at runtime
- ✅ **Zero deps** - Everything embedded in one file  
- ✅ **Type safety** - Full Coalton type system available
- ✅ **Familiar UX** - Works like any other CLI tool

## 🤔 FAQ

### Why not just use Haskell/OCaml?
**Startup time.** Smelter starts in ~43ms vs 500ms+ for compiled ML languages. For scripting, startup time matters more than peak performance.

### Why Lisp syntax?
**Simplicity and power.** Lisp syntax means a simpler parser, better error messages, and powerful macros. The ML-style type system gives you safety without verbose syntax.

### Is this production ready?
**For scripts, yes.** Smelter is perfect for DevOps scripts, data processing, and build automation with 112+ tests ensuring reliability. The clean syntax and comprehensive test coverage make it ready for production scripting.

## 🎯 Use Cases

**DevOps Scripts** - Type-safe deployments with compile-time guarantees
**Data Processing** - CSV reports, ETL pipelines with type safety
**Build Automation** - Replace complex Makefiles with typed workflows
**API Integration** - HTTP clients with JSON parsing built-in

## 🤝 Contributing

Contributions welcome! Fork the repo, make your changes with tests, run `make test-all`, and open a pull request. See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [Coalton](https://coalton-lang.github.io/) - The statically-typed Lisp that makes this possible
- [SBCL](http://www.sbcl.org/) - Steel Bank Common Lisp implementation
- [Babashka](https://babashka.org/) - Inspiration for zero-friction scripting
- [Quicklisp](https://www.quicklisp.org/) - Common Lisp package manager

## 📈 Performance

**v0.1.0 Performance Metrics:**

| Metric | Smelter | Python | Ruby | Node.js |
|--------|---------|---------|------|---------|
| **Startup Time** | **~43ms** | ~29ms | ~62ms | ~35ms |
| **Binary Size** | **9.3MB** | 45MB* | 35MB* | 75MB* |
| **Type Safety** | **✅ Yes** | ❌ No | ❌ No | ❌ No |
| **Zero Deps** | **✅ Yes** | ❌ No | ❌ No | ❌ No |
| **Test Coverage** | **112+ tests** | Variable | Variable | Variable |

*Runtime size, not including libraries

- **Self-contained binary** - No separate runtime or package manager needed
- **ML-style type inference** - Compile-time safety without annotation overhead
- **Competitive startup** - 51.6% faster than initial release (88ms → 43ms)
- **Optimized size** - 9.3MB optimized binary (50% smaller than v0.1.0-alpha)

---

**Questions?** Open an [issue](https://github.com/abacusnoir/smelter/issues) or start a [discussion](https://github.com/abacusnoir/smelter/discussions).