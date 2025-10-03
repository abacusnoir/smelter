# 🔥 Smelter - Type-safe scripting that just works

**The only scripting language with ML-style type inference, zero dependencies, and sub-100ms startup.**

[![Build Status](https://github.com/abacusnoir/smelter/workflows/Build/badge.svg)](https://github.com/abacusnoir/smelter/actions)
[![Release](https://img.shields.io/github/v/release/abacusnoir/smelter)](https://github.com/abacusnoir/smelter/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Smelter (`smt`) is a self-contained CLI runner for [Coalton](https://coalton-lang.github.io/) that brings type-safe scripting to your workflow. Think Babashka for typed Lisp, with ML-style type inference and zero runtime dependencies.

```bash
# Install (coming soon)
brew install smelter

# Run typed scripts
smt run script.coal

# Interactive REPL  
smt repl

# Evaluate expressions
smt eval '(+ 2 3)'
```

## ⚡ Why Smelter?

- **⚡ ~100ms startup** - Faster than Python (competitive with scripting languages)
- **🧠 ML-style type inference** - Catch errors at compile time with Hindley-Milner type system
- **📦 20MB binary** - Self-contained, no dependencies to install
- **🎯 Zero dependencies** - Single binary, works everywhere
- **🚀 Shebang support** - Run `.coal` files directly with `#!/usr/bin/env smt run`
- **💻 Interactive REPL** - Explore and test code interactively
- **🌍 Cross platform** - macOS (Intel/ARM) and Linux support
- **📚 Batteries included** - HTTP, JSON, file I/O, CSV processing built-in

## 🚀 Quick Start

### Installation

**macOS/Linux (Homebrew):**
```bash
# Coming soon to Homebrew
brew install smelter
```

**Manual Installation:**
```bash
# Download latest release
curl -L https://github.com/yourusername/smelter/releases/latest/download/smt-$(uname -s | tr '[:upper:]' '[:lower:]')-$(uname -m).tar.gz | tar xz

# Make executable and add to PATH
chmod +x smt
sudo mv smt /usr/local/bin/
```

### Your First Script

Create `fibonacci.coal`:
```lisp
#!/usr/bin/env smt run

(declare fib (Integer -> Integer))
(define (fib n)
  (if (<= n 1) n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define main
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (fib 10))))
```

Run it:
```bash
chmod +x fibonacci.coal
./fibonacci.coal
# Output: 55
```

## 📖 Usage

### Commands

```bash
smt run <file.coal>      # Run a Coalton script
smt eval <expression>    # Evaluate a Coalton expression  
smt repl                # Start interactive REPL
smt check <file.coal>   # Type-check without running
smt --version           # Show version information
smt --help              # Show help
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
  (smelter.stdlib.io:io-println
    (smelter.stdlib.io:show-int (factorial 10))))
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
- Quicklisp
- Make

### Build Steps

```bash
# Clone the repository
git clone https://github.com/yourusername/smelter.git
cd smelter

# Install dependencies
make deps

# Build the binary
make build

# Run tests
make test

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
**Startup time.** Smelter starts in ~100ms vs 500ms+ for compiled ML languages. For scripting, startup time matters more than peak performance.

### Why Lisp syntax?
**Simplicity and power.** Lisp syntax means a simpler parser, better error messages, and macros (coming soon). The ML-style type system gives you safety without the verbose syntax.

### Is this production ready?
**For scripts, yes. For systems, not yet.** Smelter v0.1 is perfect for DevOps scripts, data processing, and build automation. For long-running services, wait for v1.0.

## 🎯 Use Cases

**DevOps Scripts** - Type-safe deployments with compile-time guarantees
**Data Processing** - CSV reports, ETL pipelines with type safety
**Build Automation** - Replace complex Makefiles with typed workflows
**API Integration** - HTTP clients with JSON parsing built-in

## 🗺️ Roadmap

- [x] **v0.1** - Core CLI, REPL, type system ← *You are here*
- [ ] **v0.2** - Enhanced stdlib (more adapters, better I/O)
- [ ] **v0.3** - Startup optimization (target: <50ms)
- [ ] **v0.4** - Package manager integration
- [ ] **v0.5** - IDE support (LSP server)
- [ ] **v1.0** - Production ready

**Current capabilities:** HTTP, JSON, File I/O, CSV, Process execution

## 🤝 Contributing

We welcome contributions! Please see [CONTRIBUTING.md](docs/contributing.md) for guidelines.

### Quick Contribution Guide

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/amazing-feature`
3. Make your changes and add tests
4. Run the test suite: `make test`
5. Commit your changes: `git commit -m 'Add amazing feature'`
6. Push to the branch: `git push origin feature/amazing-feature`
7. Open a Pull Request

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
| **Startup Time** | **~100ms** | ~30ms | ~60ms | ~35ms |
| **Binary Size** | **20MB** | 45MB* | 35MB* | 75MB* |
| **Type Safety** | **✅ Yes** | ❌ No | ❌ No | ❌ No |
| **Zero Deps** | **✅ Yes** | ❌ No | ❌ No | ❌ No |

*Runtime size, not including libraries

- **Self-contained binary** - No separate runtime or package manager needed
- **ML-style type inference** - Compile-time safety without annotation overhead
- **Competitive startup** - Fast enough for scripting, safer than dynamic languages
- **Optional compression** - Can be reduced to ~10MB with UPX compression

---

**Questions?** Open an [issue](https://github.com/abacusnoir/smelter/issues) or start a [discussion](https://github.com/abacusnoir/smelter/discussions)!

**Want to stay updated?** Watch this repo and follow [@agambrahma](https://twitter.com/agambrahma) for updates.