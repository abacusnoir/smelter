# 🔥 Smelter: Type-Safe Scripting in 43ms

**Faster than Ruby. Safer than Python. No compile step like Go.**

[![Build Status](https://github.com/abacusnoir/smelter/workflows/Build/badge.svg)](https://github.com/abacusnoir/smelter/actions)
[![Release](https://img.shields.io/github/v/release/abacusnoir/smelter)](https://github.com/abacusnoir/smelter/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Smelter (`smt`) is a self-contained CLI runner for [Coalton](https://coalton-lang.github.io/) that brings type-safe scripting to your workflow. Think Babashka for typed Lisp.

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

## ⚡ Features

- **⚡ 43ms startup** - Faster than Ruby (62ms), competitive with Python (29ms)
- **🛡️ Type safety** - Catch errors at compile time, not runtime
- **📦 9.3MB binary** - Smaller than Node.js runtime, completely self-contained
- **🎯 Zero dependencies** - Single binary, works everywhere
- **🚀 Shebang support** - Run `.coal` files directly with `#!/usr/bin/env smt run`
- **💻 Interactive REPL** - Explore and test code interactively
- **🌍 Cross platform** - macOS (Intel/ARM) and Linux support

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

Create `hello.coal`:
```lisp
#!/usr/bin/env smt run

(coalton-toplevel
  (declare greet (String -> String))
  (define (greet name)
    (concatenate "Hello, " name "!")))

(defun main ()
  (format t "~A~%" (coalton:coalton (greet "World"))))
```

Run it:
```bash
chmod +x hello.coal
./hello.coal
# Output: Hello, World!
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

**Fibonacci sequence:**
```bash
smt eval '(let ((fib (fn (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))'
```

**Interactive REPL:**
```bash
$ smt repl
Smelter 0.1.0 - Coalton REPL
Type expressions or :help for commands

smt> (+ 2 3)
5
smt> :help
REPL Commands:
  :help    - Show this help
  :quit    - Exit REPL
  :version - Show version
smt> :quit
Goodbye!
```

**Script with types:**
```lisp
(coalton-toplevel
  (declare factorial (Integer -> Integer))
  (define (factorial n)
    (if (== n 0) 1
        (* n (factorial (- n 1))))))

(defun main ()
  (cl:loop for i from 1 to 10
           do (format t "~A! = ~A~%" i (coalton:coalton (factorial i)))))
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

## 🎯 Use Cases

### DevOps Scripts
```lisp
;; Type-safe deployment script
(coalton-toplevel
  (declare deploy (String -> String -> Boolean))
  (define (deploy environment version)
    ;; Type-checked deployment logic
    ))
```

### Data Processing
```lisp
;; ETL pipeline with guarantees
(coalton-toplevel
  (declare process-csv (String -> List Record))
  (define (process-csv filepath)
    ;; Type-safe CSV processing
    ))
```

### Build Automation  
```lisp
;; Replacement for complex Makefiles
(coalton-toplevel
  (declare build-project (List String -> BuildResult))
  (define (build-project targets)
    ;; Type-checked build steps
    ))
```

## 🗺️ Roadmap

- [ ] **v0.1** - Basic CLI functionality ← *You are here*
- [ ] **v0.2** - Standard library (HTTP, JSON, filesystem)
- [ ] **v0.3** - Binary compilation (`smt build`)
- [ ] **v0.4** - Package manager integration
- [ ] **v0.5** - IDE support (LSP server)
- [ ] **v1.0** - Production ready

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
| **Startup Time** | **43ms** ⚡ | 29ms | 62ms | 35ms |
| **Binary Size** | **9.3MB** | 45MB* | 35MB* | 75MB* |
| **Type Safety** | **✅ Yes** | ❌ No | ❌ No | ❌ No |
| **Zero Deps** | **✅ Yes** | ❌ No | ❌ No | ❌ No |

*Runtime size, not including libraries

- **51.6% faster** than previous version (88ms → 43ms)
- **48% smaller** binary (18MB → 9.3MB)
- **Faster than Ruby**, competitive with Python
- **Complete type safety** unlike dynamic languages

---

**Questions?** Open an [issue](https://github.com/abacusnoir/smelter/issues) or start a [discussion](https://github.com/abacusnoir/smelter/discussions)!

**Want to stay updated?** Watch this repo and follow [@agambrahma](https://twitter.com/agambrahma) for updates.