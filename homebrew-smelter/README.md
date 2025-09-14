# Homebrew Smelter

Homebrew tap for Smelter - the type-safe scripting language with 43ms startup.

## Installation

```bash
brew tap abacusnoir/smelter
brew install smelter
```

## What is Smelter?

Smelter brings compile-time type safety to scripting with startup times faster than Ruby.

- ⚡ **43ms startup** (faster than Ruby's 62ms)
- 🛡️ **Full type safety** (catch errors before runtime)  
- 📦 **9.3MB self-contained binary**
- 🎯 **No dependencies required**

## Quick Start

```bash
# Evaluate expressions
smt eval '(+ 1 2)'

# Start interactive REPL
smt repl

# Run script files
smt run script.coal

# Create executable scripts
echo '#!/usr/bin/env smt run
(coalton-toplevel
  (define (main)
    (lisp :unit ()
      (format t "Hello from Smelter!~%")
      (values))))' > hello.coal
chmod +x hello.coal
./hello.coal
```

## Performance Comparison

| Language | Startup Time | Type Safety | Binary Size |
|----------|-------------|-------------|-------------|
| **Smelter** | **43ms** ⚡ | **✅ Yes** | **9.3MB** |
| Python | 29ms | ❌ No | 45MB* |
| Ruby | 62ms | ❌ No | 35MB* |
| Node.js | 35ms | ❌ No | 75MB* |

*Runtime size, not including libraries

## More Information

- [GitHub Repository](https://github.com/abacusnoir/smelter)
- [Documentation](https://github.com/abacusnoir/smelter/docs)
- [Examples](https://github.com/abacusnoir/smelter/examples)