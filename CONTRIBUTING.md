# Contributing to Smelter

We love contributions! Here's how to help make Smelter even better.

## 🚀 Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/your-username/smelter.git
   cd smelter
   ```
3. **Create a feature branch**:
   ```bash
   git checkout -b feature/amazing-feature
   ```
4. **Set up development environment**:
   ```bash
   make deps     # Install dependencies
   make build    # Build smelter
   make test     # Run tests
   ```

## 🛠️ Development Workflow

### Making Changes
- Keep functions small and focused
- Use descriptive function names
- Follow existing code style and patterns
- Add docstrings to public functions
- Test your changes thoroughly

### Running Tests
```bash
make test                       # Run basic smoke tests
make test-comprehensive         # Run comprehensive test suite (74 tests)
make test-all                   # Run ALL test suites (112+ tests)
make dev                        # Quick development cycle (clean + build + test)
./test/smoke-test.sh            # Run smoke tests
./test/comprehensive-test-suite.sh  # Run comprehensive tests
./test/stress-test.sh           # Run stress tests
./test/cross-platform-test.sh   # Run cross-platform tests
```

### Performance Testing
```bash
./smt eval '(+ 1 2)'         # Test startup time
make compress                 # Test with UPX compression (optional)
```

## 📝 Code Style

- **Lisp**: Follow standard Common Lisp conventions
- **Coalton**: Use type annotations where helpful
- **Documentation**: Write clear, concise comments
- **Naming**: Use descriptive names (e.g., `parse-coalton-expression` not `parse`)

## 🧪 Testing Guidelines

All changes must include appropriate tests:

- **Unit tests**: Test individual functions
- **Integration tests**: Test feature combinations
- **Smoke tests**: Test basic functionality
- **Performance tests**: Verify startup time and memory usage
- **Stress tests**: Verify behavior under load
- **Cross-platform tests**: Ensure compatibility across platforms

Add test cases in the appropriate location:
- `test/smoke-test.sh` - Basic functionality tests
- `test/comprehensive-test-suite.sh` - Comprehensive coverage (74 tests)
- `test/stress-test.sh` - Performance and stress tests (14 tests)
- `test/cross-platform-test.sh` - Platform compatibility (24 tests)
- `test/eval-regression.sh` - Pattern matching validation
- `examples/` - Example scripts that serve as integration tests
- `examples/showcase/` - Production-ready demo scripts

## 🐛 Bug Reports

When reporting bugs, please include:
- Smelter version (`smt --version`)
- Operating system
- Minimal code example that reproduces the issue
- Expected vs actual behavior

## 💡 Feature Requests

We welcome feature suggestions! Please:
- Check existing issues first
- Describe the use case clearly
- Explain why it would benefit other users
- Consider backward compatibility

## 📦 Release Process

1. Features are merged to `master` via Pull Requests
2. CI runs comprehensive test suite (112+ tests)
3. Version tags trigger automated release workflow
4. Binaries are built for all platforms (macOS ARM64/x64, Linux x64)
5. Release artifacts are published to GitHub Releases
6. Install script is tested on all platforms

## 📜 License

By contributing, you agree that your contributions will be licensed under the same MIT License that covers the project.

## 🙏 Recognition

Contributors are recognized in:
- Git commit history
- Release notes for significant contributions
- This README's acknowledgments section

## ❓ Questions?

- Open an issue for bugs or feature requests
- Start a discussion for questions about usage or implementation
- Check existing documentation in the `docs/` directory

Thank you for contributing to Smelter! 🔥