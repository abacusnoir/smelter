# Contributing to Smelter

We love contributions! Here's how to help make Smelter even better.

## 🚀 Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/yourusername/smelter.git
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
make test                    # Run all tests
make dev                     # Quick development cycle (clean + build + test)
./test/smoke-test.sh         # Run smoke tests
```

### Performance Testing
```bash
./smt-minimal eval '(+ 1 2)'  # Test startup time
make compress                # Test UPX compression
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
- **Performance tests**: Verify startup time claims

Add test cases in the appropriate location:
- `test/smoke-test.sh` - Basic functionality
- `test/regression/` - Regression test suite
- `examples/` - Example scripts that serve as integration tests

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

1. Features are merged to `master`
2. Version tags trigger automated releases
3. Binaries are built and published automatically
4. Homebrew formula is updated

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