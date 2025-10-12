# Contributing to Smelter

Thanks for your interest in improving Smelter!

## Getting Started

```bash
git clone https://github.com/your-username/smelter.git
cd smelter
make deps     # Install dependencies
make build    # Build smelter
make test     # Run tests
```

## Making Changes

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/your-feature`
3. Make your changes and add tests
4. Run the test suite: `make test-all` (112+ tests)
5. Commit and push
6. Open a Pull Request

## Testing

```bash
make test                       # Basic smoke tests
make test-comprehensive         # 74 tests
make test-all                   # All 112+ tests
```

## Code Style

- Follow existing patterns in the codebase
- Use type annotations where helpful
- Add tests for new functionality
- Keep functions focused and well-named

## Bug Reports

Include:
- Smelter version (`smt --version`)
- Operating system
- Minimal reproduction example
- Expected vs actual behavior

## License

By contributing, you agree your contributions will be licensed under the MIT License.
